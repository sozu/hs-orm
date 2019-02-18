{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.ORM.Dialect.PostgreSQL (
    WithDB'
    , PostgreSQL(..)
    , PGTableLockMode(..)
    , PGRecordLockMode(..)
) where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.Convertible
import Data.IORef
import Data.Time
import Data.Resource
import Language.Haskell.TH
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Maybe (maybe, catMaybes)
import Database.ORM.HDBC hiding (DBSettings(..))
import Database.ORM.Query
import Database.ORM.Functionality
import Database.ORM.TH
import qualified Database.ORM.HDBC as D (DBSettings(..))

type WithDB' = WithDB PostgreSQL

data PostgreSQL = PostgreSQL {
    url :: DBURL
    , maxConnections :: Int
}

instance D.DBSettings PostgreSQL where
    type ConnectionType PostgreSQL = Connection
    type DialectType PostgreSQL = Dialect'
    url = url
    maxConnections = maxConnections
    open s = connectPostgreSQL (url s)
    dialect _ = Dialect'

instance TypeMappable PostgreSQL where
    -- TODO not defined completely
    mapColumnType _ "boolean" _ = [t| Bool |]
    mapColumnType _ "smallint" _ = [t| Integer |]
    mapColumnType _ "integer" _ = [t| Integer |]
    mapColumnType _ "bigint" _ = [t| Integer |]
    mapColumnType _ "decimal" _ = [t| Integer |]
    mapColumnType _ "numeric" _ = [t| Integer |]
    mapColumnType _ "real" _ = [t| Float |]
    mapColumnType _ "double precision" _ = [t| Double |]
    mapColumnType _ "smallserial" _ = [t| Integer |]
    mapColumnType _ "serial" _ = [t| Integer |]
    mapColumnType _ "bigserial" _ = [t| Integer |]
    mapColumnType _ "character varying" _ = [t| String |]
    mapColumnType _ "character" _ = [t| String |]
    mapColumnType _ "text" _ = [t| String |]
    mapColumnType _ "timestamp with time zone" _ = [t| UTCTime |]
    mapColumnType _ "timestamp" _ = [t| LocalTime |]
    mapColumnType _ "timestamp without time zone" _ = [t| LocalTime |]
    mapColumnType _ "date" _ = [t| Day |]
    mapColumnType _ "time with time zone" _ = [t| TimeOfDay |]
    mapColumnType _ "time without time zone" _ = [t| TimeOfDay |]

data Dialect' = Dialect'

data PGTableLockMode = ACCESS_SHARE
                     | ROW_SHARE
                     | ROW_EXCLUSIVE
                     | SHARE_UPDATE_EXCLUSIVE
                     | SHARE
                     | SHARE_ROW_EXCLUSIVE
                     | EXCLUSIVE
                     | ACCESS_EXCLUSIVE
                     deriving (Eq, Show)

data PGRecordLockMode = ForKeyShare
                      | ForShare
                      | ForNoKeyUpdate
                      | ForUpdate
                      deriving (Eq, Show)

instance QualifyRecordLock 'ForKeyShare where
    qualifyRecordLock _ query = query ++ " FOR KEY SHARE"
instance QualifyRecordLock 'ForShare where
    qualifyRecordLock _ query = query ++ " FOR SHARE"
instance QualifyRecordLock 'ForNoKeyUpdate where
    qualifyRecordLock _ query = query ++ " FOR NO KEY UPDATE"
instance QualifyRecordLock 'ForUpdate where
    qualifyRecordLock _ query = query ++ " FOR UPDATE"

showTableLockMode :: PGTableLockMode
                  -> String
showTableLockMode mode = map repl $ show mode
    where
        repl '_' = ' '
        repl c = c

instance Dialect Dialect' where
    type TableLockMode Dialect' = PGTableLockMode
    type RecordLockMode Dialect' = PGRecordLockMode
    getConnectionId = fetchConnectionId
    readTableMeta = examineTable
    readLatestSequences = latestSequences
    lockTables d mode tables = forM_ tables $ lockTable d mode

fetchConnectionId _ context = do
    stmt <- prepare (connect context) "SELECT pg_backend_pid()"
    execute stmt []
    row <- fetchRow stmt
    return $ (\r -> show (fromSql (r !! 0) :: Integer)) <$> row

latestSequences :: forall db. (WithDB db)
                => Dialect' -- ^ Dialect.
                -> ColumnMeta -- ^ Auto incremental column.
                -> Int -- ^ Inserted records by the latest insert query.
                -> IO [Int] -- ^ Generated values on the latest insert query.
latestSequences _ c n = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt
    stmt <- prepare (connect context) "SELECT lastval()"
    execute stmt []
    row <- fetchRow stmt
    return $ maybe [] (\r -> let v = fromSql (r !! 0) :: Int in [v - n + 1 .. v]) row

lockTable :: forall db. (WithDB db)
          => Dialect'
          -> PGTableLockMode
          -> String
          -> IO ()
lockTable _ mode table = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt
    stmt <- prepare (connect context) $ "LOCK TABLE " ++ table ++ " IN " ++ showTableLockMode mode ++ " MODE"
    execute stmt []
    return ()

examineTable :: forall db. (WithDB db)
             => Dialect'
             -> String
             -> IO TableMeta
examineTable _ t = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt
    let conn = connect context

    -- cols :: [ColumnMeta] -- ordinal_position順。ただしキーと関連に関わる情報は入っていない。
    cols <- _fetchAnd conn "\
                            \ SELECT \
                            \   c.column_name, c.data_type, c.is_nullable, c.column_default, c.udt_name, \
                            \   e.data_type AS element_type \
                            \ FROM \
                            \   information_schema.columns AS c \
                            \   LEFT JOIN information_schema.element_types AS e \
                            \     ON ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier) \
                            \       = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier)) \
                            \ WHERE c.table_name = ? ORDER BY c.ordinal_position \
                            \" [t] _parseColumnMeta

    keys <- _fetchAnd conn "\
                            \ SELECT \
                            \   kcu.constraint_name, kcu.column_name, \
                            \   tc.constraint_type \
                            \ FROM \
                            \   information_schema.key_column_usage AS kcu \
                            \   INNER JOIN information_schema.table_constraints AS tc \
                            \     ON kcu.constraint_name = tc.constraint_name \
                            \ WHERE kcu.table_name = ? \
                            \" [t] _parseKeyInfo

    -- pks :: [String] -- PKカラム名リスト。
    -- fks :: [(String, String)] -- FKの(カラム名, 制約名)リスト。
    let (pks, fks) = _classifyKeys keys

    -- rels :: [(String, Relation)] -- (制約名, 関連情報)リスト。
    rels <- if length fks == 0
                then return []
                else _fetchAnd conn ("\
                                      \ SELECT \
                                      \   table_name, column_name, constraint_name \
                                      \ FROM \
                                      \   information_schema.constraint_column_usage \
                                      \ WHERE \
                                      \   constraint_name IN (" ++ holder (length fks) ++ ") \
                                      \") (map snd fks) _parseConstraint

    -- PK/FK情報を埋める。
    return $ TableMeta t $ swapFK fks rels $ swapPK pks cols
    where 
        swapPK pks cols = flip map cols $ \c -> if columnName c `elem` pks then c { isPrimary = True} else c
        swapFK :: [(String, String)] -> [(String, Relation)] -> [ColumnMeta] -> [ColumnMeta]
        swapFK fks rels cols = flip map cols $ \c -> 
            let constrs = map snd $ filter (\k -> fst k == columnName c) fks
                relMap = M.fromList rels
            in c { relations = catMaybes (map (\n -> M.lookup n relMap) constrs) }

data KeyType = PRIMARY | FOREIGN String deriving (Eq, Show)

_classifyKeys :: [(String, KeyType)]
              -> ([String], [(String, String)])
_classifyKeys keys = (mconcat $ map ifPrimary keys, mconcat $ map ifForeign keys)
    where
        ifPrimary k = if snd k == PRIMARY then [fst k] else []
        ifForeign k = case snd k of 
                        FOREIGN n -> [(fst k, n)]
                        _ -> []


_fetchAnd :: (IConnection c, Convertible a SqlValue)
          => c
          -> String
          -> [a]
          -> (M.Map String SqlValue -> [b])
          -> IO [b]
_fetchAnd conn q holder f = do
    stmt <- prepare conn q
    _ <- execute stmt $ map toSql holder
    rows <- fetchAllRowsMap stmt
    return $ L.foldl (++) [] $ map f rows


_parseColumnMeta :: M.Map String SqlValue
                 -> [ColumnMeta]
_parseColumnMeta row = [ColumnMeta { isPrimary = False
                                   , columnName = name
                                   , columnType = typ
                                   , userType = udt
                                   , isNullable = null
                                   , isAutoIncrement = auto
                                   , relations = []
                                   }]
    where
        name = fromSql $ row M.! "column_name" :: String
        typ = fromSql $ row M.! "data_type" :: String
        null = (fromSql $ row M.! "is_nullable" :: String) == "YES"
        --auto = L.isPrefixOf "nextval(" (fromSql $ row M.! "column_default" :: String)
        auto = let v = row M.! "column_default"
               in case v of
                    SqlNull -> False
                    _ -> L.isPrefixOf "nextval(" (fromSql v :: String)
        udt = fromSql $ row M.! "udt_name" :: String
        --elt = fromSql $ row M.! "element_type" :: String
          
_parseKeyInfo :: M.Map String SqlValue
              -> [(String, KeyType)]
_parseKeyInfo row = case typ of 
                        "PRIMARY KEY" -> [(name, PRIMARY)]
                        "FOREIGN KEY" -> [(name, FOREIGN cnst)]
                        _ -> []
    where
        name = fromSql $ row M.! "column_name" :: String
        typ = fromSql $ row M.! "constraint_type" :: String
        cnst = fromSql $ row M.! "constraint_name" :: String

_parseConstraint :: M.Map String SqlValue
                 -> [(String, Relation)]
_parseConstraint row = [(cnst, Relation table name)]
    where
        name = fromSql $ row M.! "column_name" :: String
        table = fromSql $ row M.! "table_name" :: String
        cnst = fromSql $ row M.! "constraint_name" :: String