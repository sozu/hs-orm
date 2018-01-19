{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.ORM.Dialect.PostgreSQL (
    WithDB'
    , PostgreSQL(..)
) where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Convertible
import Data.IORef
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Maybe (maybe, fromJust)
import Database.ORM.HDBC hiding (DBSettings(..))
import Database.ORM.Query
import qualified Database.ORM.HDBC as D (DBSettings(..))

type WithDB' = WithDB PostgreSQL

data PostgreSQL = PostgreSQL {
    url :: DBURL
    , maxConnections :: Int
}

instance D.DBSettings PostgreSQL where
    type ConnectionType PostgreSQL = Connection
    type SchemaReaderType PostgreSQL = SchemaReader'
    url = url
    maxConnections = maxConnections
    open s = connectPostgreSQL (url s)
    schemaReader _ = SchemaReader'

data SchemaReader' = SchemaReader'

instance SchemaReader SchemaReader' where
    readTableMeta = examineTable

examineTable :: (WithDB db)
             => SchemaReader'
             -> String
             -> IO TableMeta
examineTable _ t = do
    context <- readIORef ?db
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
            fromJust $ let relMap = M.fromList rels
                       in (L.find (\k -> fst k == columnName c) fks >>= \k -> Just ( c { relation = M.lookup (snd k) relMap })) <|> Just c

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
_parseColumnMeta row = [ColumnMeta { isPrimary = False, columnName = name, columnType = typ, isNullable = null, isAutoIncrement = auto, relation = Nothing }]
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