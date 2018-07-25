{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.ORM.HDBC (
    -- * Database management
    DBURL
    , loggerTag
    , ColumnValue(..)
    , toSqlValue
    , DBSettings(..)
    , DBResource(..)
    , newResource
    , WithResource
    , DBContext(..)
    , close
    , WithDB
    , readSchema
    , saveSchema
    -- * Dialects
    , Dialect(..)
    , getDialect
    -- * Schema
    , TableMeta(..)
    , ColumnMeta(..)
    , hasRelation
    , Relation(..)
    , getColumn
    , relationsTo
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import Data.Convertible
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Database.HDBC
import Data.Pool
import Data.Resource

type DBURL = String

loggerTag :: String
loggerTag = "Database.ORM"

-- ------------------------------------------------------------
-- Extensible SqlValue
-- ------------------------------------------------------------

data ColumnValue = ValueOf SqlValue
                 | Qualified ColumnValue
                 | RawExpression String
                 deriving (Eq, Show)

toSqlValue :: ColumnValue
           -> Maybe SqlValue
toSqlValue (ValueOf v) = Just v
toSqlValue (Qualified v) = toSqlValue v
toSqlValue (RawExpression _) = Nothing

-- ------------------------------------------------------------
-- Database management.
-- ------------------------------------------------------------

{- | Common settings to connect database and pool the connections.

Each instance of this class corresponds to an RDBMS such as MySQL, PostgreSQL and so on.
Developers can declare additional fields to handle vendor specific features prepared for connections.
-}
class (IConnection (ConnectionType db), Dialect (DialectType db)) => DBSettings db where
    {- | Returns the type of connection. The constraint forces it to implement Database.HDBC.IConnection.
    -}
    type ConnectionType db :: *

    {- | Returns the type of Dialect for the RDBMS which reads table schemas.
    -}
    type DialectType db :: *

    {- | Create an URL to connect database from settings.
    -}
    url :: db -- ^ Connection settings.
        -> DBURL -- ^ URL which is available to connect.

    {- | Create new connection to database.
    -}
    open :: db -- ^ Connection settings.
         -> IO (ConnectionType db) -- ^ Created connection.

    {- | Get the maximum number of connections in a pool.
    -}
    maxConnections :: db -- ^ Connection settings.
                   -> Int -- The maximum number of connections.
    maxConnections _ = 10

    {- | Create a Dialect to raed table schema.
    -}
    dialect :: db -- ^ Connection settings.
            -> (DialectType db) -- ^ Dialect.

{- | DBResource manages connection pool of a database specified by settings record.

This type also holds tables schemas as the map where each key is a table name.
-}
data DBResource db = DBResource { settings :: db -- ^ Settings to connect a database.
                                , schema :: M.Map String TableMeta -- ^ Table schemas mapped by their names.
                                , pool :: Pool (ConnectionType db) -- ^ Connection pool.
                                }

{- | Create new resource which manages connnections to a database.
-}
newResource :: (DBSettings db, IConnection (ConnectionType db))
            => db -- ^ Settings to connect a database.
            -> IO (IORef (DBResource db)) -- ^ IORef holding created resource.
newResource s = do
    pool <- createPool (open s) disconnect 1 3600 (maxConnections s)
    newIORef (DBResource s M.empty pool)

{- | Constraint type to declare that the function has an implicit parameter `?resource` whose type is `DBResource db`.
-}
type WithResource db = (?resource :: IORef (DBResource db), DBSettings db)

{- | DBContext manages an active connection obtained from DBResource.

This type also holds the flag which decides whether the connection should be committed or rollbacked on closing context.
This flag is available for users to control a transaction.
-}
data DBContext db = DBContext { connect :: ConnectionType db -- ^ Get a connection.
                              , status :: Bool -- ^ If true, transaction will be committed on closing context, otherwise rollbacked.
                              , resource :: IORef (DBResource db) -- ^ Resource the connection is obtained from.
                              }

{- | Closes the context. This closes transaction by executing commit or rollback according to the status.
-}
close :: (IConnection (ConnectionType db))
      => DBContext db
      -> IO ()
close (DBContext c True _) = commit c
close (DBContext c False _) = rollback c

{- | Constraint type to declare that the function has an implicit parameter `?db` whose type is `DBContext db`.
-}
--type WithDB db = (?db :: IORef (DBContext db), WithResource db)
type WithDB db = (With '[DBContext db], DBSettings db)

{- | Read schema of a table in a DBContext. 

When schema of a table is read, it is stored in DBResource referred from the context and returned for every subsequent invocation of this function.
Therefore, database is accessed only once for one table in all contexts referreing the same DBResource.
-}
readSchema :: forall db. (WithDB db)
           => String -- ^ Table name.
           -> IO TableMeta -- ^ Schema of the table.
readSchema t = do
    cxt <- readIORef $ contextOf @(DBContext db) ?cxt
    res <- readIORef $ resource cxt
    case M.lookup t (schema res) of
        Just tm -> return tm
        Nothing -> do
            tm <- readTableMeta (dialect (settings res)) t
            saveSchema t tm
            return tm

{- | Saves a table schema in DBResource.
-}
saveSchema :: forall db. (WithDB db)
           => String -- ^ Table name.
           -> TableMeta -- ^ Schema of the table.
           -> IO () -- ^ No action.
saveSchema t meta = do
    cxt <- readIORef $ contextOf @(DBContext db) ?cxt
    let res = resource cxt
    $(logQD' "Database.ORM") ?cxt $ "HDBC: Save schema of '" ++ t ++ "'"
    modifyIORef res (\r -> r { schema = M.insert t meta (schema r)})

-- ------------------------------------------------------------
-- Dialects.
-- ------------------------------------------------------------

{- | This class declares a method to read table schema by a table name.

This class should be implemented for each RDBMS.
-}
class Dialect d where
    type LockMode d :: *

    {- | Obtains a schema of a table.
    -}
    readTableMeta :: (WithDB db, DialectType db ~ d)
                  => d -- ^ Dialect.
                  -> String -- ^ Table name.
                  -> IO TableMeta -- ^ Schema of the table.

    -- | Obtains the latest generated values for auto incremental column.
    readLatestSequences :: (WithDB db, DialectType db ~ d)
                        => d -- ^ Dialect.
                        -> ColumnMeta -- ^ Auto incremental column.
                        -> Int -- ^ Inserted records by the latest insert query.
                        -> IO [Int] -- ^ Generated values on the latest insert query.

    multiInsertQuery :: d -- ^ Dialect.
                     -> TableMeta -- ^ Table schema.
                     -> [String] -- ^ Column names to insert.
                     -> [[ColumnValue]] -- ^ Column values of records.
                     -> (String, String) -- ^ Strings of query and place holders.
    multiInsertQuery _ t cols vals = (queryBase, L.intercalate ", " (map eachRecord vals))
        where
            queryBase = "INSERT INTO " ++ tableName t ++ " (" ++ L.intercalate ", " cols ++ ") VALUES "
            eachRecord cvs =
                let exp cv = case cv of
                                ValueOf v -> "?"
                                RawExpression s -> s
                                Qualified v -> exp v
                in "(" ++ L.intercalate ", " (map exp cvs) ++ ")"

    lockTables :: (WithDB db, DialectType db ~ d)
               => d
               -> LockMode d
               -> [String]
               -> IO ()

-- | Shortcut function to get dialect instance from DBContext.
getDialect :: forall db d. (WithDB db, DialectType db ~ d, Dialect d)
           => IO d -- ^ Dialect instance.
getDialect = do
    cxt <- readIORef $ contextOf @(DBContext db) ?cxt
    res <- readIORef $ resource cxt
    return $ dialect (settings res)

-- ------------------------------------------------------------
-- Database schema.
-- ------------------------------------------------------------

-- TODO: Need to handle foreign key columns which are part of primary keys correctly.

{- | Schema of a table.
-}
data TableMeta =
    TableMeta { tableName :: String -- ^ Table name.
              , tableColumns :: [ColumnMeta] -- ^ Columns of the table.
              } deriving (Show)

{- | Schema of a column.
-}
data ColumnMeta =
    ColumnMeta { isPrimary :: Bool -- ^ Denotes if this column is a primary key.
               , columnName :: String -- ^ Column name.
               , columnType :: String -- ^ Type name defined by DB engine.
               , userType :: String -- ^ Type name defined by user.
               , isNullable :: Bool -- ^ Denotes if this column is nullable.
               , isAutoIncrement :: Bool -- ^ Denotes if this column has auto incremental attribute.
               , relations :: [Relation] -- ^ Relation informations of this column.
               } deriving (Show)

-- | Checks if the column is foreign key.
hasRelation :: ColumnMeta
            -> Bool
hasRelation c = length (relations c) > 0

{- | Relation information of a foreign key column.
-}
data Relation =
    Relation { referenceTable :: String -- ^ Table name the column references.
             , referenceColumn :: String -- ^ Column name the column references.
             } deriving (Show)

{- | Get schema of a column. 
-}
getColumn :: TableMeta -- ^ Table schema.
          -> String -- ^ Column name.
          -> Maybe ColumnMeta -- ^ Schema of the column if its exists.
getColumn t n = L.find (\c -> columnName c == n) $ tableColumns t

{- | Collects informations of relations to a table in a table.
-}
relationsTo :: TableMeta -- ^ Schema of a table from which relations will be collected.
            -> String -- ^ Name of referenced table.
            -> [(String, Relation)] -- ^ List where each tuple has referencing column name and relation information.
relationsTo t to = catMaybes $ map references (tableColumns t)
    where
        references c = (columnName c,) <$> L.find (\r -> referenceTable r == to) (relations c)