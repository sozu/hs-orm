{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Database.ORM.HDBC (
    DBURL
    , DBSettings(..)
    , DBResource(..)
    , newResource
    , WithResource
    , DBContext(..)
    , close
    , WithDB
    , withContext
    , readSchema
    , saveSchema
    , TableMeta(..)
    , ColumnMeta(..)
    , Relation(..)
    , getColumn
    , relationsTo
    , SchemaReader(..)
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Database.HDBC
import Data.Pool

type DBURL = String

-- ------------------------------------------------------------
-- Database management.
-- ------------------------------------------------------------

{- | Common settings to connect database and pool the connections.

Each instance of this class corresponds to an RDBMS such as MySQL, PostgreSQL and so on.
Developers can declare additional fields to handle vendor specific features prepared for connections.
-}
class (IConnection (ConnectionType db), SchemaReader (SchemaReaderType db)) => DBSettings db where
    {- | Returns the type of connection. The constraint forces it to implement Database.HDBC.IConnection.
    -}
    type ConnectionType db :: *

    {- | Returns the type of SchemaReader for the RDBMS which reads table schemas.
    -}
    type SchemaReaderType db :: *

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

    {- | Create a SchemaReader to raed table schema.
    -}
    schemaReader :: db -- ^ Connection settings.
                 -> (SchemaReaderType db) -- ^ SchemaReader.

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
type WithDB db = (?db :: IORef (DBContext db), WithResource db)

{- | Execute function which has an implicit parameter of DBContext.

This function needs implicit parameter `?resource` of `DBResource db`.
A connection is obtained from connection pool, then, the function is executed with it, finally, it is closed according to the status.
-}
withContext :: (WithResource db, IConnection (ConnectionType db))
            => (WithDB db => IO b) -- ^ A function to execute.
            -> IO b -- ^ A value returned by the function.
withContext f = do
    res <- readIORef ?resource
    withResource (pool res) $ \c -> do
        cxtRef <- newIORef (DBContext c True ?resource)
        let ?db = cxtRef
        r <- f
        cxt <- readIORef cxtRef
        close cxt
        return r

{- | Read schema of a table in a DBContext. 

When schema of a table is read, it is stored in DBResource referred from the context and returned for every subsequent invocation of this function.
Therefore, database is accessed only once for one table in all contexts referreing the same DBResource.
-}
readSchema :: (WithDB db)
           => String -- ^ Table name.
           -> IO TableMeta -- ^ Schema of the table.
readSchema t = do
    res <- readIORef ?resource
    case M.lookup t (schema res) of
        Just tm -> return tm
        Nothing -> do
            tm <- readTableMeta (schemaReader (settings res)) t
            saveSchema t tm
            return tm

{- | Saves a table schema in DBResource.
-}
saveSchema :: (WithResource db)
           => String -- ^ Table name.
           -> TableMeta -- ^ Schema of the table.
           -> IO () -- ^ No action.
saveSchema t meta = modifyIORef ?resource (\r -> r { schema = M.insert t meta (schema r)})

-- ------------------------------------------------------------
-- Database schema.
-- ------------------------------------------------------------

{- | This class declares a method to read table schema by a table name.

This class should be implemented for each RDBMS.
-}
class SchemaReader r where
    {- | Obtains a schema of a table.
    -}
    readTableMeta :: (WithDB db, SchemaReaderType db ~ r)
                  => r -- ^ SchemaReader.
                  -> String -- ^ Table name.
                  -> IO TableMeta -- ^ Schema of the table.

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
               , columnType :: String -- ^ Data type name of the column.
               , isNullable :: Bool -- ^ Denotes if this column is nullable.
               , isAutoIncrement :: Bool -- ^ Denotes if this column has auto incremental attribute.
               , relation :: Maybe Relation -- ^ Has a relation information if this column is a foreign key, otherwise Nothing.
               } deriving (Show)

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
relationsTo t to = map (\c -> (columnName c, fromJust $ relation c)) cols
    where cols = filter (\c -> maybe False (\r -> referenceTable r == to) (relation c)) (tableColumns t)