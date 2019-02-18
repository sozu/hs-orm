{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Database.ORM.Dialect.Mock (
    Mock(..)
  , MockRecordLock(..)
  , latestExecution
) where

import qualified Data.Map as M
import Data.IORef
import Data.Maybe (maybe)
import Database.HDBC
import Database.HDBC.Statement
import qualified Database.ORM.HDBC as D
import Database.ORM.Functionality

data Mock = Mock { dbUrl :: D.DBURL
                 , tables :: M.Map String D.TableMeta
                 }

data MockConnection = MockConnection { latestQuery :: IORef String
                                     , latestHolder :: IORef [SqlValue]
                                     }

instance D.DBSettings Mock where
    type ConnectionType Mock = MockConnection
    type DialectType Mock = Dialect
    url = dbUrl
    maxConnections _ = 1
    open = open
    dialect s = Dialect s

data Dialect = Dialect Mock

data MockTableLock
data MockRecordLock = Exclusive | Shared

instance QualifyRecordLock 'Exclusive where
    qualifyRecordLock _ query = query ++ " FOR UPDATE"
instance QualifyRecordLock 'Shared where
    qualifyRecordLock _ query = query ++ " FOR SHARE"

instance D.Dialect Dialect where
    type TableLockMode Dialect = MockTableLock
    type RecordLockMode Dialect = MockRecordLock
    readTableMeta (Dialect s) t = return $ maybe (D.TableMeta t []) id $ M.lookup t (tables s)
    readLatestSequences _ _ _ = return []
    lockTables _ _ _ = return ()

instance IConnection MockConnection where
    disconnect _ = return ()
    commit _ = return ()
    rollback _ = return ()
    run _ _ _ = return 0
    prepare c@(MockConnection lq lh) q = do
        writeIORef lq q
        return $ Statement { execute = \h -> writeIORef lh h >> return 0
                           , executeRaw = (return ())
                           , executeMany = (\_ -> return ())
                           , finish = (return ())
                           , fetchRow = (return Nothing)
                           , getColumnNames = (return [])
                           , originalQuery = q
                           , describeResult = (return [])
                           }
    clone c = return c
    hdbcDriverName _ = "mockdb"
    hdbcClientVer _ = "0.0.0"
    proxiedClientName _ = ""
    proxiedClientVer _ = "0.0.0"
    dbServerVer _ = "0.0.0"
    dbTransactionSupport _ = True
    getTables _ = return []
    describeTable _ _ = return []

open :: Mock
     -> IO MockConnection
open _ = do
    lq <- newIORef ""
    lh <- newIORef ([] :: [SqlValue])
    return $ MockConnection lq lh

latestExecution :: MockConnection
                -> IO (String, [SqlValue])
latestExecution (MockConnection lq lh) = (,) <$> readIORef lq <*> readIORef lh
