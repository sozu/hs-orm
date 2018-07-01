{-# LANGUAGE TypeFamilies #-}

module Database.ORM.Dialect.Mock (
    Mock(..)
) where

import qualified Data.Map as M
import Data.Maybe (maybe)
import Database.HDBC
import Database.HDBC.Statement
import qualified Database.ORM.HDBC as D

data Mock = Mock { dbUrl :: D.DBURL
                 , tables :: M.Map String D.TableMeta
                 }

data MockConnection = MockConnection

instance D.DBSettings Mock where
    type ConnectionType Mock = MockConnection
    type DialectType Mock = Dialect
    url = dbUrl
    maxConnections _ = 1
    open = open
    dialect s = Dialect s

data Dialect = Dialect Mock

data LockMock

instance D.Dialect Dialect where
    type LockMode Dialect = LockMock
    readTableMeta (Dialect s) t = return $ maybe (D.TableMeta t []) id $ M.lookup t (tables s)
    readLatestSequences _ _ _ = return []
    lockTables _ _ _ = return ()

instance IConnection MockConnection where
    disconnect _ = return ()
    commit _ = return ()
    rollback _ = return ()
    run _ _ _ = return 0
    prepare _ q = return $ Statement { execute = (\_ -> return 0)
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
open _ = return MockConnection
