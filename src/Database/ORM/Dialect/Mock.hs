{-# LANGUAGE TypeFamilies #-}

module Database.ORM.Dialect.Mock (
    Mock(..)
) where

import qualified Data.Map as M
import Data.Maybe (maybe)
import Database.HDBC
import qualified Database.ORM.HDBC as D

data Mock = Mock { dbUrl :: D.DBURL
                 , tables :: M.Map String D.TableMeta
                 }

data MockConnection = MockConnection

instance D.DBSettings Mock where
    type ConnectionType Mock = MockConnection
    type SchemaReaderType Mock = SchemaReader
    url = dbUrl
    maxConnections _ = 1
    open = open
    schemaReader s = SchemaReader s

data SchemaReader = SchemaReader Mock

instance D.SchemaReader SchemaReader where
    readTableMeta (SchemaReader s) t = return $ maybe (D.TableMeta t []) id $ M.lookup t (tables s)

instance IConnection MockConnection where
    disconnect _ = return ()
    commit _ = return ()
    rollback _ = return ()
    run _ _ _ = return 0
    prepare _ _ = return undefined
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
