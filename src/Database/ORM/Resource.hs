{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Database.ORM.Resource where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import Data.Maybe (maybe)
import Data.Pool
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC

instance (DBSettings db) => Resource (DBResource db) where
    type ContextType (DBResource db) = DBContext db

    newContext r = liftIO $ newIORef $ DBContext undefined True r Nothing Nothing

instance (DBSettings db) => ResourceContext (DBContext db) where
    type ResourceType (DBContext db) = DBResource db

    closeContext c b
        | b && status c = do
            logD' loggerTag $ "(#" ++ maybe "None" id (connectionId c) ++ ") Commit connection"
            liftIO $ commit (connect c) >> return c
        | otherwise = do
            logD' loggerTag $ "(#" ++ maybe "None" id (connectionId c) ++ ") Rollback connection"
            liftIO $ rollback (connect c) >> return c

    execContext c f = do
        cxt <- liftIO $ readIORef c
        r <- liftIO $ readIORef (resource cxt)
        withPool (pool r) $ \conn -> do
            liftIO $ do
                connId <- getConnectionId (dialect $ settings $ r) (cxt { connect = conn })
                writeIORef c (cxt { connect = conn, connectionId = connId })
            f

    failedContext cxt e = do
        logD' loggerTag $ "(#" ++ maybe "None" id (connectionId cxt) ++ ") Mark this context is failed"
        return $ cxt { status = False }