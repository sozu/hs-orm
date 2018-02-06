{-# LANGUAGE TypeFamilies #-}

module Database.ORM.Resource where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.IORef
import Data.Pool
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC

instance (DBSettings db) => Resource (DBResource db) where
    type ContextType (DBResource db) = DBContext db

    newContext r = liftIO $ newIORef $ DBContext undefined True r

instance (DBSettings db) => ResourceContext (DBContext db) where
    type ResourceType (DBContext db) = DBResource db

    closeContext c b
        | b && status c = liftIO $ commit (connect c) >> return c
        | otherwise = liftIO $ rollback (connect c) >> return c

    execContext c f = do
        cxt <- liftIO $ readIORef c
        r <- liftIO $ readIORef (resource cxt)
        withResource (pool r) $ \conn -> do
            liftIO $ writeIORef c (cxt { connect = conn })
            f