{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.Handler (
    GraphHandler(..)
  , restoreGraph
) where

import Data.Proxy
import Data.Model.Graph
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Insert
import Database.ORM.Update

-- | This class declares methods to restore graph into database.
class (GraphFactory g) => GraphHandler g (as :: [*]) where
    -- | Restores graph into database.
    -- How each @TableModels@ is handled is decided by its @ModelRole@.
    restoreGraph' :: (WithDB db)
                  => g -- ^ A graph to restore.
                  -> proxy as -- ^ A proxy specifying types of @TableModel@ and @Edge@ listed in dependency order.
                  -> IO g -- ^ Restored graph.

restoreGraph :: forall g db. (GraphHandler g (Serialize g), WithDB db)
             => g
             -> IO g
restoreGraph graph = restoreGraph' graph (Proxy :: Proxy (Serialize g))

instance (GraphFactory g) => GraphHandler g '[] where
    restoreGraph' graph p = return graph

instance (GraphContainer g a, GraphHandler g as, RecordWrapper a, EdgeMap g g a, RoleForWhat (RW'Role a)) => GraphHandler g (a ': as) where
    restoreGraph' graph p = do
        let (cs, p') = serializeCursor graph (Proxy :: Proxy (a ': as))

        graph' <- if length cs > 0 then
                    let m = (cs !! 0) @< graph
                    in if roleForInsert (Proxy :: Proxy (RW'Role a)) then do
                        insertNodes graph cs 
                    else if roleForUpdate (Proxy :: Proxy (RW'Role a)) then do 
                        updateNodes graph cs
                    else 
                        return graph
                else
                    return graph

        restoreGraph' graph' p'
