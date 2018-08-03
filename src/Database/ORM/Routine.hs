{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.Routine where

import GHC.TypeLits
import Data.Proxy
import Data.IORef
import Control.Monad.Identity
import Data.Convertible
import Data.Extensible
import Data.Extensible.HList
import Data.Model.Graph
import Data.Resource
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Query
import Database.ORM.Select
import Database.ORM.Delete
import Database.ORM.Handler
import Database.ORM.Utility
import Debug.Trace

-- ------------------------------------------------------------
-- SQL Execution
-- ------------------------------------------------------------

-- | Executs a query with values in @PlaceHolder@.
executeSQL :: forall db h. (WithDB db, PlaceHolder h)
           => String -- ^ Query string.
           -> h -- ^ @PlaceHolder@ holding values used in query.
           -> IO Integer -- ^ The number of affected rows.
executeSQL sql holder = do
    conn <- readIORef (contextOf @(DBContext db) ?cxt) >>= return . connect
    stmt <- prepare conn sql
    execute stmt $ holderValues holder

-- | Another version of @executeSQL@ discarding returned values.
executeSQL_ :: forall db h. (WithDB db, PlaceHolder h)
            => String -- ^ Query string.
            -> h -- ^ @PlaceHolder@ holding values used in query.
            -> IO () -- ^ No values.
executeSQL_ sql holder = executeSQL sql holder >> return ()

-- ------------------------------------------------------------
-- Select
-- ------------------------------------------------------------

class Fetcher g a (args :: [*]) (keys :: [Symbol]) where
    fetcher :: (WithDB db) => [(String, SqlValue)] -> Apply args (IO g)

instance ( GraphContainer g a
         , SelectNodes g a (EdgeTypes g a)
         , ElemIndexes (ReplicateType a (RW'KeyTypes a)) (EdgeTypes g a)
         , PKConditions a (ReplicateType a (RW'KeyTypes a))
         , KnownNat (Length (EdgeTypes g a))
         , Forall SqlValueConstraint (RW'KeyTypes a)
         ) => Fetcher g a '[] '[] where
    fetcher values = do
        let conds = pkConditions (Proxy :: Proxy a) (Proxy :: Proxy (ReplicateType a (RW'KeyTypes a))) (reverse values)
        selectNodes (Proxy :: Proxy g) (Proxy :: Proxy a) conds (../) Nothing

instance ( GraphContainer g a
         , SelectNodes g a (EdgeTypes g a)
         , ElemIndexes (ReplicateType a (RW'KeyTypes a)) (EdgeTypes g a)
         , PKConditions a (ReplicateType a (RW'KeyTypes a))
         , KnownNat (Length (EdgeTypes g a))
         , Forall SqlValueConstraint (RW'KeyTypes a)
         , KnownSymbol k
         , Convertible v SqlValue
         , Fetcher g a vs ks
         ) => Fetcher g a (v ': vs) (k ': ks) where
    fetcher values = \v -> fetcher @g @a @vs @ks ((symbolVal (Proxy :: Proxy k), toSql v) : values)

-- | Returns a function to obtain a graph contains a record and its relation records by primary keys.
--
-- The type of target record is determined by the top type of the graph.
-- Graph type should be specified by using type application.
--
-- Returned function accept arguments of types of columns represented by @PK@ appendix of target model.
--
-- > type A = "a" :## '["id" :> Int, "key" :> String, "name" :> String] :^+ PK '["id", "key"]
-- > type B = "b" :## '["id" :> Int, "name" :> String]
-- > graph <- fetchRecord @(Graph A :><: B :><: Edge (B :- A)) 1 "abc"
--
-- In above example, 'graph' contains a record of 'a' selected by 'id = 1 and key = "abc"'
-- and records of 'b' having foreign keys indicating the 'a' record.
--
-- Note: Columns specified by @PK@ must not be an actual primary key of the tabla. 
fetchOne :: forall g a db. (
            WithDB db
          , a ~ GraphTop g
          , GraphContainer g a
          , SelectNodes g a (EdgeTypes g a)
          , KnownNat (Length (EdgeTypes g a))
          , ElemIndexes (ReplicateType a (RW'KeyTypes a)) (EdgeTypes g a)
          , PKConditions a (ReplicateType a (RW'KeyTypes a))
          , Identifiable a
          , Forall SqlValueConstraint (RW'KeyTypes a)
          , Fetcher g a (RW'KeyTypes a) (RW'Key a)
          )
          => Apply (RW'KeyTypes a) (IO g) -- ^ Function to obtain the graph by taking primary key values.
fetchOne = fetcher @g @a @(RW'KeyTypes a) @(RW'Key a) []

-- ------------------------------------------------------------
-- Insert
-- ------------------------------------------------------------

-- | Inserts a record into a table.
insertOne :: forall db r. (
             WithDB db
           , RecordWrapper r
           , ForWhat r
           , ForInsert r ~ 'True)
          => r -- ^ Record to insert.
          -> IO r -- ^ Inserted record where the value of auto incremental column is filled.
insertOne record = do
    graph <- restoreGraph . fst $ record +< (newGraph :: Graph r)
    return $ head $ (values graph :: [r])

-- ------------------------------------------------------------
-- Update
-- ------------------------------------------------------------

-- | Update a record of a table.
updateOne :: forall db r. (
             WithDB db
           , RecordWrapper r
           , ForWhat r
           , ForUpdate r ~ 'True)
          => r -- ^ Record to update.
          -> IO () -- ^ Returns nothing.
updateOne record = do
    graph <- restoreGraph . fst $ record +< (newGraph :: Graph r)
    return ()

-- ------------------------------------------------------------
-- Delete
-- ------------------------------------------------------------

class Deleter g a (args :: [*]) (keys :: [Symbol]) where
    deleter :: (WithDB db) => [(String, SqlValue)] -> Apply args (IO ())

instance ( GraphContainer g a
         , RecordWrapper (Head (EdgeTypes g a))
         , PKConditions a (ReplicateType a (RW'KeyTypes a))
         , KnownNat (Length (EdgeTypes g a))
         , Forall SqlValueConstraint (RW'KeyTypes a)
         , Deletable g a (ReplicateType a (RW'KeyTypes a))
         ) => Deleter g a '[] '[] where
    deleter values = do
        let conds = pkConditions (Proxy :: Proxy a) (Proxy :: Proxy (ReplicateType a (RW'KeyTypes a))) (reverse values)
        deleteByCondition (Proxy :: Proxy g) (Proxy :: Proxy a) conds
        return ()

instance ( GraphContainer g a
         , PKConditions a (ReplicateType a (RW'KeyTypes a))
         , KnownNat (Length (EdgeTypes g a))
         , Forall SqlValueConstraint (RW'KeyTypes a)
         , KnownSymbol k
         , Convertible v SqlValue
         , Deleter g a vs ks
         ) => Deleter g a (v ': vs) (k ': ks) where
    deleter values = \v -> deleter @g @a @vs @ks ((symbolVal (Proxy :: Proxy k), toSql v) : values)

-- | Returns a function to delete a record from a tabel by its primary keys.
--
-- Returned function accepts values of primary keys in the same order as @PK@ appendix.
-- Usage of the function is similar to @fetchOne@, show its description to know the detail.
deleteOne :: forall g a db. (
             WithDB db
           , PKConditions a (ReplicateType a (RW'KeyTypes a))
           , Identifiable a
           , Forall SqlValueConstraint (RW'KeyTypes a)
           , Deleter g a (RW'KeyTypes a) (RW'Key a))
          => Apply (RW'KeyTypes a) (IO ()) -- ^ Function to delete a record by taking primary key values.
deleteOne = deleter @g @a @(RW'KeyTypes a) @(RW'Key a) []

-- ------------------------------------------------------------
-- Helper types and functions
-- ------------------------------------------------------------

type family GraphTop g :: *
type instance GraphTop (Graph a :><: as) = a
type instance GraphTop (Graph a) = a

type family ReplicateType a (as :: [*]) :: [*] where
    ReplicateType a '[] = '[]
    ReplicateType a (x ': xs) = a ': (ReplicateType a xs)

class PKConditions (t :: *) (ts :: [*]) where
    pkConditions :: Proxy t -> Proxy ts -> [(String, SqlValue)] -> Condition ts

instance (RecordWrapper t) => PKConditions t '[t] where
    pkConditions _ _ ((c, v) : _) = (==?) @t c v
instance (RecordWrapper t, PKConditions t ts) => PKConditions t (t ': ts) where
    pkConditions p _ ((c, v) : vs) = (==?) @t c v .& pkConditions p (Proxy :: Proxy ts) vs
