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
{-# LANGUAGE OverloadedLabels #-}

module Database.ORM.Routine where

import GHC.TypeLits
import Data.Proxy
import Data.IORef
import Control.Monad.Identity
import Data.Convertible
import Control.Lens hiding ((:>))
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
import Database.ORM.Functionality
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
    fetcher :: (WithDB db, ApplyRecordLock db (RW'Spec a)) => [(String, SqlValue)] -> Apply args (IO g)

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
          , Identifiable a
          , ApplyRecordLock db (RW'Spec a)
          , Fetcher g a (RW'KeyTypes a) (RW'Key a)
          )
          => Apply (RW'KeyTypes a) (IO g) -- ^ Function to obtain the graph by taking primary key values.
fetchOne = fetcher @g @a @(RW'KeyTypes a) @(RW'Key a) []

type CountModel = ExtraModel '["count" :> Integer] '[ColExp "count" "COUNT(*)"]

type ForCount g = JustRelate g :><: CountModel :><: (CountModel :- ((=*) (GraphTop g)))

-- | Make roles of all model types in graph to @Relate'@.
--
-- Note: GHC can't compile higher-order type families like 'Map (f :: * -> *) (xs :: [*])' despite GHCi can.
-- General type function to convert types of nodes is not available currently.
type family JustRelate (g :: *) where
    JustRelate (Graph x) = Graph ((=*) x)
    JustRelate (g :><: x) = JustRelate g :><: (=*) x

type family AllRelate (ts :: [*]) where
    AllRelate '[]       = '[]
    AllRelate (t ': ts) = (=*)t ': AllRelate ts

countCondition :: forall ts. Condition ts
               -> Condition (AllRelate ts)
countCondition (Condition fmt _ vs) = Condition fmt (Proxy :: Proxy (AllRelate ts)) vs

-- | Counts the number of rows of a graph.
--
-- The graph type should be specified by type application.
countGraph :: forall g g' a' (ts :: [*]) db. (
              WithDB db
            , g' ~ ForCount g
            , a' ~ GraphTop g'
            , GraphFactory (JustRelate g)
            , GraphContainer g' a'
            , SelectNodes g' a' (EdgeTypes g' a')
            , KnownNat (Length (EdgeTypes g' a'))
            , ApplyRecordLock db (RW'Spec a')
            , ElemIndexes (AllRelate ts) (EdgeTypes g' a')
            )
            => Condition ts -- ^ Conditions.
            -> IO Integer -- ^ The number of rows.
countGraph conds = do
    graph <- selectNodes (Proxy :: Proxy g') (Proxy :: Proxy a') (countCondition conds) (../) Nothing
    return $ view #count $ (values graph :: [CountModel]) !! 0

-- | Counts the number of rows of a table.
--
-- The table should be specified by type application.
countTable :: forall a a' g' (ts :: [*]) db. (
              WithDB db
            , a' ~ ((=*) a)
            , g' ~ ForCount (Graph a)
            , RecordWrapper a'
            , SelectNodes g' a' (EdgeTypes g' a')
            , KnownNat (Length (EdgeTypes g' a'))
            , ApplyRecordLock db (RW'Spec a')
            , ElemIndexes (AllRelate ts) (EdgeTypes g' a')
            )
            => Condition ts -- ^ Conditions.
            -> IO Integer -- ^ The number of rows.
countTable conds = countGraph @(Graph a) conds

-- ------------------------------------------------------------
-- Insert
-- ------------------------------------------------------------

-- | Inserts a record into a table.
insertOne :: forall db r. (
             WithDB db
           , RecordWrapper r
           , RW'Role r ~ Insert')
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
           , RW'Role r ~ Update')
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
deleteOne :: forall a db. (
             WithDB db
           , PKConditions a (ReplicateType a (RW'KeyTypes a))
           , Identifiable a
           , Forall SqlValueConstraint (RW'KeyTypes a)
           , Deleter (Graph a) a (RW'KeyTypes a) (RW'Key a))
          => Apply (RW'KeyTypes a) (IO ()) -- ^ Function to delete a record by taking primary key values.
deleteOne = deleter @(Graph a) @a @(RW'KeyTypes a) @(RW'Key a) []

-- ------------------------------------------------------------
-- Helper types and functions
-- ------------------------------------------------------------

type family GraphTop g :: *
type instance GraphTop (g :><: a) = GraphTop g
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
