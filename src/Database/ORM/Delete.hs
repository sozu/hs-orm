{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.Delete (
    -- * Execute delete
    deleteNodes
    , delete
    , deleteKeys
    , deleteByCondition
    -- * Build delete query
    , pkColumnsAndValues
    , pkDeleteQuery
    , joinDeleteQuery
    , Deletable(..)
) where

import Control.Monad.State
import qualified Data.List as L
import Data.IORef
import Data.Either (isLeft, isRight)
import Data.Maybe (maybe, catMaybes)
import GHC.TypeLits
import Data.Proxy
import Database.HDBC
import Data.Model.Graph
import Data.Resource
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Record
import Database.ORM.Condition
import Database.ORM.Utility
import qualified Database.ORM.Select as S -- to get join informations in deleteByCondition

-- | Deletes records of nodes specified by cursors.
deleteNodes :: forall db g a. (WithDB db, GraphContainer g a, RecordWrapper a)
            => g -- ^ A graph.
            -> [Cursor a] -- ^ Cursors indicating nodes of records to delete.
            -> IO Integer -- ^ The number of affected rows.
deleteNodes graph [] = return 0
deleteNodes graph cursors = do
    let models = map (@< graph) cursors
    delete models

-- | Deletes records represented by models.
delete :: forall db a. (WithDB db, RecordWrapper a)
       => [a] -- ^ Models representing records.
       -> IO Integer -- ^ The number of affected rows.
delete [] = return 0
delete models = do
    ta <- readSchema $ getName (Proxy :: Proxy a)

    context <- readIORef $ contextOf @(DBContext db) ?cxt

    let vs = map (pkColumnsAndValues ta) models

    let (q, holder) = pkDeleteQuery ta vs

    logDWithId $ "SQL: " ++ q

    stmt <- prepare (connect context) q
    execute stmt holder

deleteKeys :: forall db a k. (WithDB db, RecordWrapper a)
           => Proxy a
           -> [k]
           -> IO Integer
deleteKeys _ [] = return 0
deleteKeys pa keys = do
    -- TODO
    return 0

-- | Constraint for deletion by condition.
type Deletable g a ts = ( GraphContainer g a
                        , RecordWrapper a
                        , AllRecord (S.EdgeTypes g a)
                        , AllRecord ts
                        , ListTables (S.EdgeTypes g a)
                        , ForEachType (S.EdgeTypes g a) RecordWrapper
                        , S.Joins g (S.Edges g) (S.EdgeTypes g a)
                        , KnownNat (ElemIndex a (S.EdgeTypes g a))
                        , ContainsAll (S.EdgeTypes g a) ts NoConstraint
                        )

{- | Deletes records selected by condition.

    Given graph must include all model types appeareing in the condition.
    This function deletes records of table determined by the model of second argument.
    Other model types in the graph are used just for resolving relationship between tables.
-}
deleteByCondition :: forall db g a ts ms. (WithDB db, ms ~ S.EdgeTypes g a, Deletable g a ts, RecordWrapper (Head ms), KnownNat (Length ms))
                  => Proxy g -- ^ Type of a graph.
                  -> Proxy a -- ^ Model type to be deleted.
                  -> Condition ts -- ^ Conditions to select records.
                  -> IO Integer -- ^ The number of affected rows.
deleteByCondition pg pa conds = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt

    let n = natVal (Proxy :: Proxy (Length ms))
    let aliases = map (\i -> 't':show i) [0..n-1]

    ta <- readSchema $ getName pa

    let w = formatCondition conds (Proxy :: Proxy ms) aliases
    (q, holder) <- joinDeleteQuery pg pa conds ta aliases

    logDWithId $ "SQL: " ++ q

    stmt <- prepare (connect context) q
    execute stmt holder

-- | Gets a list of names and values of primary key columns.
pkColumnsAndValues :: (RecordWrapper a)
                   => TableMeta -- ^ Table schema.
                   -> a -- ^ A record model.
                   -> [(String, SqlValue)] -- ^ List of names and values of primary key columns.
pkColumnsAndValues t v = catMaybes $ map (\(c, v) -> v >>= \v' -> Just (c, v')) vs
    where
        pks = filter isPrimary (tableColumns t)
        vs = map (\c -> let n = columnName c in (n, fieldValue (getRecord v) n)) pks

-- | Creates query string and holder values to delete records by their primary keys.
pkDeleteQuery :: TableMeta -- ^ Table schema.
              -> [[(String, SqlValue)]] -- ^ List of primary keys of records. Each item has names and values of primary key columns.
              -> (String, [SqlValue]) -- ^ Query string and place holder.
pkDeleteQuery t vs = (body ++ L.intercalate " OR " (replicate (length vs) cond), concat $ map (map snd) vs)
    where
        body = "DELETE FROM " ++ tableName t ++ " WHERE "
        cond = "(" ++ L.intercalate " AND " (map (\c -> c ++ " = ?") (map fst $ vs !! 0)) ++ ")"

-- | Creates query string to delete records selected by conditions.
joinDeleteQuery :: forall db g a ts. (WithDB db, Deletable g a ts, RecordWrapper (Head (S.EdgeTypes g a)))
                => Proxy g -- ^ Type of a graph.
                -> Proxy a -- ^ Model type to be deleted.
                -> Condition ts -- ^ Conditions to select records.
                -> TableMeta -- ^ Table schema.
                -> [String] -- ^ Aliases of tables.
                -> IO (String, [SqlValue]) -- ^ Query string and place holder.
joinDeleteQuery pg pa conds t aliases = do
    joins <- S.collectJoins (Proxy :: Proxy (S.Edges g)) (Proxy :: Proxy (S.EdgeTypes g a)) aliases :: IO [S.JoinEdge g (S.EdgeTypes g a)]

    let index = fromInteger $ natVal (Proxy :: Proxy (ElemIndex a (S.EdgeTypes g a)))

    --let usings = filter (\(u, _) -> u /= tableName t) $ zip (listTables (Proxy :: Proxy (S.EdgeTypes g a))) aliases
    let usings = filter (\(u, _) -> u /= tableName t) $ zip (listTables' (Proxy :: Proxy (S.EdgeTypes g a))) aliases

    let c = formatCondition conds (Proxy :: Proxy (S.EdgeTypes g a)) aliases

    let jw = catMaybes (map S.joinCondition joins)
    let cw = whereClause c

    let w = if length jw == 0 then cw
                              else L.intercalate " AND " (jw ++ ["(" ++ cw ++ ")"])

    let t0 = readSchema $ getName (Proxy :: Proxy (Head (S.EdgeTypes g a)))

    let q = "DELETE FROM "
            ++ tableName t ++ " AS " ++ (aliases !! index) -- table to delete
            ++ (if length usings == 0 then ""
                                      else " USING " ++ L.intercalate ", " (map (\(u, a) -> u ++ " AS " ++ a) usings))
            ++ (if w == "" then ""
                           else " WHERE " ++ w)

    return (q, whereValues c)

listTables' :: (ForEachType as RecordWrapper)
            => Proxy as
            -> [String]
listTables' p = forEachType 0 (\i -> getName) p (Proxy :: Proxy RecordWrapper)

-- | Declares a method to get table names specified by a list of model types.
class ListTables (as :: [*]) where
    listTables :: Proxy as -> [String]

instance ListTables '[] where
    listTables _ = []

instance (ListTables as, RecordWrapper a) => ListTables (a ': as) where
    listTables _ = getName (Proxy :: Proxy a) : listTables (Proxy :: Proxy as)