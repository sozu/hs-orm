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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.ORM.Select (
    -- * Execute select
      EdgeTypes
    , SelectNodes
    , SelectQuery
    , selectNodes
    , selectQuery
    -- * Graph construction
    , addRelations
    , addEdge
    , MaybeCursor(..)
    -- * Result handling
    , RowParser(..)
    , rowToRecord
    , findInGraph
    -- * Build select query
    , createSelectQuery
    , columnsAndTables
    , selectColumns
    -- * Edges
    , Join(..)
    , JoinEdge(..)
    , joinTypeOf
    , joinCondition
    , arrangeJoins
    , Joins(..)
    , EdgeToJoin(..)
    -- * Type families
    , Edges
    , ArrangeEdges
    , findCursor'
) where

import GHC.TypeLits
import Control.Applicative
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe, isJust, fromJust)
import Data.Convertible
import Data.IORef
import Data.Proxy
import Database.HDBC
import Data.Extensible.HList
import Data.Model.Graph
import Data.Resource
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Condition
import Database.ORM.Functionality
import Database.ORM.Utility

-- ------------------------------------------------------------
-- Select functions.
-- ------------------------------------------------------------

-- | Arranged model types in graph `g` collected by tracing edges from model `a`.
type EdgeTypes g a = ArrangeEdges g a

-- | A constraint to enable select operation for the graph `g` starting from `a`.
type SelectNodes g a ms = (ms ~ EdgeTypes g a, GraphContainer g a, RecordWrapper a, ForEachType ms RecordWrapper, Contains ms a, Joins g (Edges g) ms, RowParser g ms)

-- | A constraint to enable select operation for the graph `g` using a query where selecting columns is arranged in the same order as `ms`.
type SelectQuery g ms = (Joins g (Edges g) ms, RowParser g ms)

{- | Selects rows and constructs a graph of given type.

    This function constructs a query which has following features.
    - Model types in the graph `g` which are traceable from `a` are collected and arranged into `ms`.
    - Selecting expressions are obtained from all of fields of `ms` in their orders.
    - Each edge in `g` falls into a table relation which appears as join phrase in the query. 

    The graph this function returns has following features.
    - When multiple rows returns the same records, they are merged into a model object.
    - Records in a row are considered to be related. Those relations make edges in the graph.
-}
selectNodes :: forall db g a o ts us ms. (WithDB db, ms ~ EdgeTypes g a, SelectNodes g a ms, ApplyRecordLock db (RW'Spec a), KnownNat (Length ms), ContainsAll' ms ts, ContainsAll' ms us)
            => Proxy g -- ^ Type of a graph.
            -> Proxy a -- ^ A model type where tracing models starts from.
            -> Condition ts -- ^ Conditions.
            -> OrderBy us -- ^ Sorting informations.
            -> LimitOffset -- ^ Limit and offset values if needed.
            -> IO g -- ^ Graph containing nodes and edges representing the result of query.
selectNodes pg pa conds sorts lo = do
    let modelTypes = Proxy :: Proxy (EdgeTypes g a)

    let n = natVal (Proxy :: Proxy (Length (EdgeTypes g a)))
    let aliases = map (\i -> 't':show i) [0..n-1]

    (columns, joins) <- columnsAndTables pg pa aliases

    let w = formatCondition conds modelTypes aliases
    let o = formatOrderBy sorts modelTypes aliases

    let index = indexOf 0 (Proxy :: Proxy (EdgeTypes g a)) (Proxy :: Proxy a)

    let !q = recordLockQuery (Proxy :: Proxy db)
                             (Proxy :: Proxy (RW'Spec a))
                             (createSelectQuery columns (getName pa, aliases !! index) joins w o lo)
    let !holder = whereValues w ++ maybe [] (\(l, o) -> [toSql l, toSql o]) lo

    execSelect pg columns joins modelTypes q holder

{- | Executes a query and constructs a graph holding obtained values.

    A query must conform to some rules.

    - Selecting expressions must be arranged in the same order as fields of `ms` except for foreign keys.
    - To recover relationships between tables, foreign key column and referenced column must appear in column expressions.

    TODO: Place holders are not available in column expressions .
-}
selectQuery :: forall db g p ms ms'. (WithDB db, ms' ~ UnqualifiedModels p ms, SelectQuery g ms', GenerateColumns p ms)
            => Proxy g -- ^ Type of a graph.
            -> p ms -- ^ List of model types corresponding to selecting expressions in the query.
            -> String -- ^ Query string.
            -> [SqlValue] -- ^ Values for place holders.
            -> IO g -- ^ Constructed graph.
selectQuery pg gc query holder = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt
    let conn = connect context

    let columns = generateColumns gc
    joins <- collectJoins (Proxy :: Proxy (Edges g)) (Proxy :: Proxy ms') (getAliases gc)

    execSelect pg columns joins (Proxy :: Proxy ms') query holder

-- | Executes a query and constructs a graph.
execSelect :: forall db g (ms :: [*]). (WithDB db, RowParser g ms)
           => Proxy g -- ^ Type of a graph.
           -> [[String]] -- ^ List of column expressions separated by tables.
           -> [JoinEdge g ms] -- ^ Join informations.
           -> Proxy ms -- ^ List of model types corresponding to selecting expressions in the query.
           -> String -- ^ Query string.
           -> [SqlValue] -- ^ Values for place holders.
           -> IO g -- ^ Constructed graph.
execSelect pg columns joins modelTypes query holder = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt

    logDWithId $ "SQL: " ++ query

    stmt <- prepare (connect context) query
    execute stmt holder
    rows <- fetchAllRowsAL stmt

    (_, graph) <- flip runStateT (newGraph :: g) $ do
                    forM_ rows $ \r -> do
                        cs <- parseRow modelTypes $ sep r columns
                        addRelations cs joins

    return graph
    where
        sep as [] = []
        sep as (bs:bss) = let c = length bs in take c as : sep (drop c as) bss

-- ------------------------------------------------------------
-- Graph constructions with querying result.
-- ------------------------------------------------------------

-- | New type to declare type level list of `Maybe (Cursor a)`.
newtype MaybeCursor a = MaybeCursor { getCursor :: Maybe (Cursor a) }

-- | Adds edges corresponding to relations in the graph.
addRelations :: (GraphFactory g, Monad m)
             => HList MaybeCursor (ms :: [*]) -- ^ Cursors of model types.
             -> [JoinEdge g ms] -- ^ Edge informations.
             -> StateT g m () -- ^ New state holding modified graph.
addRelations l = mapM_ (addEdge l)

-- | Adds an edge to the graph if cursors of both ends of the edge exist.
addEdge :: (GraphFactory g, Monad m)
        => HList MaybeCursor (ms :: [*]) -- ^ Cursors of model types.
        -> JoinEdge g ms -- ^ Edge information to add.
        -> StateT g m () -- ^ New state holding modified graph.
addEdge l (JoinEdge f t r _) = maybe (return ()) id $ (-*<) <$> (fmap (r +|) $ findCursor' l f) <*> findCursor' l t

instance PopType HList where
    popType (x `HCons` h) = (x, h)

-- | Finds a cursor of a type if exists.
findCursor' :: (Contains as a)
            => HList MaybeCursor as -- ^ Cursors of model types.
            -> Proxy a -- ^ A type of the cursor.
            -> Maybe (Cursor a) -- ^ Cursor if exists.
findCursor' h p = getCursor $ getByType h

-- ------------------------------------------------------------
-- Handling result of query.
-- ------------------------------------------------------------

-- | Declares method to convert result of a query into cursors.
class (GraphFactory g) => RowParser g (as :: [*]) where
    -- | Parses a row and converts values into cursors in the order of given models.
    parseRow :: (WithDB db)
             => Proxy as -- ^ List of model types deciding the type a value should be converted to.
             -> [[(String, SqlValue)]] -- ^ Pairs of column name and value in a row.
             -> StateT g IO (HList MaybeCursor as) -- ^ Returns list of cursors.

instance (GraphFactory g) => RowParser g '[] where
    parseRow _ _ = return HNil

instance (RowParser g as, GraphContainer g a, RecordWrapper a) => RowParser g (a ': as) where
    parseRow _ (vs:values) = do
        t <- liftIO $ readSchema $ getName (Proxy :: Proxy a)
        c <- rowToRecord t vs :: StateT g IO (Maybe (Cursor a))
        cs <- parseRow (Proxy :: Proxy as) values
        return $ MaybeCursor c `HCons` cs

-- | Converts values in a row into a cursor of given table.
rowToRecord :: forall g a. (GraphContainer g a, RecordWrapper a)
            => TableMeta -- ^ Table schema.
            -> [(String, SqlValue)] -- ^ Pairs of column name and value in a row.
            -> StateT g IO (Maybe (Cursor a)) -- ^ Returns a cursor unless all columns are `SqlNull`.
rowToRecord t row
    | L.all (\v -> SqlNull == snd v) row = return Nothing
    | otherwise = do
        let values = let m = M.fromList row in map (m M.!) $ fieldNames (Proxy :: Proxy (RW'Type a))
        let v = newRecord @a values
        c <- findInGraph t v
        return $ Just c

-- | Finds a model which has the same primary key as given model, and if no model is found, inserts the model into the graph.
findInGraph :: (GraphContainer g a, RecordWrapper a, Monad m)
            => TableMeta -- ^ Table schema.
            -> a -- ^ Model to insert if the same model is not found.
            -> StateT g m (Cursor a) -- ^ Returns the cursor to the model which is found or inserted.
findInGraph t v = do
    c <- (?<<) match 
    case c of
        Just c' -> return c'
        Nothing -> (+<<) v
    where
        pks = filter isPrimary (tableColumns t)
        match v' = if length pks > 0
                    then let pkv x = [fieldValue (getRecord x) (columnName c) | c <- pks]
                         in length (pkv v) /= 0 && pkv v == pkv v'
                    else False

-- ------------------------------------------------------------
-- Queries.
-- ------------------------------------------------------------

-- | Creates query string.
createSelectQuery :: (FormattedCondition c, FormattedOrderBy o)
                  => [[String]] -- ^ Qualified columns of tables.
                  -> (String, String) -- ^ Top table name and its alias.
                  -> [JoinEdge g ms] -- ^ Join informations.
                  -> c -- ^ Conditions.
                  -> o -- ^ Sorting informations.
                  -> LimitOffset -- ^ Limit and offset values if needed.
                  -> String -- ^ Created query string.
createSelectQuery cols (t, alias) joins conds sorts lo = s ++ f ++ w ++ o ++ (maybe "" (\(l, o) -> " LIMIT ? OFFSET ?") lo)
    where
        s =  "SELECT " ++ L.intercalate ", " (L.concat cols)
        f =  " FROM " ++ t ++ " AS " ++ alias ++ " " ++ L.intercalate " " (filter (/= "") $ map show joins)
        w =  let w' = whereClause conds
             in if w' == "" then "" else " WHERE " ++ w'
        o =  let o' = orders sorts
              in if o' == "" then "" else " ORDER BY " ++ o'

-- | Obtains column names and join informations to construct the graph starting from a model.
columnsAndTables :: forall db g a. (WithDB db, GraphContainer g a, SelectNodes g a (EdgeTypes g a))
                 => Proxy g -- ^ Type of graph.
                 -> Proxy a -- ^ Type of a model treated as the starting point of the graph.
                 -> [String] -- ^ Aliases of tables.
                 -> IO ([[String]], [JoinEdge g (EdgeTypes g a)]) -- ^ Column names of the tables and join informations.
columnsAndTables graph p aliases = do
    let pts = Proxy :: Proxy (EdgeTypes g a)
    joins <- collectJoins (Proxy :: Proxy (Edges g)) pts aliases
    let columns = selectColumns pts pts aliases
    return ([cs ++ fksOfTable (map getJoin joins) i | (cs, i) <- zip columns [0..]], joins)
    where
        fksOfTable :: [Maybe Join] -> Int -> [String]
        fksOfTable (Just j:js) i = if leftIndex j == i
                                    then (leftAlias j ++ "." ++ leftColumn j) : fksOfTable js i
                                    else fksOfTable js i
        fksOfTable (Nothing:js) i = fksOfTable js i
        fksOfTable [] i = []

-- | Returns columns grouped by their tables. Each column is qualified with alias.
selectColumns :: forall as ts. (ContainsAll (ts :: [*]) (as :: [*]) RecordWrapper)
              => Proxy as -- ^ Types of models from which columns are obtained.
              -> Proxy ts -- ^ Types of models in the same order of aliases.
              -> [String] -- ^ Alisses of tables.
              -> [[String]] -- ^ Column names grouped by their tables.
selectColumns pas pts aliases = mapEach f pts pas (Proxy :: Proxy RecordWrapper) 
    where
        f :: forall a. (RecordWrapper a) => Int -> Proxy a -> [String]
        f index p
            | roleForRelate p = []
            | otherwise       = let expMap = M.fromList $ getExpression (Proxy :: Proxy (RW'Spec a))
                                in map (\n -> maybe ((aliases !! index) ++ "." ++ n) id (expMap M.!? n)) $ fieldNames (Proxy :: Proxy (RW'Type a))

-- ------------------------------------------------------------
-- Edges.
-- ------------------------------------------------------------

-- | A constraint to translate an edge into join clause in the query.
type EdgeForJoin g ms a b rs = (RecordWrapper a, RecordWrapper b, GraphContainer g (EdgeT a b rs), JoinTypeable rs, Contains ms a, Contains ms b)

-- | Join information used in a query.
data Join = Join { leftIndex :: Int -- ^ Index of lhs table.
                 , rightIndex :: Int -- ^ Index of rhs table.
                 , leftTable :: String -- ^ Name of lhs table.
                 , leftAlias :: String -- ^ Alias of lhs table.
                 , rightTable :: String -- ^ Name of rhs table.
                 , rightAlias :: String -- ^ Alias of rhs table.
                 , leftColumn :: String -- ^ Column name of lhs table.
                 , rightColumn :: String -- ^ Column name of rhs table.
                 }

-- | Edge information corresponding to a join clause in q query.
data JoinEdge g (ms :: [*]) = forall a b (rs :: [*]). (GraphContainer g (EdgeT a b rs), JoinTypeable rs, Contains ms a, Contains ms b)
    => JoinEdge { fromProxy :: Proxy a -- ^ Proxy to a model type of lhs table.
                , toProxy :: Proxy b -- ^ Proxy to a model type of rhs table.
                , relationProxy :: Proxy rs -- ^ Proxy to a type from which join type can be decided.
                , getJoin :: Maybe Join -- ^ Get the join information of this edge.
                }

-- | Returns a join type from edge information.
joinTypeOf :: JoinEdge g ms -- ^ Edge information
           -> JoinType -- ^ Join type.
joinTypeOf (JoinEdge _ _ r _) = getJoinType r

instance Show (JoinEdge g ms) where
    show (JoinEdge _ _ _ Nothing) = ""
    show je@(JoinEdge _ _ _ (Just j)) =
        let (t, a1, c1, a2, c2) = if leftIndex j > rightIndex j
                then (leftTable j, leftAlias j, leftColumn j, rightAlias j, rightColumn j)
                else (rightTable j, rightAlias j, rightColumn j, leftAlias j, leftColumn j)
        in jt ++ t ++ " AS " ++ a1 ++ " ON " ++ a1 ++ "." ++ c1 ++ " = " ++ a2 ++ "." ++ c2
        where
            jt = case joinTypeOf je of
                    InnerJoinType -> "INNER JOIN "
                    LeftJoinType -> "LEFT JOIN "
                    RightJoinType -> "RIGHT JOIN "

joinCondition :: JoinEdge g ms
              -> Maybe String
joinCondition (JoinEdge _ _ _ Nothing) = Nothing
joinCondition (JoinEdge _ _ _ (Just j)) = Just $ leftAlias j ++ "." ++ leftColumn j ++ " = " ++ rightAlias j ++ "." ++ rightColumn j

arrangeJoins :: ([JoinEdge g ms], [JoinEdge g ms])
             -> [String]
             -> ([JoinEdge g ms], [JoinEdge g ms])
arrangeJoins (ls, rs) ts = if length ls' == 0
                            then (ls, rs)
                            else arrangeJoins (ls ++ ls', rs') (map (rightTable . fromJust . getJoin) ls')
    where
        (ls', rs') = L.partition (\j -> maybe False (`elem` ts) (leftTable <$> getJoin j)) rs

-- | Declares a method to collect join informations from edges in a graph.
class Joins g edges (ms :: [*]) where
    -- | Collects join informations of edges.
    collectJoins :: (WithDB db)
                 => Proxy edges -- ^ Edges of a graph.
                 -> Proxy ms -- ^ Types of models determining the order of tables.
                 -> [String] -- ^ Aliases of tables.
                 -> IO [JoinEdge g ms] -- ^ Join informations.

instance Joins g '[] ms where
    collectJoins _ _ _ = return []

instance (Joins g edges ms, EdgeForJoin g ms a b rs) => Joins g (EdgeT a b rs ': edges) ms where
    collectJoins _ p aliases = (:) <$> toJoin (Proxy :: Proxy (EdgeT a b rs)) p aliases <*> collectJoins (Proxy :: Proxy edges) p aliases

-- Instances to bind instances of EdgeToJoin specialized for ExtraModel.
instance (Joins g edges ms, EdgeForJoin g ms (ExtraModel xs as) b rs) => Joins g (EdgeT (ExtraModel xs as) b rs ': edges) ms where
    collectJoins _ p aliases = (:) <$> toJoin (Proxy :: Proxy (EdgeT (ExtraModel xs as) b rs)) p aliases <*> collectJoins (Proxy :: Proxy edges) p aliases
instance (Joins g edges ms, EdgeForJoin g ms a (ExtraModel xs as) rs) => Joins g (EdgeT a (ExtraModel xs as) rs ': edges) ms where
    collectJoins _ p aliases = (:) <$> toJoin (Proxy :: Proxy (EdgeT a (ExtraModel xs as) rs)) p aliases <*> collectJoins (Proxy :: Proxy edges) p aliases

-- | Declares a method to convert an edge to a join information.
class EdgeToJoin g e (ms :: [*]) where
    -- | Converts an edge to a join information.
    toJoin :: (WithDB db)
           => Proxy e -- ^ Type of an edge.
           -> Proxy ms -- ^ Types of models determining the order of tables.
           -> [String] -- ^ Aliases of tables.
           -> IO (JoinEdge g ms) -- ^ Join information.

instance (EdgeForJoin g ms a b rs) => EdgeToJoin g (EdgeT a b rs) ms where
    toJoin _ p aliases = do
        ta <- readSchema (getName (Proxy :: Proxy a))
        tb <- readSchema (getName (Proxy :: Proxy b))
        let ia = indexOf 0 p (Proxy :: Proxy a)
        let ib = indexOf 0 p (Proxy :: Proxy b)
        -- If no relations between the pair of tables, runtime error will occur.
        -- TODO:
        -- Available relation between a pair of tables is just the first one.
        let (ca, r) = relationsTo ta (tableName tb) !! 0
        let j = Join ia ib (tableName ta) (aliases !! ia) (tableName tb) (aliases !! ib) ca (referenceColumn r)
        return $ JoinEdge (Proxy :: Proxy a) (Proxy :: Proxy b) (Proxy :: Proxy rs) (Just j)

-- TODO:
-- ExtraModel does not support join query, that is, it can't be a model of subquery.

instance (EdgeForJoin g ms (ExtraModel xs as) b rs) => EdgeToJoin g (EdgeT (ExtraModel xs as) b rs) ms where
    toJoin _ _ _ = return $ JoinEdge (Proxy :: Proxy (ExtraModel xs as)) (Proxy :: Proxy b) (Proxy :: Proxy rs) Nothing
instance (EdgeForJoin g ms a (ExtraModel xs as) rs) => EdgeToJoin g (EdgeT a (ExtraModel xs as) rs) ms where
    toJoin _ _ _ = return $ JoinEdge (Proxy :: Proxy a) (Proxy :: Proxy (ExtraModel xs as)) (Proxy :: Proxy rs) Nothing

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

type family Edges g :: [*] where
    Edges g = Reverse (Edges' g)
type family Edges' g :: [*] where
    Edges' (xs :><: EdgeT a b rs) = EdgeT a b rs ': Edges' xs
    Edges' (xs :><: x) = Edges' xs
    Edges' (EdgeT a b rs) = '[EdgeT a b rs]
    Edges' _ = '[]

type family ArrangeEdges g (a :: *) :: [*] where
    ArrangeEdges g a = a ': ArrangeEdges' '[] (Reverse (Edges' g)) 'True '[a]

type family ArrangeEdges' (acc :: [*]) (edges :: [*]) (from :: Bool) (as :: [*]) :: [*] where
    ArrangeEdges' acc edges from '[] = '[]

    ArrangeEdges' acc (EdgeT a b rs ': edges) 'True (a ': as) = b ': ArrangeEdges' acc edges 'True (a ': b ': as)
    ArrangeEdges' acc (e ': edges) 'True (a ': as) = ArrangeEdges' (e ': acc) edges 'True (a ': as)
    ArrangeEdges' acc '[] 'True (a ': as) = ArrangeEdges' '[] (Reverse acc) 'False (a ': as)

    ArrangeEdges' acc (EdgeT b a rs ': edges) 'False (a ': as) = b ': ArrangeEdges' acc edges 'False (a ': b ': as)
    ArrangeEdges' acc (e ': edges) 'False (a ': as) = ArrangeEdges' (e ': acc) edges 'False (a ': as)
    ArrangeEdges' acc '[] 'False (a ': as) = ArrangeEdges' '[] (Reverse acc) 'True (Reverse as)