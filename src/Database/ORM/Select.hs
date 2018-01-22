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

module Database.ORM.Select where
--module Database.ORM.Select (
--    selectNodes
--    , Condition(..)
--    , SortType(..)
--    , OrderBy(..)
--    , LimitOffset
--    , unconditional
--    , unordered
--) where

import GHC.TypeLits
import Control.Applicative
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe, isJust)
import Data.Convertible
import Data.IORef
import Data.Proxy
import Database.HDBC
import Data.Extensible.HList
import Data.Model.Graph
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Condition
import Database.ORM.Utility
import Debug.Trace

-- ------------------------------------------------------------
-- Edges.
-- ------------------------------------------------------------

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

-- | Edge information corresponding to a join in q query.
-- `g` is a type of graph and `t` is a model type from which edges are traced.
data JoinEdge g t = forall a b (rs :: [*]). (GraphContainer g (EdgeT a b rs), JoinTypeable rs, CursorFindable g t a b)
    => JoinEdge { fromProxy :: Proxy a -- ^ Proxy to a model type of lhs table.
                , toProxy :: Proxy b -- ^ Proxy to a model type of rhs table.
                , relationProxy :: Proxy rs -- ^ Proxy to a type from which join type can be decided.
                , getJoin :: Maybe Join -- ^ Get the join information of this edge.
                }

-- | Returns a join type from edge information.
joinTypeOf :: JoinEdge g t -- ^ Edge information
           -> JoinType -- ^ Join type.
joinTypeOf (JoinEdge _ _ r _) = getJoinType r

instance Show (JoinEdge g t) where
    show (JoinEdge _ _ _ Nothing) = ""
    show je@(JoinEdge _ _ _ (Just j)) = jt ++ leftTable j ++ " AS " ++ leftAlias j ++ " ON " ++ leftAlias j ++ "." ++ leftColumn j ++ " = " ++ rightAlias j ++ "." ++ rightColumn j
        where
            jt = case joinTypeOf je of
                    InnerJoinType -> "INNER JOIN "
                    LeftJoinType -> "LEFT JOIN "
                    RightJoinType -> "RIGHT JOIN "

-- ------------------------------------------------------------
-- Select functions.
-- ------------------------------------------------------------

-- | Arranged model types in graph `g` collected by tracing edges from model `a`.
type EdgeTypes g a = ArrangeEdgeTypes (CollectEdges '[a] (Edges g))

{- | A constraint to show the model `a` can be a selecting entry from the graph `g`.

    Constraints in this type synonym have following meanings.
    - All columns of arranged models can be listed with indexed table aliases.
    - `JoinEdge g a` can be obtained from collected edges in `g` by starting from `a`.
    - Values in a row returned by a query correspond to serialized record fields of arranged model list.
    - The cursor of model `a` can be found from cursors of arranged model types.
    They are needed just for the compilation and fulfilled requisitely in typical use of this library. 
-}
type Selectable g a = (KnownNat (Length (EdgeTypes g a)), SelectColumns (EdgeTypes g a) (EdgeTypes g a), Joins g a (CollectEdges '[a] (Edges g)) (EdgeTypes g a), RowParser g (EdgeTypes g a), FindCursor a (EdgeTypes g a))

type family Head (as :: [*]) :: * where
    Head (a ': as) = a

type Selectables g ms = (Joins g (Head ms) (CollectEdges ms (Edges g)) ms, RowParser g ms, FindCursor (Head ms) ms)

{- | Selects rows and constructs a graph of given type.

    Select query is created to get values enough to construct given graph.
    Starting from a model which is specified by second argument,
    model types in the graph is collected and tables of those models are joined according to join informations defined in the graph.
-}
selectNodes :: forall db g a o ts us. (WithDB db, GraphContainer g a, Selectable g a, RecordWrapper a, ElemIndexes ts (EdgeTypes g a), ElemIndexes us (EdgeTypes g a))
            => Proxy g -- ^ Type of a graph.
            -> Proxy a -- ^ Starting type of a model in the graph.
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

    let q = _selectQuery columns (getName pa) joins w o lo
    let holder = whereValues w ++ maybe [] (\(l, o) -> [toSql l, toSql o]) lo

    execSelect pg columns joins modelTypes q holder

{- | Executes a query and constructs a graph holding obtained values.

    You must give a query which conforms to some rules.
    - Column expressions has the same columns as fileds of corresponding model except for foreign key columns.
    - To recever relationships between tables, foreign key column and referenced column must appear in column expressions.
    - The order of tables in column expression must the same as the order of types of models.
select :: forall db g p ms. (WithDB db, Selectables g ms, GenerateColumns p ms)
       => Proxy g
       -> p ms
       -> String
       -> [SqlValue]
       -> IO g
select pg gc query holder = do
    context <- readIORef ?db
    let conn = connect context

    let columns = generateColumns gc
    joins <- collectJoins (Proxy :: Proxy (CollectEdges ms (Edges g))) (Proxy :: Proxy ms) (getAliases gc)

    -- TODO: 
    -- Enables appending values in column expressions to place holders.
    execSelect pg columns joins (Proxy :: Proxy ms) query holder
-}

-- | Executes a query and constructs a graph holding obtained values.
execSelect :: forall db g a ms. (WithDB db, Selectable g a)
           => Proxy g -- ^ Type of a graph.
           -> [[String]] -- ^ List of column names separated by tables.
           -> [JoinEdge g a] -- ^ Join informations.
           -- -> Proxy (ms :: [*]) -- ^ Types of models in the same order as column expresions of the query.
           -> Proxy (EdgeTypes g a)
           -> String -- ^ Query string.
           -> [SqlValue] -- ^ Values for place holders.
           -> IO g -- ^ Constructed graph.
execSelect pg columns joins modelTypes query holder = do
    context <- readIORef ?db

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
-- Graph constructions by querying result.
-- ------------------------------------------------------------

-- | Adds edges corresponding to relations in the graph.
addRelations :: (GraphFactory g, Monad m)
             => HList MaybeCursor (EdgeTypes g t) -- ^ Cursors of model types.
             -> [JoinEdge g t] -- ^ Edge informations of relations in the graph.
             -> StateT g m () -- ^ New state holding modified graph.
addRelations l = mapM_ (addEdge l)

-- | Adds an edge to the graph if cursors of both ends of the edge exist.
addEdge :: (GraphFactory g, Monad m)
        => HList MaybeCursor (EdgeTypes g t) -- ^ Cursors of model types.
        -> JoinEdge g t -- ^ Edge information to add.
        -> StateT g m () -- ^ New state holding modified graph.
addEdge l (JoinEdge f t r _) = maybe (return ()) id $ (-*<) <$> (fmap (r +|) $ findCursor l f) <*> findCursor l t

-- | Constrains to claim that the model types of `a` and `b` both exist in the arranged models.
-- This means that cursor whose underlying model is `a` or `b` will be found.
type CursorFindable g t a b = (FindCursor a (EdgeTypes g t), FindCursor b (EdgeTypes g t))

{- | Declares a method to get a cursor by its type from a list of cursor types.

    The list contains each cursor in Maybe context and it is retained to the result.
    In that context, Nothing means no relational record is found.
-}
class FindCursor a (as :: [*]) where
    findCursor :: HList MaybeCursor as -> Proxy a -> Maybe (Cursor a)

instance FindCursor a '[] where
    findCursor _ _ = Nothing

instance FindCursor a (a ': as) where
    findCursor (c `HCons` cs) _ = getCursor c

instance (FindCursor a as) => FindCursor a (x ': as) where
    findCursor (c `HCons` cs) p = findCursor cs p

-- | New type to declare type level list of `Maybe (Cursor a)`.
newtype MaybeCursor a = MaybeCursor { getCursor :: Maybe (Cursor a) }

-- ------------------------------------------------------------
-- Handling result of query.
-- ------------------------------------------------------------

-- | Declares method to convert result of a query into cursors.
class (GraphFactory g) => RowParser g (as :: [*]) where
    -- | Parses a row and converts values into cursors in the order of given models.
    parseRow :: (WithDB db)
             => Proxy as -- ^ List of model types deciding the type a value should be converted to.
             -> [[(String, SqlValue)]] -- ^ Each element has obtained pairs of column name and value of a table.
             -> StateT g IO (HList MaybeCursor as) -- ^ Returns cursors. Nothing means no relation.

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
            -> StateT g IO (Maybe (Cursor a)) -- ^ Returns cursor. Nothing means all values are SqlNull.
rowToRecord t row
    | L.all (\v -> SqlNull == snd v) row = return Nothing
    | otherwise = do
        let values = let m = M.fromList row in map (m M.!) $ fieldNames (Proxy :: Proxy (RW'Type a))
        let v = newRecord values :: a
        c <- findInGraph t v
        return $ Just c
    where
        fks = map columnName $ filter (\c -> isJust $ relation c) (tableColumns t)

-- | Searches a model which has the same primary key as given model, and if no model is found, inserts the model into the graph.
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

-- | Create selecting query string.
_selectQuery :: (FormattedCondition c, FormattedOrderBy o)
             => [[String]] -- ^ Qualified columns of tables.
             -> String -- ^ Table name.
             -> [JoinEdge g t] -- ^ Join informations.
             -> c -- ^ Conditions.
             -> o -- ^ Sorting informations.
             -> LimitOffset -- ^ Limit and offset values if needed.
             -> String -- ^ Created query string.
_selectQuery cols t joins conds sorts lo = s ++ f ++ w ++ o ++ (maybe "" (\(l, o) -> " LIMIT ? OFFSET ?") lo)
    where
        s = "SELECT " ++ L.intercalate ", " (L.concat cols)
        f = " FROM " ++ t ++ " AS t0 " ++ L.intercalate " " (filter (/= "") $ map show joins)
        w = let w' = whereClause conds
            in if w' == "" then "" else " WHERE " ++ w'
        o = let o' = orders sorts
            in if o' == "" then "" else " ORDER BY " ++ o'

-- | Obtains column names and join informations to construct the graph starting from a model.
columnsAndTables :: forall db g a. (WithDB db, GraphContainer g a, Selectable g a)
                 => Proxy g -- ^ Type of graph.
                 -> Proxy a -- ^ Type of a model treated as the starting point of the graph.
                 -> [String] -- ^ Aliases of tables arranged in the same order as first argument.
                 -> IO ([[String]], [JoinEdge g a]) -- ^ Column names of the tables and join informations.
columnsAndTables graph p aliases = do
    let pts = Proxy :: Proxy (EdgeTypes g a)
    joins <- collectJoins (Proxy :: Proxy (CollectEdges '[a] (Edges g))) pts aliases
    let columns = selectColumns pts pts aliases
    return ([cs ++ fksOfTable (map getJoin joins) i | (cs, i) <- zip columns [0..]], joins)
    where
        fksOfTable :: [Maybe Join] -> Int -> [String]
        fksOfTable (Just j:js) i = if leftIndex j == i
                                    then (leftAlias j ++ "." ++ leftColumn j) : fksOfTable js i
                                    else fksOfTable js i
        fksOfTable (Nothing:js) i = fksOfTable js i
        fksOfTable [] i = []

-- | Declares a method to collect column names of tables represented with types of models.
class SelectColumns (as :: [*]) (ts :: [*]) where
    -- | Collects column names of tables in certain order.
    selectColumns :: Proxy as -- ^ Types of models.
                  -> Proxy ts -- ^ Types of models determining the order of tables.
                  -> [String] -- ^ Aliases of tables.
                  -> [[String]] -- ^ Column names of tables arranged in the same order as the second argument.

instance SelectColumns '[] ts where
    selectColumns _ _ _ = []

instance (RecordWrapper a, SelectColumns as ts, KnownNat (ElemIndex a ts)) => SelectColumns (a ': as) ts where
    selectColumns _ pts aliases = [(aliases !! i) ++ "." ++ c | c <- cols] : selectColumns (Proxy :: Proxy as) pts aliases
        where
            i = fromInteger $ natVal (Proxy :: Proxy (ElemIndex a ts))
            cols = fieldNames (Proxy :: Proxy (RW'Type a))

-- | Declares a method to collect join informations.
class Joins g t edges (as :: [*]) where
    -- | Collects join informations of edges.
    collectJoins :: (WithDB db)
                 => Proxy edges -- ^ Edges of a graph.
                 -> Proxy as -- ^ Types of models determining the order of tables.
                 -> [String] -- ^ Aliases of tables.
                 -> IO [JoinEdge g t] -- ^ Join informations.

instance Joins g t '[] as where
    collectJoins _ _ _ = return []

instance (Joins g t edges as, RecordWrapper a, RecordWrapper b, GraphContainer g (EdgeT a b rs), KnownNat (ElemIndex a as), KnownNat (ElemIndex b as), JoinTypeable rs, CursorFindable g t a b) => Joins g t (EdgeT a b rs ': edges) as where
    collectJoins _ p aliases = (:) <$> toJoin (Proxy :: Proxy (EdgeT a b rs)) p aliases <*> collectJoins (Proxy :: Proxy edges) p aliases

-- | Declares a method to convert an edge to a join information.
class EdgeToJoin g t e (as :: [*]) where
    -- | Converts an edge to a join information.
    toJoin :: (WithDB db)
           => Proxy e -- ^ Type of an edge.
           -> Proxy as -- ^ Types of models determining the order of tables.
           -> [String] -- ^ Aliases of tables.
           -> IO (JoinEdge g t) -- ^ Join information.

instance (RecordWrapper a, RecordWrapper b, GraphContainer g (EdgeT a b rs), KnownNat (ElemIndex a as), KnownNat (ElemIndex b as), JoinTypeable rs, CursorFindable g t a b) => EdgeToJoin g t (EdgeT a b rs) as where
    toJoin _ _ aliases = do
        ta <- readSchema (getName (Proxy :: Proxy a))
        tb <- readSchema (getName (Proxy :: Proxy b))
        let ia = fromInteger $ natVal (Proxy :: Proxy (ElemIndex a as)) :: Int
        let ib = fromInteger $ natVal (Proxy :: Proxy (ElemIndex b as)) :: Int
        -- If no relations between the pair of tables, runtime error will occur.
        -- TODO:
        -- Available relation between a pair of tables is just the first one.
        let (ca, r) = relationsTo ta (tableName tb) !! 0
        let j = Join ia ib (tableName ta) (aliases !! ia) (tableName tb) (aliases !! ib) ca (referenceColumn r)
        return $ JoinEdge (Proxy :: Proxy a) (Proxy :: Proxy b) (Proxy :: Proxy rs) (Just j)

-- TODO:
-- ExtraModel does not support join query, that is, it can't be a model of subquery.

instance (GraphContainer g (EdgeT (ExtraModel xs) b rs), JoinTypeable rs, CursorFindable g t (ExtraModel xs) b) => EdgeToJoin g t (EdgeT (ExtraModel xs) b rs) as where
    toJoin _ _ _ = return $ JoinEdge (Proxy :: Proxy (ExtraModel xs)) (Proxy :: Proxy b) (Proxy :: Proxy rs) Nothing

instance (GraphContainer g (EdgeT a (ExtraModel xs) rs), JoinTypeable rs, CursorFindable g t a (ExtraModel xs)) => EdgeToJoin g t (EdgeT a (ExtraModel xs) rs) as where
    toJoin _ _ _ = return $ JoinEdge (Proxy :: Proxy a) (Proxy :: Proxy (ExtraModel xs)) (Proxy :: Proxy rs) Nothing

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

-- | Create reversed type level list.
type family Reverse (as :: [*]) :: [*]
type instance Reverse '[] = '[]
type instance Reverse (a ': as) = Append a (Reverse as)

-- | Create a type level list holding types of models in the order they appear in a edge list.
type family ArrangeEdgeTypes (edges :: [*]) :: [*]
type instance ArrangeEdgeTypes '[] = '[]
type instance ArrangeEdgeTypes (EdgeT a b rs : edges) = '[b, a] +++ ArrangeEdgeTypes edges

-- | Add a type into a type level set.
type family AddSet (a :: k) (as :: [k]) :: [k] where
    AddSet a '[] = '[a]
    AddSet a (a ': as) = a ': as
    AddSet a (x ': as) = x ': AddSet a as

-- | Collects edges associated with given types.
type family CollectEdges (as :: [*]) (edges :: [k]) :: [k] where
    CollectEdges as '[] = '[]
    CollectEdges '[] edges = edges
    CollectEdges (a ': as) edges = ResolveEdges a edges
                               +++ CollectEdges (EdgesToValue a (ResolveEdges a edges) +++ as) (edges /// ResolveEdges a edges)

-- | Collects types each of which makes an edge in the edge list by paired with given type.
type family EdgesToValue (a :: *) (e :: [k]) :: [*] where
    EdgesToValue a '[] = '[]
    EdgesToValue a ((Edge b a) ': edges) = b ': EdgesToValue a edges
    EdgesToValue a ((Edge a b) ': edges) = b ': EdgesToValue a edges
    EdgesToValue a (x ': edges) = EdgesToValue a edges

-- | Collects edges whose one endpoint is a given type.
type family ResolveEdges a (edges :: [k]) :: [k]
type instance ResolveEdges a '[] = '[]
type instance ResolveEdges a (e ': edges) = EdgeFrom' a (e ': edges) +++ EdgeTo' a (e ': edges)

-- | Concatenates two type level list into a type level set.
type family (+++) (as :: [k]) (bs :: [k]) :: [k] where
    (+++) '[] bs = bs
    (+++) as (b ': bs) = AddSet b as +++ bs
    (+++) as '[] = as

-- | Removes types from a type level set.
type family (///) (as :: [k]) (bs :: [k]) :: [k] where
    (///) '[] bs = '[]
    (///) as '[] = as
    (///) (Edge a b ': as) (Edge a b ': bs) = as /// bs
    (///) as '[x] = as
    (///) as (x ': bs) = (as /// '[x]) /// bs

-- | Extracts edges which get out of a type.
type family EdgeFrom' a (edges :: [*]) :: [*] where
    EdgeFrom' a (EdgeT a b rs ': edges) = EdgeT a b rs ': EdgeFrom' a edges
    EdgeFrom' a (x ': edges) = EdgeFrom' a edges
    EdgeFrom' a '[] = '[]

-- | Extracts edges which end at a type.
type family EdgeTo' a (edges :: [*]) :: [*] where
    EdgeTo' a (EdgeT b a rs ': edges) = EdgeT b a rs ': EdgeTo' a edges
    EdgeTo' a (x ': edges) = EdgeTo' a edges
    EdgeTo' a '[] = '[]

-- | Gets edges in a graph.
type family Edges g :: [*] where
    Edges (xs :><: EdgeT a b rs) = Append (EdgeT a b rs) (Edges xs)
    Edges (xs :><: x) = Edges xs
    Edges (EdgeT a b rs) = '[ EdgeT a b rs]
    Edges _ = '[]
