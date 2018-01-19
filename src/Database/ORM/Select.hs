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
import Data.Maybe (maybe)
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
                , getJoin :: Join -- ^ Get the join information of this edge.
                }

-- | Returns a join type from edge information.
joinTypeOf :: JoinEdge g t -- ^ Edge information
           -> JoinType -- ^ Join type.
joinTypeOf (JoinEdge _ _ r _) = getJoinType r

instance Show (JoinEdge g t) where
    show je = let j = getJoin je in jt ++ leftTable j ++ " AS " ++ leftAlias j ++ " ON " ++ leftAlias j ++ "." ++ leftColumn j ++ " = " ++ rightAlias j ++ "." ++ rightColumn j
        where
            jt = case joinTypeOf je of
                    InnerJoinType -> "INNER JOIN "
                    LeftJoinType -> "LEFT JOIN "
                    RightJoinType -> "RIGHT JOIN "

-- ------------------------------------------------------------
-- Query elements.
-- ------------------------------------------------------------

-- TODO
-- テーブルとカラムを指定することで、t0.columnの文字列に落とせる方法。
--
-- data ColumnLabel = ColumnLabel String
-- 
-- instance (KnownSymbol l) => IsLabel l ColumnLabel where
--     fromLabel = ColumnLabel (symbolVal (Proxy :: Proxy l))
-- 
-- -- eq @News.Model #title "abc"
-- eq_ :: (RecordWrapper a, SqlValueConstraint v, Condition c)
--     => ColumnLabel
--     -> v
--     -> c
-- eq_ (ColumnLabel n) p value = (n, [value])
--
-- TODO
-- SQLを与えて実行してからグラフに落とす方法。
-- q <- "SELECT a.id, count(b.*), (SELECT c FROM C WHERE a_id = a.id) FROM A as a INNER JOIN B as b ON a.b_id = b.id WHERE ..."
-- カラムの並びに対応させる型レベルリストが必要。
-- カラムの並びと完全に揃えなければならないため、生のSQLは禁止。
-- select [cols :: A, cols :: B, ...]
-- カラムでは無い値を文字列で書きたい場合、
-- select [cols :: A ++ ["count(b.*)", "(SELECT c FROM C WHERE a_id = a.id)"], cols :: B, ...]
-- "SELECT " ++ cols @A ["COUNT(b.*)", "(SELECT...)"]
-- とすると、Aのextra fieldsの数と追加のカラムの数をチェックできる？
-- ExtraだけのRecordWrapperがあると、それはreadSchemaでこける。
-- ただしTableMetaはprimary keyによる同一レコードチェックにしか利用していないため、Maybeなどに変えても問題はなさそう。
-- 1) select (cols graph ++ cols extras)として全行をextrasに対応させる方法
-- 2) RecordWrapperにextra fieldを与えて、関連を維持しつつ取ってくる方法
-- まとめられそう。どっちもRecordWrapperにしてExtraだけのRecordWrapperも利用できるようにする？
-- FROM節のエイリアスとの関連はどうするか。
-- [("t0", cols @A), ("t1", cols @B), ("t2", cols @Extra)]
-- これを与えてさらにグラフも与えれば、B -> Aの関連を表すエッジも追加される。
-- つまり、selectNodesではカラムの並びが完全にグラフから決められたが、
-- 並びを明示的に与える関数を作成すれば、グラフから決めた場合に取り除かれた関連のない独立モデルも利用することができる。

class ColumnRepr c (as :: [*]) where
    resolveColumn :: Proxy as
                  -> [String]
                  -> c
                  -> String

instance ColumnRepr String as where
    resolveColumn _ _ c = c

data TableColumn a = TableColumn (Proxy a) String

instance (KnownNat (ElemIndex a as)) => ColumnRepr (TableColumn a) as where
    resolveColumn _ aliases (TableColumn t c) =
        let index = fromInteger $ natVal (Proxy :: Proxy (ElemIndex a as))
        in (aliases !! index) ++ "." ++ c

-- | Declares methods to get where clause and values for placeholders in it.
class Condition c where
    -- | Gets the string of where clause.
    whereClause :: c -- ^ Condition.
                -> String -- ^ Where clause.

    -- | Gets values for placeholders.
    whereValues :: c -- ^ Condition.
                -> [SqlValue] -- ^ Values for placeholders.

instance (Convertible v SqlValue) => Condition (String, [v]) where
    whereClause (c, _) = c
    whereValues (_, vs) = map toSql vs

-- | Gets an empty condition.
unconditional = [] :: [(String, [Int])]

-- | Predefined sorting orders.
data SortType = ASC -- ^ Ascending order.
              | DESC -- ^ Descending order.
              deriving (Show)

-- | Declares methods to get column name for sort and the order.
class OrderBy o where
    -- | Gets a column name used for sorting.
    sortColumn :: o -- ^ Sorting information.
               -> String -- ^ Column name.

    -- | Gets sorting order.
    sortType :: o -- ^ Sorting information.
             -> SortType -- ^ Sorting order.

-- | Gets default sorting.
unordered = [] :: [(String, SortType)]

instance OrderBy (String, SortType) where
    sortColumn (c, _) = c
    sortType (_, t) = t

-- | Type synonym where each value corresponds to LIMIT and OFFSET value respectively.
type LimitOffset = Maybe (Int, Int)

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

{- | Selects rows and constructs a graph of given type.

    Select query is created to get values enough to construct given graph.
    Starting from a model which is specified by second argument,
    model types in the graph is collected and tables of those models are joined according to join informations defined in the graph.
-}
selectNodes :: forall db g a c o. (WithDB db, GraphContainer g a, Selectable g a, RecordWrapper a, Condition c, OrderBy o)
            => Proxy g -- ^ Type of a graph.
            -> Proxy a -- ^ Starting type of a model in the graph.
            -> [c] -- ^ Conditions.
            -> [o] -- ^ Sorting informations.
            -> LimitOffset -- ^ Limit and offset values if needed.
            -> IO g -- ^ Graph containing nodes and edges representing the result of query.
selectNodes pg pa conds sorts lo = do
    let n = natVal (Proxy :: Proxy (Length (EdgeTypes g a)))
    let aliases = map (\i -> 't':show i) [0..n-1]

    (cols, joins) <- columnsAndTables pg pa aliases

    let q = _selectQuery cols (getName pa) joins conds sorts lo

    context <- readIORef ?db 
    let conn = connect context

    stmt <- prepare conn (trace q q)
    execute stmt $ (concat $ map whereValues conds) ++ maybe [] (\(l, o) -> [toSql l, toSql o]) lo
    rows <- fetchAllRows stmt

    (_, graph) <- flip runStateT (newGraph :: g) $ do
                    forM_ rows $ \r -> do
                        cs <- _parseRow (Proxy :: Proxy (EdgeTypes g a)) $ sep r cols -- HList MaybeCursor (EdgeTypes g a)
                        _addRelations cs joins

    return graph
    where
        sep as [] = []
        sep as (bs:bss) = let c = length bs in take c as : sep (drop c as) bss

-- ------------------------------------------------------------
-- Graph constructions by querying result.
-- ------------------------------------------------------------

-- | Adds edges corresponding to relations in the graph.
_addRelations :: (GraphFactory g, Monad m)
              => HList MaybeCursor (EdgeTypes g t) -- ^ Cursors of model types.
              -> [JoinEdge g t] -- ^ Edge informations of relations in the graph.
              -> StateT g m () -- ^ New state holding modified graph.
_addRelations l = mapM_ (_addEdge l)

-- | Adds an edge to the graph if cursors of both ends of the edge exist.
_addEdge :: (GraphFactory g, Monad m)
         => HList MaybeCursor (EdgeTypes g t) -- ^ Cursors of model types.
         -> JoinEdge g t -- ^ Edge information to add.
         -> StateT g m () -- ^ New state holding modified graph.
_addEdge l (JoinEdge f t r _) = maybe (return ()) id $ (-*<) <$> (fmap (r +|) $ _findCursor l f) <*> _findCursor l t

-- | Constrains to claim that the model types of `a` and `b` both exist in the arranged models.
-- This means that cursor whose underlying model is `a` or `b` will be found.
type CursorFindable g t a b = (FindCursor a (EdgeTypes g t), FindCursor b (EdgeTypes g t))

{- | Declares a method to get a cursor by its type from a list of cursor types.

    The list contains each cursor in Maybe context and it is retained to the result.
    In that context, Nothing means no relational record is found.
-}
class FindCursor a (as :: [*]) where
    _findCursor :: HList MaybeCursor as -> Proxy a -> Maybe (Cursor a)

instance FindCursor a '[] where
    _findCursor _ _ = Nothing

instance FindCursor a (a ': as) where
    _findCursor (c `HCons` cs) _ = getCursor c

instance (FindCursor a as) => FindCursor a (x ': as) where
    _findCursor (c `HCons` cs) p = _findCursor cs p

-- | New type to declare type level list of `Maybe (Cursor a)`.
newtype MaybeCursor a = MaybeCursor { getCursor :: Maybe (Cursor a) }

-- ------------------------------------------------------------
-- Handling result of query.
-- ------------------------------------------------------------

-- | Declares method to convert result of a query into cursors.
class (GraphFactory g) => RowParser g (as :: [*]) where
    -- | Parses a row and converts values into cursors in the order of given models.
    _parseRow :: (WithDB db)
              => Proxy as -- ^ List of model types deciding the type a value should be converted to.
              -> [[SqlValue]] -- ^ Each element has obtained values of a table.
              -> StateT g IO (HList MaybeCursor as) -- ^ Returns cursors. Nothing means no relation.

instance (GraphFactory g) => RowParser g '[] where
    _parseRow _ _ = return HNil

instance (RowParser g as, GraphContainer g a, RecordWrapper a) => RowParser g (a ': as) where
    _parseRow _ (vs:values) = do
        t <- liftIO $ readSchema $ getName (Proxy :: Proxy a)
        c <- _rowToRecord t vs :: StateT g IO (Maybe (Cursor a))
        cs <- _parseRow (Proxy :: Proxy as) values
        return $ MaybeCursor c `HCons` cs

-- | Converts values in a row into a cursor of given table.
_rowToRecord :: forall g a. (GraphContainer g a, RecordWrapper a)
             => TableMeta -- ^ Table schema.
             -> [SqlValue] -- ^ Values in a row.
             -> StateT g IO (Maybe (Cursor a)) -- ^ Returns cursor. Nothing means all values are SqlNull.
_rowToRecord t row
    | L.all (SqlNull ==) row = return Nothing
    | otherwise = do
        let v = newRecord row :: a
        c <- _findInGraph t v
        return $ Just c

-- | Searches a model which has the same primary key as given model, and if no model is found, inserts the model into the graph.
_findInGraph :: (GraphContainer g a, RecordWrapper a, Monad m)
             => TableMeta -- ^ Table schema.
             -> a -- ^ Model to insert if the same model is not found.
             -> StateT g m (Cursor a) -- ^ Returns the cursor to the model which is found or inserted.
_findInGraph t v = do
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
_selectQuery :: (Condition c, OrderBy o)
             => [[String]] -- ^ Qualified columns of tables.
             -> String -- ^ Table name.
             -> [JoinEdge g t] -- ^ Join informations.
             -> [c] -- ^ Conditions.
             -> [o] -- ^ Sorting informations.
             -> LimitOffset -- ^ Limit and offset values if needed.
             -> String -- ^ Created query string.
_selectQuery cols t joins conds orders lo = s ++ f ++ w ++ o ++ (maybe "" (\(l, o) -> " LIMIT ? OFFSET ?") lo)
    where
        s = "SELECT " ++ L.intercalate ", " (L.concat cols)
        f = " FROM " ++ t ++ " AS t0 " ++ L.intercalate " " (map show joins)
        w = if length conds > 0
                then " WHERE " ++ L.intercalate " AND " ['(' : whereClause c ++ ")" | c <- conds]
                else ""
        o = if length orders > 0
                then " ORDER BY " ++ L.intercalate ", " [sortColumn o ++ " " ++ show (sortType o) | o <- orders]
                else ""

-- | Obtains column names and join informations to construct the graph starting from a model.
columnsAndTables :: forall db g a. (WithDB db, GraphContainer g a, Selectable g a)
                 => Proxy g -- ^ Type of graph.
                 -> Proxy a -- ^ Type of a model treated as the starting point of the graph.
                 -> [String] -- ^ Aliases of tables arranged in the same order as first argument.
                 -> IO ([[String]], [JoinEdge g a]) -- ^ Column names of the tables and join informations.
columnsAndTables graph p aliases = do
    let pts = Proxy :: Proxy (EdgeTypes g a)
    joins <- collectJoins (Proxy :: Proxy (CollectEdges '[a] (Edges g))) pts aliases
    let columns = _selectColumns pts pts aliases
    return ([cs ++ fksOfTable (map getJoin joins) i | (cs, i) <- zip columns [0..]], joins)
    where
        fksOfTable :: [Join] -> Int -> [String]
        fksOfTable (j:js) i = if leftIndex j == i
                                then (leftAlias j ++ "." ++ leftColumn j) : fksOfTable js i
                                else fksOfTable js i
        fksOfTable [] i = []

-- | Declares a method to collect column names of tables represented with types of models.
class SelectColumns (as :: [*]) (ts :: [*]) where
    -- | Collects column names of tables in certain order.
    _selectColumns :: Proxy as -- ^ Types of models.
                   -> Proxy ts -- ^ Types of models determining the order of tables.
                   -> [String] -- ^ Aliases of tables.
                   -> [[String]] -- ^ Column names of tables arranged in the same order as the second argument.

instance SelectColumns '[] ts where
    _selectColumns _ _ _ = []

instance (RecordWrapper a, SelectColumns as ts, KnownNat (ElemIndex a ts)) => SelectColumns (a ': as) ts where
    _selectColumns _ pts aliases = [(aliases !! i) ++ "." ++ c | c <- cols] : _selectColumns (Proxy :: Proxy as) pts aliases
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
        -- TODO
        -- Available relation between a pair of tables is just the first one.
        let (ca, r) = relationsTo ta (tableName tb) !! 0
        let j = Join ia ib (tableName ta) (aliases !! ia) (tableName tb) (aliases !! ib) ca (referenceColumn r)
        return $ JoinEdge (Proxy :: Proxy a) (Proxy :: Proxy b) (Proxy :: Proxy rs) j

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

-- | Gets a type at an index of a type level list.
type family ElemOf (i :: Nat) (as :: [*]) :: * where
    ElemOf 0 (a ': as) = a
    ElemOf i (a ': as) = ElemOf (i - 1) as

-- | Gets an index of a type in a type level list.
type family ElemIndex (a :: *) (as :: [*]) :: Nat where
    ElemIndex a (a ': as) = 0
    ElemIndex a (x ': as) = 1 + (ElemIndex a as)

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

-- | Appends a type to a type level list.
type family Append a as :: [k] where
    Append x '[] = '[x]
    Append x (y ': ys) = y ': Append x ys

type family Length (as :: [*]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs