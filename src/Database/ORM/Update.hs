{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}

module Database.ORM.Update (
    -- * Execute update
    updateNodes
    -- * Build update query
    , columnsAndValues
    , classifyColumns
    , updateQuery
) where

import Control.Monad.State
import qualified Data.List as L
import Data.IORef
import Data.Maybe (catMaybes)
import GHC.TypeLits
import Data.Proxy
import Database.HDBC
import Data.Model.Graph
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Record

-- | Update records indicated by cursors.
-- Records to update are indicated by primary keys, thus the table must have primary key column(s).
-- TODO
-- Reject invocation of this function for the table having no primary keys at compile time.
updateNodes :: forall db g a. (WithDB db, GraphContainer g a, RecordWrapper a, EdgeMap g g a)
            => g -- ^ A graph.
            -> [Cursor a] -- ^ Cursors indicating nodes to update.
            -> IO g -- ^ The same graph as the first argument.
updateNodes graph cs = do
    -- Get schema of a.
    ta <- readSchema $ symbolVal (Proxy :: Proxy (RW'Name a))

    vs <- columnsAndValues graph cs ta

    -- Classify a list into the list of primary keys and the list of other columns.
    forM_ (map (classifyColumns ta) vs) $ \(keys, vals) -> update ta keys vals

    return graph

-- | Extracts names and values of columns to insert from cursors.
columnsAndValues :: forall g a. (GraphContainer g a, RecordWrapper a, EdgeMap g g a)
                 => g -- ^ A graph.
                 -> [Cursor a] -- ^ Cursors indicating nodes to insert.
                 -> TableMeta -- ^ Table schema.
                 -> IO ([[(String, SqlValue)]]) -- ^ Each item holds names and values of columns to insert of a cursor.
columnsAndValues graph cs ta = do
    -- Gets list where each item has a name of foreign key column and a function getting its SqlValue from a cursor.
    let foreigns = catMaybes $ mapEdges graph (Proxy :: Proxy a) (Proxy :: Proxy g) (resolveRelations ta) :: [(String, Cursor a -> Maybe SqlValue)]

    -- Gets list of cursors where each item is a list of pairs of column name and its value.
    return $ flip map cs $ \c ->
                -- Collects edges from a and concatenates fields of a model and foreign key columns of the edges.
                let r = getRecord $ c @< graph
                    relValues = map (toSql . ($ c)) $ map snd foreigns
                in zip (recordFields r ++ map fst foreigns) (recordValues r ++ relValues)

-- | Update a record.
update :: (WithDB db)
       => TableMeta -- ^ Table schema.
       -> [(String, SqlValue)] -- ^ A list of pairs holding name and value of primary key columns.
       -> [(String, SqlValue)] -- ^ A list of pairs holding name and value of columns to update.
       -> IO Integer -- ^ The number of updated records.
update t keys cols = do
    context <- readIORef ?db
    let conn = connect context
    let q = updateQuery t (map fst keys) (map fst cols)
    stmt <- prepare conn q
    execute stmt $ (map snd cols) ++ (map snd keys)

-- | Separate list of columns and values into two lists where one corresponds to primary keys and another corresponds to other columns.
classifyColumns :: TableMeta -- ^ Table schema.
                -> [(String, SqlValue)] -- ^ List of columns and values.
                -> ([(String, SqlValue)], [(String, SqlValue)]) -- ^ List classified by whether the column is primary key or not.
classifyColumns t as = let (pks, others) = L.foldl (\(ks, vs) a -> if isPK a then (a:ks, vs) else (ks, a:vs)) ([], []) as
                       in (reverse pks, reverse others)
    where
        isPK a = maybe False isPrimary (getColumn t (fst a))

-- | Create a query for update.
updateQuery :: TableMeta -- ^ TableSchema
            -> [String] -- ^ Names of primary key columns.
            -> [String] -- ^ Names of columns to update.
            -> String -- ^ Query string.
updateQuery t keys cols = "UPDATE " ++ tableName t ++ " SET "
            ++ L.intercalate ", " (L.map (\c -> c ++ " = ?") cols)
            ++ " WHERE "
            ++ L.intercalate " AND " (L.map (\k -> k ++ " = ?") keys)