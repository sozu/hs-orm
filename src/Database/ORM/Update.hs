{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}

module Database.ORM.Update (
    updateNodes
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

updateNodes :: forall db g a. (WithDB db, GraphContainer g a, RecordWrapper a, EdgeMap g g a)
            => g
            -> [Cursor a]
            -> IO g
updateNodes graph cs = do
    -- Get schema of a.
    ta <- readSchema $ symbolVal (Proxy :: Proxy (RW'Name a))

    let edgeProxies = Proxy :: Proxy g

    -- Get foreign key informations. :: [(column name, function to get value of referenced column), ...]
    let foreigns = catMaybes $ mapEdges graph (Proxy :: Proxy a) edgeProxies (resolveRelations ta) :: [(String, Cursor a -> Maybe SqlValue)]

    -- Generate column and value pairs for cursors.
    -- A cursor corresponds to a list of pairs, therefore, nested list is generated.
    -- :: [[for cursor1 (column name, value), ...], [for cursor2 (), ...], ...]
    let vs = flip map cs $ \c ->
                let r = getRecord $ c @< graph
                    relValues = map (toSql . ($ c)) $ map snd foreigns
                in zip (recordFields r ++ map fst foreigns) (recordValues r ++ relValues)

    -- Classify a list into the list of primary keys and the list of other columns.
    forM_ (map (_classifyColumns ta) vs) $ \(keys, vals) -> update ta keys vals

    return graph

-- | Update a record.
update :: (WithDB db)
       => TableMeta -- ^ Table schema.
       -> [(String, SqlValue)] -- ^ A list of pairs holding name and value of primary key columns.
       -> [(String, SqlValue)] -- ^ A list of pairs holding name and value of columns to update.
       -> IO Integer
update t keys cols = do
    context <- readIORef ?db
    let conn = connect context
    let q = _updateQuery t (map fst keys) (map fst cols)
    stmt <- prepare conn q
    execute stmt $ (map snd cols) ++ (map snd keys)

_classifyColumns :: TableMeta
                 -> [(String, SqlValue)]
                 -> ([(String, SqlValue)], [(String, SqlValue)])
_classifyColumns t as = L.foldl (\(ks, vs) a -> if isPK a then (a:ks, vs) else (ks, a:vs)) ([], []) as
    where
        isPK a = maybe False isPrimary (getColumn t (fst a))

_updateQuery :: TableMeta -- ^ TableSchema
             -> [String] -- ^ Names of primary key columns.
             -> [String] -- ^ Names of columns to update.
             -> String
_updateQuery t keys cols = "UPDATE " ++ tableName t ++ " SET "
            ++ L.intercalate ", " (L.map (\c -> c ++ " = ?") cols)
            ++ " WHERE"
            ++ L.intercalate " AND " (L.map (\k -> k ++ " = ?") keys)