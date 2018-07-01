{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.ORM.Insert (
    -- * Execute insertion
    insertNodes
    -- * Build insert query
    , columnsAndValues
    , swapAutoIncrementalValue
) where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.IORef
import Data.Maybe (catMaybes, maybe)
import GHC.TypeLits
import Data.Proxy
import Database.HDBC
import Data.Model.Graph
import Data.Resource
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Record

-- | Inserts nodes in a graph into database.
insertNodes :: forall db g a. (WithDB db, GraphContainer g a, RecordWrapper a, EdgeMap g g a)
            => g -- ^ A graph.
            -> [Cursor a] -- ^ Cursors indicating nodes to insert.
            -> IO g -- ^ Graph where inserted models may be changed by being given newly assigned values such as auto incremented values.
insertNodes graph cs = do
    -- Gets table schema of `a`.
    ta <- readSchema $ symbolVal (Proxy :: Proxy (RW'Name a))

    vs <- columnsAndValues graph cs ta

    sequences <- insert ta (map fst (vs !! 0)) (map (\vs' -> map snd vs') vs)

    return $ maybe graph (swapAutoIncrementalValue graph ta cs) sequences

-- | Extracts names and values of columns to insert from cursors.
columnsAndValues :: forall g a. (GraphContainer g a, RecordWrapper a, EdgeMap g g a)
                 => g -- ^ A graph.
                 -> [Cursor a] -- ^ Cursors indicating nodes to insert.
                 -> TableMeta -- ^ Table schema.
                 -> IO ([[(String, ColumnValue)]]) -- ^ Each item holds names and values of columns to insert of a cursor.
columnsAndValues graph cs ta = do
    -- Gets list where each item has a name of foreign key column and a function getting its SqlValue from a cursor.
    let foreigns = catMaybes $ mapEdges graph (Proxy :: Proxy a) (Proxy :: Proxy g) (resolveRelations ta) :: [(String, Cursor a -> Maybe SqlValue)]

    let expressions = M.fromList $ getExpression (Proxy :: Proxy (RW'Spec a))

    -- Gets list of cursors where each item is a list of pairs of column name and its value.
    return $ flip map cs $ \c ->
                -- Collects edges from a and concatenates fields of a model and foreign key columns of the edges.
                let r = getRecord $ c @< graph
                    relValues = map (toSql . ($ c)) $ map snd foreigns
                in applyExp expressions $ removeAuto $ zip (recordFields r ++ map fst foreigns) (map ValueOf (recordValues r ++ relValues))
    where
        -- Function to remove auto increment column from the list of columns to insert.
        removeAuto :: [(String, ColumnValue)] -> [(String, ColumnValue)]
        removeAuto as = case L.findIndex isAutoIncrement (tableColumns ta) of
                                Just i -> take i as ++ drop (i+1) as
                                Nothing -> as

        applyExp :: M.Map String String -> [(String, ColumnValue)] -> [(String, ColumnValue)]
        applyExp expMap as = let app (c, cv) = (c, maybe cv RawExpression (expMap M.!? c))
                             in map app as

-- | Swaps auto incremented values of inserted models.
swapAutoIncrementalValue :: (GraphContainer g a, RecordWrapper a)
                         => g -- ^ Graph.
                         -> TableMeta -- ^ Table schema.
                         -> [Cursor a] -- ^ Cursors to inserted models.
                         -> [Int] -- ^ Auto incremented values generated by the insertion.
                         -> g -- ^ Graph where values of auto incremented columns are modified.
swapAutoIncrementalValue graph t cs vs = snd $ (`runState` graph) $ do
    let autoCol = L.find isAutoIncrement (tableColumns t)
    case autoCol of
        Just ac -> forM_ (zip cs vs) $ \(c, v) ->
                    let m = c @< graph
                        r = setFieldValue (getRecord m) (columnName ac) (toSql v)
                    in (updateRecord m r) /<< c
        Nothing -> return ()

-- | Inserts records into database.
insert :: forall db. (WithDB db)
       => TableMeta -- ^ Table schema.
       -> [String] -- ^ Column names.
       -> [[ColumnValue]] -- ^ Column values of records.
       -> IO (Maybe [Int]) -- ^ Returns the list of generated auto incremented values if any.
insert _ _ [] = return Nothing
insert t cols vs = do
    context <- readIORef $ contextOf @(DBContext db) ?cxt
    let conn = connect context
    -- TODO:
    -- The number of records one insertion can handle at maximum should be configurable.
    forM_ (seg 1000 vs) (insert_ conn t cols)

    case L.find isAutoIncrement (tableColumns t) of
        Just c -> do
            dialect <- getDialect
            sequences <- readLatestSequences dialect c (length vs)
            return $ Just sequences
        Nothing -> return Nothing
    where
        seg n [] = []
        seg n as = take n as : seg n (drop n as)

-- | Executes an insertion.
insert_ :: (WithDB db, IConnection c)
        => c -- ^ Database connection.
        -> TableMeta -- ^ Table schema.
        -> [String] -- ^ Column names.
        -> [[ColumnValue]] -- ^ Column values of records.
        -> IO Integer -- ^ Returns the number of records inserted actually.
insert_ conn t cols vs = do
    dialect <- getDialect
    let (q, hs) = multiInsertQuery dialect t cols vs

    $(logQD' loggerTag) ?cxt $ "SQL: " ++ q ++ "...(" ++ show (length vs) ++ " holders)"

    stmt <- prepare conn (q ++ hs)
    execute stmt $ catMaybes $ map toSqlValue $ concat vs