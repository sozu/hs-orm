{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.RoutineSpec where

import Test.Hspec
import Data.IORef
import Data.Proxy
import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Model.Graph
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Routine
import Database.ORM.Resource
import Database.ORM.Condition
import Database.ORM.Dialect.Mock

type A = "a" :## Record '["aid" :> Int, "cola" :> String] :^+ PK '["aid"]
type B = "b" :## Record '["bid" :> Int, "colb" :> String] :^+ PK '["bid", "b_cid"] :^+ FK '["b_cid" :> Int]
type C = "c" :## Record '["cid" :> Int, "ckey" :> String, "colc" :> String] :^+ PK '["cid", "ckey"]

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" "" False auto []

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relations = [Relation t c] }

tableMap :: M.Map String TableMeta
tableMap = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                      , ("b", TableMeta "b" [col True "bid" True, col False "colb" False, col False "b_cid" False `rel` ("c", "cid")])
                      , ("c", TableMeta "c" [col True "cid" True, col False "ckey" False, col False "colc" False])
                      ]

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = tableMap
            } 

-- TODO: How to test? Executing queries are not accessible...

spec :: Spec
spec = do
    describe "Fetch one record" $ do
        it "Fetch from a table having a PK" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            let aliases = ["t0"]
            ((q, h), _) <- withContext @'[DBContext Mock] resources $ do
                fetchOne @(Graph A) (1 :: Int)
                conn <- connect <$> readIORef (contextOf @(DBContext Mock) ?cxt)
                latestExecution conn
            q `shouldBe` "SELECT t0.aid, t0.cola FROM a AS t0  WHERE t0.aid = ?"
            h `shouldBe` [toSql (1 :: Int)]

        it "Fetch from a table having a Foreign PK" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            let aliases = ["t0"]
            ((q, h), _) <- withContext @'[DBContext Mock] resources $ do
                fetchOne @(Graph B) (1 :: Int) (2 :: Int)
                conn <- connect <$> readIORef (contextOf @(DBContext Mock) ?cxt)
                latestExecution conn
            q `shouldBe` "SELECT t0.bid, t0.colb FROM b AS t0  WHERE (t0.bid = ?) AND (t0.b_cid = ?)"
            h `shouldBe` [toSql (1 :: Int), toSql (2 :: Int)]

        it "Fetch from a table having multiple PKs" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            let aliases = ["t0"]
            ((q, h), _) <- withContext @'[DBContext Mock] resources $ do
                fetchOne @(Graph C) (1 :: Int) "abc"
                conn <- connect <$> readIORef (contextOf @(DBContext Mock) ?cxt)
                latestExecution conn
            q `shouldBe` "SELECT t0.cid, t0.ckey, t0.colc FROM c AS t0  WHERE (t0.cid = ?) AND (t0.ckey = ?)"
            h `shouldBe` [toSql (1 :: Int), toSql "abc"]
