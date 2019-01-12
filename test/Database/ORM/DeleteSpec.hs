{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.DeleteSpec where

import Test.Hspec
import Control.Monad.State
import Control.Lens hiding ((:>))
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Model.Graph
import Data.Resource hiding ((.+))
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Delete
import Database.ORM.Condition
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Resource
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type A = "a" :## Record '["aid" :> Int, "cola" :> Int]
type B = "b" :## Record '["bid" :> Int, "colb" :> Int]
type C = "c" :## Record '["cid" :> Int, "colc" :> Int]
type D = "d" :## Record '["did" :> Int, "cold" :> Int]

type WithPK = "withPK" :## Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithPKs = "withPKs" :## Record '["id1" :> Int, "id2" :> Int, "c1" :> Int, "c2" :> String]

type AGraph = Graph A
type ABGraph = Graph A :><: B :><: (B :- A)
type ABCDGraph = Graph A :><: B :><: C :><: D :><: (B :- A) :><: (C :- B) :><: (D :- B)

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" "" False auto []

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relations = [Relation t c] }

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                                  , ("b", TableMeta "b" [col True "bid" False, col False "colb" False, col False "b_a_id" False `rel` ("a", "aid")])
                                  , ("c", TableMeta "c" [col True "cid" False, col False "colc" False, col False "c_b_id" False `rel` ("b", "bid")])
                                  , ("d", TableMeta "d" [col True "did" False, col False "cold" False, col False "d_b_id" False `rel` ("b", "bid")])
                                  , ("withPK", TableMeta "withpk"
                                                        [col True "id" True, col False "c1" False, col False "c2" False])
                                  , ("withPKs", TableMeta "classify"
                                                        [col True "id1" False, col True "id2" False, col False "c1" False, col False "c2" False])
                                  ]
            } 

spec :: Spec
spec = do
    describe "Create delete query" $ do
        it "Delete query by a PK" $ do
            let (q, holder) = pkDeleteQuery (TableMeta "table" []) [ [("id1", toSql (1 :: Int))] ]
            q `shouldBe` "DELETE FROM table WHERE (id1 = ?)"
            holder `shouldBe` [toSql (1 :: Int)]

        it "Delete query by PKs of a record" $ do
            let (q, holder) = pkDeleteQuery (TableMeta "table" []) [ [("id1", toSql (1 :: Int)), ("id2", toSql (2 :: Int))] ]
            q `shouldBe` "DELETE FROM table WHERE (id1 = ? AND id2 = ?)"
            holder `shouldBe` [toSql (1 :: Int), toSql (2 :: Int)]

        it "Delete query by PKs of multiple records" $ do
            let (q, holder) = pkDeleteQuery (TableMeta "table" []) [ [("id1", toSql (1 :: Int)), ("id2", toSql (2 :: Int))]
                                                                   , [("id1", toSql (3 :: Int)), ("id2", toSql (4 :: Int))]
                                                                   , [("id1", toSql (5 :: Int)), ("id2", toSql (6 :: Int))]
                                                                   ]
            q `shouldBe` "DELETE FROM table WHERE (id1 = ? AND id2 = ?) OR (id1 = ? AND id2 = ?) OR (id1 = ? AND id2 = ?)"
            holder `shouldBe` [ toSql (1 :: Int), toSql (2 :: Int)
                              , toSql (3 :: Int), toSql (4 :: Int)
                              , toSql (5 :: Int), toSql (6 :: Int)
                              ]

    describe "Extract pk columns and values to delete" $ do
        it "With a PK" $ do
            let vs = pkColumnsAndValues (tables mock M.! "withPK")
                                        (Model (#id @= 1 <: #c1 @= 2 <: #c2 @= "v" <: emptyRecord) :: WithPK)
            vs `shouldBe` [("id", toSql (1 :: Int))]

        it "With PKs" $ do
            let vs = pkColumnsAndValues (tables mock M.! "withPKs")
                                        (Model (#id1 @= 1 <: #id2 @= 2 <: #c1 @= 3 <: #c2 @= "v" <: emptyRecord) :: WithPKs)
            vs `shouldBe` [("id1", toSql (1 :: Int)), ("id2", toSql (2 :: Int))]

    describe "Create query with relations" $ do
        it "No joins" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            withContext @'[DBContext Mock] resources $ do
                let c = cond @'[A] "#" "#.cola = ?" .+ "abc"
                (q, holder) <- joinDeleteQuery (Proxy :: Proxy AGraph) (Proxy :: Proxy A) c (tables mock M.! "a") ["ta"]

                q `shouldBe` "DELETE FROM a AS ta WHERE ta.cola = ?"
                holder `shouldBe` [toSql "abc"]
            return ()

        it "Delete query on a table having relation to another table" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            withContext @'[DBContext Mock] resources $ do
                let c = cond @'[A] "#" "#.cola = ?" .+ "abc"
                (q, holder) <- joinDeleteQuery (Proxy :: Proxy ABGraph) (Proxy :: Proxy B) c (tables mock M.! "b") ["tb", "ta"]

                q `shouldBe` "DELETE FROM b AS tb USING a AS ta WHERE tb.b_a_id = ta.aid AND (ta.cola = ?)"
                holder `shouldBe` [toSql "abc"]
            return ()

        it "Delete query on a table having relations to many tables" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            withContext @'[DBContext Mock] resources $ do
                let c = (cond @'[A] "#" "#.cola = ?" .+ "abc")
                     .& (cond @'[C, D] "#" "#.colc * 2 = #.cold")
                (q, holder) <- joinDeleteQuery (Proxy :: Proxy ABCDGraph) (Proxy :: Proxy B) c (tables mock M.! "b") ["tb", "ta", "tc", "td"]

                q `shouldBe` "DELETE FROM b AS tb USING a AS ta, c AS tc, d AS td \
                             \WHERE tb.b_a_id = ta.aid AND tc.c_b_id = tb.bid AND td.d_b_id = tb.bid AND ((ta.cola = ?) AND (tc.colc * 2 = td.cold))"
                holder `shouldBe` [toSql "abc"]
            return ()

