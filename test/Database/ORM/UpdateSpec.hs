{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.UpdateSpec where

import Test.Hspec
import Control.Monad.State
import Control.Lens hiding ((:>))
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Model.Graph
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Update
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Resource
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type A = "a" :// Record '["aid" :> Int, "cola" :> Int]
type B = "b" :// Record '["bid" :> Int, "colb" :> Int]

type WithPK = "withPK" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithFK = "withFK" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]

type WithPKGraph = Graph WithPK
type WithFKGraph = Graph A :><: B :><: WithFK :><: (WithFK :- A) :><: (WithFK :- B)

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" "" False auto Nothing

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relation = Just (Relation t c) }

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                                  , ("b", TableMeta "b" [col True "bid" False, col False "colb" True])
                                  , ("classify", TableMeta "classify"
                                                        [col True "id1" False, col True "id2" False, col False "col1" False, col False "col2" False])
                                  , ("withPK", TableMeta "withpk"
                                                        [col True "id" True, col False "c1" False, col False "c2" False])
                                  , ("withFK", TableMeta "withfk"
                                                        [ col True "id" True, col False "c1" False, col False "c2" False
                                                        , col False "a_id" False `rel` ("a", "aid"), col False "b_id" False `rel` ("b", "bid")
                                                        ])
                                  ]
            } 

spec :: Spec
spec = do
    describe "Create update query" $ do
        it "Update query" $ do
            let q = updateQuery (TableMeta "table" []) ["id1", "id2"] ["col1", "col2"]
            q `shouldBe` "UPDATE table SET col1 = ?, col2 = ? WHERE id1 = ? AND id2 = ?"

    describe "Extract columns and values to update" $ do
        it "With an incremental column" $ do
            (cursors, g) <- flip runStateT (newGraph :: WithPKGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: WithPK)
            let t = tables mock M.! "withPK"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("id", toSql (1 :: Int)), ("c1", toSql (2 :: Int)), ("c2", toSql "v1")]
                           , [("id", toSql (2 :: Int)), ("c1", toSql (4 :: Int)), ("c2", toSql "v2")]
                           , [("id", toSql (3 :: Int)), ("c1", toSql (6 :: Int)), ("c2", toSql "v3")]
                           ]

        it "With incremental column and foreign keys" $ do
            (cursors, g) <- flip runStateT (newGraph :: WithFKGraph) $ do
                                    as <- forM [1..3] $ \i -> (+<<) (Model (#aid @= i <: #cola @= i*2 <: emptyRecord) :: A)
                                    b <- (+<<) (Model (#bid @= 10 <: #colb @= 2 <: emptyRecord) :: B)
                                    cs <- forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: WithFK)
                                    forM_ (zip cs as) $ \(c, a) -> c -*< a
                                    forM_ cs $ \c -> c -*< b
                                    return cs
            let t = tables mock M.! "withFK"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("id", toSql (1 :: Int)), ("c1", toSql (2 :: Int)), ("c2", toSql "v1"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (1 :: Int))]
                           , [("id", toSql (2 :: Int)), ("c1", toSql (4 :: Int)), ("c2", toSql "v2"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (2 :: Int))]
                           , [("id", toSql (3 :: Int)), ("c1", toSql (6 :: Int)), ("c2", toSql "v3"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (3 :: Int))]
                           ]

    describe "Classify columns by whether the column is primary key or not" $ do
        it "Classify" $ do
            let (pks, others) = classifyColumns (tables mock M.! "classify")
                                                [("id2", toSql "id2"), ("col1", toSql "col1"), ("col2", toSql "col2"), ("id1", toSql "id1")]
            pks `shouldBe` [("id2", toSql "id2"), ("id1", toSql "id1")]
            others `shouldBe` [("col1", toSql "col1"), ("col2", toSql "col2")]