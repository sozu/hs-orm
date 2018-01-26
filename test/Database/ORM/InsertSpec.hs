{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.InsertSpec where

import Test.Hspec
import Control.Monad.State
import Control.Lens hiding ((:>))
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Model.Graph
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Insert
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type A = "a" :++ Record '["aid" :> Int, "cola" :> Int]
type B = "b" :++ Record '["bid" :> Int, "colb" :> Int]

type NoKey = "nokey" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithAuto = "withAuto" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithFK = "withFK" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]

type NoKeyGraph = Graph NoKey
type WithAutoGraph = Graph WithAuto
type WithFKGraph = Graph A :><: B :><: WithFK :><: (WithFK :- A) :><: (WithFK :- B)

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" False auto Nothing

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relation = Just (Relation t c) }

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                                  , ("b", TableMeta "b" [col True "bid" False, col False "colb" True])
                                  , ("nokey", TableMeta "nokey"
                                                        [col True "id" False, col False "c1" False, col False "c2" False])
                                  , ("withAuto", TableMeta "withpk"
                                                        [col True "id" True, col False "c1" False, col False "c2" False])
                                  , ("withFK", TableMeta "withfk"
                                                        [ col True "id" True, col False "c1" False, col False "c2" False
                                                        , col False "a_id" False `rel` ("a", "aid"), col False "b_id" False `rel` ("b", "bid")
                                                        ])
                                  ]
            } 

spec :: Spec
spec = do
    describe "Create insert query" $ do
        it "Multiple records" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                d <- getDialect
                let q = multiInsertQuery d (TableMeta "table" []) ["col1", "col2"] 5
                q `shouldBe` "INSERT INTO table (col1, col2) VALUES (?, ?), (?, ?), (?, ?), (?, ?), (?, ?)"

    describe "Swap values of auto incremental column" $ do
        it "Auto incremental PK" $ do
            let ais = [11, 12, 13, 14, 15]
            (cursors, g) <- flip runStateT (newGraph :: Graph A :><: B) $ do
                                forM [1..5] $ \i -> (+<<) (Model (#aid @= i <: #cola @= (i * 2) <: emptyRecord) :: A)
            let g' = swapAutoIncrementalValue g (tables mock M.! "a") cursors ais
            let as = values g' :: [A] in do
                map (\v -> view #aid (getRecord v)) as `shouldBe` ais
                map (\v -> view #cola (getRecord v)) as `shouldBe` [2, 4, 6, 8, 10]

        it "Auto incremental non-PK column" $ do
            let ais = [11, 12, 13, 14, 15]
            (cursors, g) <- flip runStateT (newGraph :: Graph A :><: B) $ do
                                forM [1..5] $ \i -> (+<<) (Model (#bid @= i <: #colb @= (i * 2) <: emptyRecord) :: B)
            let g' = swapAutoIncrementalValue g (tables mock M.! "b") cursors ais
            let bs = values g' :: [B] in do
                map (\v -> view #bid (getRecord v)) bs `shouldBe` [1, 2, 3, 4, 5]
                map (\v -> view #colb (getRecord v)) bs `shouldBe` ais

    describe "Extract columns and values to insert" $ do
        it "No auto incremental column and foreign key" $ do
            (cursors, g) <- flip runStateT (newGraph :: NoKeyGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: NoKey)
            let t = tables mock M.! "nokey"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("id", toSql (1 :: Int)), ("c1", toSql (2 :: Int)), ("c2", toSql "v1")]
                           , [("id", toSql (2 :: Int)), ("c1", toSql (4 :: Int)), ("c2", toSql "v2")]
                           , [("id", toSql (3 :: Int)), ("c1", toSql (6 :: Int)), ("c2", toSql "v3")]
                           ]

        it "With an incremental column" $ do
            (cursors, g) <- flip runStateT (newGraph :: WithAutoGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: WithAuto)
            let t = tables mock M.! "withAuto"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("c1", toSql (2 :: Int)), ("c2", toSql "v1")]
                           , [("c1", toSql (4 :: Int)), ("c2", toSql "v2")]
                           , [("c1", toSql (6 :: Int)), ("c2", toSql "v3")]
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
            cvs `shouldBe` [ [("c1", toSql (2 :: Int)), ("c2", toSql "v1"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (1 :: Int))]
                           , [("c1", toSql (4 :: Int)), ("c2", toSql "v2"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (2 :: Int))]
                           , [("c1", toSql (6 :: Int)), ("c2", toSql "v3"), ("b_id", toSql (10 :: Int)), ("a_id", toSql (3 :: Int))]
                           ]