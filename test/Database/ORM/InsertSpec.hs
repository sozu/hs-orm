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
import qualified Data.List as L
import qualified Data.Map as M
import Data.Convertible
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Model.Graph
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Insert
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Resource
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type A = "a" :++ Record '["aid" :> Int, "cola" :> Int]
type B = "b" :++ Record '["bid" :> Int, "colb" :> Int]

type NoKey = "nokey" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithAuto = "withAuto" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithFK = "withFK" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String]
type WithColExp = "withColExp" :++ Record '["id" :> Int, "c1" :> Int, "c2" :> String] :^+ ColExp "c2" "raw_exp"

type NoKeyGraph = Graph NoKey
type WithAutoGraph = Graph WithAuto
type WithFKGraph = Graph A :><: B :><: WithFK :><: (WithFK :- A) :><: (WithFK :- B)
type WithColExpGraph = Graph WithColExp

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" "" False auto []

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relations = [Relation t c] }

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
            let resources = r `RCons` RNil
            withContext @'[DBContext Mock] resources $ do
                d <- getDialect
                let (q, hs) = multiInsertQuery d (TableMeta "table" []) ["col1", "col2"] (L.replicate 5 [ValueOf SqlNull, ValueOf SqlNull])
                (q ++ hs) `shouldBe` "INSERT INTO table (col1, col2) VALUES (?, ?), (?, ?), (?, ?), (?, ?), (?, ?)"
            return ()

        it "With columns using raw expressions" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            withContext @'[DBContext Mock] resources $ do
                d <- getDialect
                let (q, hs) = multiInsertQuery d (TableMeta "table" []) ["col1", "col2"] [ [ValueOf SqlNull, ValueOf SqlNull]
                                                                                         , [RawExpression "exp1", ValueOf SqlNull]
                                                                                         , [ValueOf SqlNull, ValueOf SqlNull]
                                                                                         , [ValueOf SqlNull, RawExpression "exp2"]
                                                                                         , [ValueOf SqlNull, ValueOf SqlNull]
                                                                                         ]
                (q ++ hs) `shouldBe` "INSERT INTO table (col1, col2) VALUES (?, ?), (exp1, ?), (?, ?), (?, exp2), (?, ?)"
            return ()

    describe "Swap values of auto incremental column" $ do
        it "Auto incremental PK" $ do
            let ais = [11, 12, 13, 14, 15]
            (cursors, g) <- flip runStateT (newGraph :: Graph A :><: B) $ do
                                forM [1..5] $ \i -> (+<<) (Model (#aid @= i <: #cola @= (i * 2) <: emptyRecord) :: A)
            let g' = swapAutoIncrementalValue g (tables mock M.! "a") cursors ais
            let as = valuesOf @A g' in do
                map (\v -> view #aid (getRecord v)) as `shouldBe` ais
                map (\v -> view #cola (getRecord v)) as `shouldBe` [2, 4, 6, 8, 10]

        it "Auto incremental non-PK column" $ do
            let ais = [11, 12, 13, 14, 15]
            (cursors, g) <- flip runStateT (newGraph :: Graph A :><: B) $ do
                                forM [1..5] $ \i -> (+<<) (Model (#bid @= i <: #colb @= (i * 2) <: emptyRecord) :: B)
            let g' = swapAutoIncrementalValue g (tables mock M.! "b") cursors ais
            let bs = valuesOf @B g' in do
                map (\v -> view #bid (getRecord v)) bs `shouldBe` [1, 2, 3, 4, 5]
                map (\v -> view #colb (getRecord v)) bs `shouldBe` ais

    describe "Extract columns and values to insert" $ do
        it "No auto incremental column and foreign key" $ do
            (cursors, g) <- flip runStateT (newGraph :: NoKeyGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: NoKey)
            let t = tables mock M.! "nokey"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("id", sqlVal (1 :: Int)), ("c1", sqlVal (2 :: Int)), ("c2", sqlVal "v1")]
                           , [("id", sqlVal (2 :: Int)), ("c1", sqlVal (4 :: Int)), ("c2", sqlVal "v2")]
                           , [("id", sqlVal (3 :: Int)), ("c1", sqlVal (6 :: Int)), ("c2", sqlVal "v3")]
                           ]

        it "With an incremental column" $ do
            (cursors, g) <- flip runStateT (newGraph :: WithAutoGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: WithAuto)
            let t = tables mock M.! "withAuto"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("c1", sqlVal (2 :: Int)), ("c2", sqlVal "v1")]
                           , [("c1", sqlVal (4 :: Int)), ("c2", sqlVal "v2")]
                           , [("c1", sqlVal (6 :: Int)), ("c2", sqlVal "v3")]
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
            cvs `shouldBe` [ [("c1", sqlVal (2 :: Int)), ("c2", sqlVal "v1"), ("b_id", sqlVal (10 :: Int)), ("a_id", sqlVal (1 :: Int))]
                           , [("c1", sqlVal (4 :: Int)), ("c2", sqlVal "v2"), ("b_id", sqlVal (10 :: Int)), ("a_id", sqlVal (2 :: Int))]
                           , [("c1", sqlVal (6 :: Int)), ("c2", sqlVal "v3"), ("b_id", sqlVal (10 :: Int)), ("a_id", sqlVal (3 :: Int))]
                           ]

        it "With ColExp" $ do
            (cursors, g) <- flip runStateT (newGraph :: WithColExpGraph) $ do
                                    forM [1..3] $ \i -> (+<<) (Model (#id @= i <: #c1 @= i*2 <: #c2 @= "v" ++ show i <: emptyRecord) :: WithColExp)
            let t = tables mock M.! "nokey"

            cvs <- columnsAndValues g cursors t
            cvs `shouldBe` [ [("id", sqlVal (1 :: Int)), ("c1", sqlVal (2 :: Int)), ("c2", RawExpression "raw_exp")]
                           , [("id", sqlVal (2 :: Int)), ("c1", sqlVal (4 :: Int)), ("c2", RawExpression "raw_exp")]
                           , [("id", sqlVal (3 :: Int)), ("c1", sqlVal (6 :: Int)), ("c2", RawExpression "raw_exp")]
                           ]

    where
        sqlVal :: (Convertible a SqlValue) => a -> ColumnValue
        sqlVal v = ValueOf (toSql (v))