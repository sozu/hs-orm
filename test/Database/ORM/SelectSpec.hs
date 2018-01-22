{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Database.ORM.SelectSpec where

import Test.Hspec
import qualified Data.Map as M
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Data.Model.Graph
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Select
import Database.ORM.Utility
import Database.ORM.Dialect.Mock

type Extra1 = ExtraModel '["a" :> Int, "b" :> String]
type Extra2 = ExtraModel '["c" :> Int, "d" :> String]
type Extra3 = ExtraModel '["e" :> Int, "f" :> String]
type Extra4 = ExtraModel '["g" :> Int, "h" :> String]

type A = "a" :## Record '["id" :> Int, "cola" :> String]
type B = "b" :## Record '["id" :> Int, "colb" :> String]
type C = "c" :## Record '["id" :> Int, "colc" :> String]
type D = "d" :## Record '["id" :> Int, "cold" :> String]

type ABCDGraph = Graph A
                  :><: B
                  :><: C
                  :><: D
                  :><: (B :- A)
                  :><: (C :- B)
                  :><: (D :- B)

type ABCD = '[A, B, C, D]

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" False auto Nothing

rel :: ColumnMeta -> (String, String) -> ColumnMeta
rel cm (t, c) = cm { relation = Just (Relation t c) }

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                                  , ("b", TableMeta "b" [col True "bid" True, col False "colb" False, col False "b_a_id" False `rel` ("a", "aid")])
                                  , ("c", TableMeta "c" [col True "cid" True, col False "colc" False, col False "c_b_id" False `rel` ("b", "bid")])
                                  , ("d", TableMeta "d" [col True "did" True, col False "cold" False, col False "d_b_id" False `rel` ("b", "bid")])
                                  ]
            } 

spec :: Spec
spec = do
    describe "Collect join informations" $ do
        it "Collect all" $ do
            r <- newResource mock
            let ?resource = r
            withContext $ do
                joins <- collectJoins (Proxy :: Proxy (CollectEdges ABCD (Edges ABCDGraph))) (Proxy :: Proxy ABCD) ["ta", "tb", "tc", "td"] :: IO [JoinEdge ABCDGraph ABCD]
                map show joins `shouldBe` [ "INNER JOIN b AS tb ON tb.b_a_id = ta.aid"
                                          , "INNER JOIN c AS tc ON tc.c_b_id = tb.bid"
                                          , "INNER JOIN d AS td ON td.d_b_id = tb.bid"
                                          ]