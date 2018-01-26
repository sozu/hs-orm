{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Database.ORM.QuerySpec where

import Test.Hspec
import Data.Proxy
import Data.Extensible
import Database.ORM.Query
import Database.ORM.Model
import Database.ORM.Utility

type Extra1 = ExtraModel '["a" :> Int, "b" :> String] '[]
type Extra2 = ExtraModel '["c" :> Int, "d" :> String] '[]

spec :: Spec
spec = do
    describe "Create formatted sorting informations" $ do
        it "Check empty OrderBy" $ do
            let s = orders $ formatOrderBy (../) (Proxy :: Proxy '[Int, Extra1, String]) ["t1", "t2", "t3"]
            s `shouldBe` ""

        it "Check an OrderBy" $ do
            let o = orderBy @Extra1 "col1" ASC
            let s = orders $ formatOrderBy o (Proxy :: Proxy '[Int, Extra1, String]) ["t1", "t2", "t3"]
            s `shouldBe` "t2.col1 ASC"

        it "Check merged OrderBy" $ do
            let o1 = orderBy @Extra1 "col1" ASC
            let o2 = orderBy @Extra2 "col2" DESC
            let o = o1 ./ o2
            let s = orders $ formatOrderBy o (Proxy :: Proxy '[Int, Extra1, String, Extra2]) ["t1", "t2", "t3", "t4"]
            s `shouldBe` "t2.col1 ASC, t4.col2 DESC"

    describe "Create combinations of models and aliases" $ do
        it "By type level list" $ do
            let ms = Proxy :: Proxy '[Extra1 @: "t1", Extra2 @: "t2"]
            generateColumns ms `shouldBe` [["t1.a", "t1.b"], ["t2.c", "t2.d"]]

        it "By alias strings" $ do
            let ms = aliasModels @'[Extra1, Extra2] ["t1", "t2"]
            generateColumns ms `shouldBe` [["t1.a", "t1.b"], ["t2.c", "t2.d"]]