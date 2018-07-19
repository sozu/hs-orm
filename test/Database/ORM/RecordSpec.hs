{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.ORM.RecordSpec where

import Test.Hspec
import Data.Proxy
import Control.Lens hiding ((:>))
import Data.Extensible
import Database.ORM.Model
import Database.ORM.Record

type TestTable = "test" :++ Record '["id" :> Int, "name" :> String]

spec :: Spec
spec = do
    describe "Lens access" $ do
        it "Getter" $ do
            let m = Model (#id @= 1 <: #name @= "abc" <: emptyRecord) :: TestTable
            view #id m `shouldBe` (1 :: Int)
            view #name m `shouldBe` "abc"