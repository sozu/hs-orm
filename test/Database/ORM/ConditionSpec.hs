{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Database.ORM.ConditionSpec where

import Test.Hspec
import Data.Convertible
import Data.Extensible
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.Model

type Extra1 = ExtraModel '["a" :> Int, "b" :> String]
type Extra2 = ExtraModel '["c" :> Int, "d" :> String]

spec :: Spec
spec = do
    describe "Create formattable condition" $ do
        it "String and model" $ do
            let (x, y) = (2, 3) :: (Int, Int)
            let c = cond @[Extra1, Extra2] "#" "#.val * ? > #.num - ?" ?+ x ?+ y
            case c of
                FormattableCondition fmt rep _ vs -> do
                    fmt `shouldBe` "#.val * ? > #.num - ?" 
                    rep `shouldBe` "#"
                    vs `shouldBe` [convert (2 :: Int) :: SqlValue, convert (3 :: Int) :: SqlValue]