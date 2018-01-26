{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Database.ORM.ConditionSpec where

import Test.Hspec
import Data.Proxy
import Data.Convertible
import Data.Extensible
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.Model
import Database.ORM.Utility

type Extra1 = ExtraModel '["a" :> Int, "b" :> String]
type Extra2 = ExtraModel '["c" :> Int, "d" :> String]
type Extra3 = ExtraModel '["e" :> Int, "f" :> String]
type Extra4 = ExtraModel '["g" :> Int, "h" :> String]

sqlVal :: Int -> SqlValue
sqlVal i = convert i :: SqlValue

spec :: Spec
spec = do
    describe "Create formattable condition" $ do
        it "Check empty condition" $ do
            let s = formatCondition (..?) (Proxy :: Proxy '[Int, String, Extra1]) ["t0", "t1", "t2"]
            fst s `shouldBe` ""

        it "Check strings and values" $ do
            let (x, y) = (2, 3) :: (Int, Int)
            let c = cond @[Extra1, Extra2] "#" "#.val * ? > #.num - ?" .+ x .+ y
            case c of
                Condition fmt _ vs -> do
                    fmt `shouldBe` ["", ".val * ? > ", ".num - ?"]
                    vs `shouldBe` [sqlVal 2, sqlVal 3]

        it "Check model types by formatting" $ do
            let (x, y) = (2, 3) :: (Int, Int)
            let c = cond @[Extra1, Extra2] "#" "#.val * ? > #.num - ?" .+ x .+ y
            let s = formatCondition c (Proxy :: Proxy '[Int, Extra2, String, Extra1, Double]) ["t0", "t1", "t2", "t3", "t4"]
            fst s `shouldBe` "t3.val * ? > t1.num - ?"

    describe "Merge conditions" $ do
        it "Merge with AND/OR" $ do
            let c1 = cond @'[Extra1] "#" "#.col1 = ?" .+ (1 :: Int)
            let c2 = cond @'[Extra2] "#" "#.col2 = ?" .+ (2 :: Int)
            let c3 = cond @[Extra3, Extra4] "#" "#.col3 * ? < #.col4 * ?" .+ (3 :: Int) .+ (4 :: Int)
            let c = c3 .& (c1 .| c2)
            case c of
                Condition fmt _ vs -> do
                    -- (#.col3 * ? < #.col4 * ?) AND ((#.col1 = ?) OR (#.col2 = ?))
                    fmt `shouldBe` ["(", ".col3 * ? < ", ".col4 * ?) AND ((", ".col1 = ?) OR (", ".col2 = ?))"]
                    vs `shouldBe` [sqlVal 3, sqlVal 4, sqlVal 1, sqlVal 2]
            let s = formatCondition c (Proxy :: Proxy '[Extra4, Extra3, Extra2, Extra1]) ["t4", "t3", "t2", "t1"]
            fst s `shouldBe` "(t3.col3 * ? < t4.col4 * ?) AND ((t1.col1 = ?) OR (t2.col2 = ?))"

    describe "Simple condition functions" $ do
        it "Equal" $ do
            let c = (==?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b = ?", [toSql "abc"])

        it "Not equal" $ do
            let c = (!=?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b != ?", [toSql "abc"])

        it "Smaller" $ do
            let c = (<?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b < ?", [toSql "abc"])

        it "Larger" $ do
            let c = (>?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b > ?", [toSql "abc"])

        it "Smaller or equal to" $ do
            let c = (<=?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b <= ?", [toSql "abc"])

        it "Larger or equal to" $ do
            let c = (>=?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b >= ?", [toSql "abc"])

        it "In" $ do
            let c = (=@?) @Extra1 "b" ["abc", "def"]
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b IN (?, ?)", [toSql "abc", toSql "def"])

        it "Not in" $ do
            let c = (!@?) @Extra1 "b" ["abc", "def"]
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b NOT IN (?, ?)", [toSql "abc", toSql "def"])

        it "Pattern" $ do
            let c = (~=?) @Extra1 "b" "_abc%def_"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b LIKE ?", [toSql "_abc%def_"])

        it "Substring" $ do
            let c = (~@?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b LIKE ?", [toSql "%abc%"])

        it "Prefix" $ do
            let c = (~^?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b LIKE ?", [toSql "abc%"])

        it "Suffix" $ do
            let c = (~$?) @Extra1 "b" "abc"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b LIKE ?", [toSql "%abc"])

        it "Between" $ do
            let c = (><?) @Extra1 "b" "abc" "def"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b BETWEEN ? AND ?", [toSql "abc", toSql "def"])

        it "Not between" $ do
            let c = (<>?) @Extra1 "b" "abc" "def"
            formatCondition c (Proxy :: Proxy '[Extra1]) ["t"] `shouldBe` ("t.b NOT BETWEEN ? AND ?", [toSql "abc", toSql "def"])