{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.THSpec where

import Test.Hspec
import GHC.TypeLits
import Data.Proxy
import Control.Monad.IO.Class
import Control.Lens hiding ((:>))
import Data.Extensible
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.TH

type A = "a" :## Record '["id" :> Int, "col1" :> String, "col2" :> Double, "col3" :> String]
--type A = TableModel "a" Select' (Record '["id" :> Int, "col1" :> String, "col2" :> Double, "col3" :> String]) '[]

$(shrinkDefinition ''A $ TableMeta "a" [ ColumnMeta True "id" "int" "" False True [] , ColumnMeta False "col1" "text" "" False False [] , ColumnMeta False "col2" "real" "" False False [] , ColumnMeta False "col3" "text" "" False False [] ])

check :: (RecordWrapper m)
      => m
      -> IO ()
check m = do
    let r = getRecord m
    recordFields r `shouldBe` ["id", "col3"]
    recordValues r `shouldBe` [toSql (1 :: Int), toSql "def"]

spec :: Spec
spec = do
    describe "Use shrink model" $ do
        it "shrink to 2 columns" $ do
            let a = Model ( #id @= 1
                         <: #col1 @= "abc"
                         <: #col2 @= 2.5
                         <: #col3 @= "def"
                         <: emptyRecord
                          ) :: A

            withSubModel a ["id", "col3"] check :: IO ()