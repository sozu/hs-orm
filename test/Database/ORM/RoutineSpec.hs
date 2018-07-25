{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Database.ORM.RoutineSpec where

import Test.Hspec
import Data.Proxy
import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Model.Graph
import Data.Resource
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Routine
import Database.ORM.Resource
import Database.ORM.Condition
import Database.ORM.Dialect.Mock

type A = "a" :## Record '["aid" :> Int, "cola" :> String] :^+ PK '["aid"]

col :: Bool -> String -> Bool -> ColumnMeta
col pk n auto = ColumnMeta pk n "" "" False auto []

tableMap :: M.Map String TableMeta
tableMap = M.fromList [ ("a", TableMeta "a" [col True "aid" True, col False "cola" False])
                      ]

mock :: Mock
mock = Mock { dbUrl = ""
            , tables = tableMap
            } 

-- TODO: How to test? Executing queries are not accessible...

spec :: Spec
spec = do
    describe "Fetch one record" $ do
        it "Fetch from a table having a PK" $ do
            r <- newResource mock
            let resources = r `RCons` RNil
            let aliases = ["t0"]
            withContext @'[DBContext Mock] resources $ do
                let conds = pkConditions (Proxy :: Proxy A) (Proxy :: Proxy '[A]) [("aid", toSql (1 :: Int))]
                print $ formatCondition conds (Proxy :: Proxy '[A]) aliases
                fetchRecord @(Graph A) (1 :: Int)
            return ()