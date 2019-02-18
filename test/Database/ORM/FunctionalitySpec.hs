{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Database.ORM.FunctionalitySpec where

import Test.Hspec
import Data.Proxy
import qualified Data.Map as M
import Database.HDBC
import Database.ORM.Dialect.Mock
import Database.ORM.Functionality

spec :: Spec
spec = do
    describe "Apply record lock" $ do
        it "Apply record lock" $ do
            let q = recordLockQuery (Proxy :: Proxy Mock)
                                    (Proxy :: Proxy '[Int, RecordLock Mock 'Exclusive, String])
                                    "SELECT * FROM t"
            q `shouldBe` "SELECT * FROM t FOR UPDATE"

        it "No record lock" $ do
            let q = recordLockQuery (Proxy :: Proxy Mock)
                                    (Proxy :: Proxy '[Int, String])
                                    "SELECT * FROM t"
            q `shouldBe` "SELECT * FROM t"

        it "Empty list" $ do
            let q = recordLockQuery (Proxy :: Proxy Mock)
                                    (Proxy :: Proxy '[])
                                    "SELECT * FROM t"
            q `shouldBe` "SELECT * FROM t"