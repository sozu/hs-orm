{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)
import Data.Proxy
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Default
import Data.Convertible
import Database.HDBC
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL

db = PostgreSQL "postgresql://postgres:postgres@localhost:15432/hs_orm" 10

$(declareModels (PostgreSQL "postgresql://postgres:postgres@localhost:15432/hs_orm" 10) "parent" "Parent")
$(declareModels (PostgreSQL "postgresql://postgres:postgres@localhost:15432/hs_orm" 10) "child" "Child")

type ParentChild = Graph Parent
                    :><: Child
                    :><: (Child :- Parent)

main :: IO ()
main = do
    lr <- newLogger def
    dr <- newResource db
    resources <- lr @+ dr

    args <- getArgs

    withContext' @'[DBContext PostgreSQL] resources $ do
        case args of
            []         -> profileInsert
            ["insert"] -> profileInsert
            ["update"] -> return ()
            ["select"] -> profileSelect
            _          -> return ()

    return ()

profileInsert :: (With '[DBContext PostgreSQL])
              => IO ()
profileInsert = do
    let p = Model ( #id @= 0
                 <: #name @= "name"
                 <: #description @= "description"
                 <: emptyRecord
                  ) :: (=+)Parent
    (_, !graph) <- (`runStateT` (newGraph :: AllInsert ParentChild)) $ do
        replicateM_ 1000 $ do
            cp <- (+<<) p
            replicateM_ 10 $ do
                cc <- (+<<) $! (Model (#id @= 0 <: #value @= 1.5 <: emptyRecord) :: (=+)Child)
                cc -*< cp

    restoreGraph graph
    return ()


profileSelect :: (With '[DBContext PostgreSQL])
              => IO ()
profileSelect = do
    graph <- selectNodes (Proxy :: Proxy ParentChild)
                         (Proxy :: Proxy Parent)
                         (..?)
                         (../)
                         Nothing
    let pv = foldl (\acc p -> acc + (p ^. #id)) 0 $ valuesOf @Parent graph
    let cv = foldl (\acc c -> acc + (c ^. #id)) 0 $ valuesOf @Child graph

    print $ "Parent = " ++ show (length $ valuesOf @Parent graph) ++ " Sum = " ++ show pv
    print $ "Child = " ++ show (length $ valuesOf @Child graph) ++ " Sum = " ++ show cv
    print $ "Children of the first parent = " ++ show (length ((cursorsOf' @Parent graph !! 0) *@< graph :: [Cursor Child]))