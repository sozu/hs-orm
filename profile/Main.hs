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

module Main where

import Control.Applicative
import Control.Monad.State
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

    withContext' @'[DBContext PostgreSQL] resources $ do
        let p = Model ( #id @= 0
                     <: #name @= "name"
                     <: #description @= "description"
                     <: emptyRecord
                      ) :: (=+)Parent
        (_, !graph) <- (`runStateT` (newGraph :: AllInsert ParentChild)) $ do
            replicateM_ 100 $ do
                cp <- (+<<) p
                replicateM_ 10 $ do
                    cc <- (+<<) $! (Model (#id @= 0 <: #value @= 1.5 <: emptyRecord) :: (=+)Child)
                    cc -*< cp

        restoreGraph graph

    --print $ valuesOf @((=+)Parent) g
    --print $ valuesOf @((=+)Child) g

    return ()