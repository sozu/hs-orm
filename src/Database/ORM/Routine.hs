{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Database.ORM.Routine where

import GHC.TypeLits
import Data.Proxy
import Control.Monad.Identity
import Data.Extensible
import Data.Extensible.HList
import Data.Model.Graph
import Database.HDBC
import Database.ORM.Condition
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record
import Database.ORM.Query
import Database.ORM.Select
import Database.ORM.Utility

fetchRecord :: forall db g a. (
               WithDB db
             , GraphContainer g a
             , SelectNodes g a (EdgeTypes g a)
             , KnownNat (Length (EdgeTypes g a))
             , ElemIndexes (ReplicateType a (RW'KeyTypes a)) (EdgeTypes g a)
             , Identifiable a
             , Forall SqlValueConstraint (RW'KeyTypes a))
            => Proxy g
            -> Proxy a
            -> HList Identity (RW'KeyTypes a)
            -> IO g
fetchRecord pg pa values = do
    let conds = makePKCondition pa (getKeyNames pa) values
    selectNodes pg pa conds (../) Nothing

makePKCondition :: forall a h vs as. (Identifiable a, Forall SqlValueConstraint vs)
                => Proxy a
                -> [String]
                -> HList Identity vs
                -> Condition (ReplicateType a vs)
makePKCondition _ _ HNil = (..?)
makePKCondition pa (c : cs) (v `HCons` vs) = ((==?) @a c (runIdentity v)) .& (makePKCondition pa cs vs)

type family ReplicateType a (as :: [*]) :: [*] where
    ReplicateType a '[] = '[]
    ReplicateType a (x ': xs) = a ': (ReplicateType a xs)
