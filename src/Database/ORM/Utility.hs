{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Database.ORM.Utility where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy

class ElemIndexes (ts :: [*]) (as :: [*]) where
    elemIndexes :: Proxy ts -> Proxy as -> [Int]

instance ElemIndexes '[] (as :: [*]) where
    elemIndexes _ _ = []

instance (KnownNat (ElemIndex t as), ElemIndexes ts as) => ElemIndexes (t ': ts) (as :: [*]) where
    elemIndexes _ p = fromInteger (natVal (Proxy :: Proxy (ElemIndex t as))) : elemIndexes (Proxy :: Proxy ts) p

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

type family Head (as :: [*]) :: * where
    Head (a ': as) = a

-- | Gets a type at an index of a type level list.
type family ElemOf (i :: Nat) (as :: [*]) :: * where
    ElemOf 0 (a ': as) = a
    ElemOf i (a ': as) = ElemOf (i - 1) as

-- | Gets an index of a type in a type level list.
type family ElemIndex (a :: *) (as :: [*]) :: Nat where
    ElemIndex a (a ': as) = 0
    ElemIndex a (x ': as) = 1 + (ElemIndex a as)

type family HasElems (as :: [*]) (bs :: [*]) :: Constraint where
    HasElems (a ': '[]) bs = KnownNat (ElemIndex a bs)
    HasElems (a ': as) bs = (KnownNat (ElemIndex a bs), HasElems as bs)

type family Concat (as :: [*]) (bs :: [*]) :: [*] where
    Concat '[] bs = bs
    Concat (a ': as) bs = a ': Concat as bs

-- | Appends a type to a type level list.
type family Append a as :: [k] where
    Append x '[] = '[x]
    Append x (y ': ys) = y ': Append x ys

type family Length (as :: [*]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs