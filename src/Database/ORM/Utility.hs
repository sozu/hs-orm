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
{-# LANGUAGE RankNTypes #-}

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

class NoConstraint a
instance NoConstraint a

type family Head (as :: [*]) :: * where
    Head (a ': as) = a

type family Tail (as :: [*]) :: [*] where
    Tail (a ': as) = as

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

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat '[] bs = bs
    Concat (a ': as) bs = a ': Concat as bs

-- | Appends a type to a type level list.
type family Append a as :: [k] where
    Append x '[] = '[x]
    Append x (y ': ys) = y ': Append x ys

type family Reverse (as :: [k]) :: [k] where
    Reverse as = Rev as '[]
type family Rev (as :: [k]) (bs :: [k]) :: [k] where
    Rev '[] as = as
    Rev (x ': xs) as = Rev xs (x ': as)

type family Length (as :: [*]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Apply (as :: [*]) (f :: *) :: *
type instance Apply '[] f = f
type instance Apply (a ': as) f = a -> Apply as f

type family ConstraintAll (c :: * -> Constraint) (as :: [*]) :: Constraint
type instance ConstraintAll c '[] = ()
type instance ConstraintAll c (a ': as) = (c a, ConstraintAll c as)

class PopType c where
    popType :: c h (a ': as) -> (h a, c h as)

class Contains (as :: [*]) a where
    indexOf :: Int -> Proxy as -> Proxy a -> Int
    getByType :: (PopType c) => c h as -> h a

instance {-# OVERLAPS #-} Contains (a ': as) a where
    indexOf i _ _ = i
    getByType h = fst $ popType h
instance (Contains as a) => Contains (x ': as) a where
    indexOf i _ p = indexOf (i+1) (Proxy :: Proxy as) p
    getByType h = getByType (snd $ popType h)

class ContainsAll (as :: [*]) (bs :: [*]) (c :: * -> Constraint) where
    mapEach :: (forall a. (c a) => Int -> Proxy a -> r) -> Proxy as -> Proxy bs -> Proxy c -> [r]

instance {-# OVERLAPPING #-} ContainsAll as '[] c where
    mapEach _ _ _ _ = []

instance {-# OVERLAPPABLE #-} (Contains as b, ContainsAll as bs c, c b) => ContainsAll as (b ': bs) c where
    mapEach f pa _ pc = let p = Proxy :: Proxy b
                        in f (indexOf 0 pa p) p : mapEach f pa (Proxy :: Proxy bs) pc

instance {-# OVERLAPS #-} (ForEachType (a ': as) c) => ContainsAll (a ': as) (a ': as) c where
    mapEach f pa _ pc = forEachType 0 f pa pc

type ContainsAll' as bs = ContainsAll as bs NoConstraint

mapEach' :: (ContainsAll' as bs)
         => (forall (a :: *). Int -> Proxy a -> r)
         -> Proxy as
         -> Proxy bs
         -> [r]
mapEach' f pa pb = mapEach f pa pb (Proxy :: Proxy NoConstraint)

class ForEachType (as :: [*]) (c :: * -> Constraint) where
    forEachType :: Int -> (forall (a :: *). (c a) => Int -> Proxy a -> r) -> Proxy as -> Proxy c -> [r]

instance {-# OVERLAPPING #-} ForEachType '[] c where
    forEachType i f _ pc = []
instance {-# OVERLAPPING #-} (c a) => ForEachType '[a] c where
    forEachType i f _ pc = f i (Proxy :: Proxy a) : []
instance (ForEachType as c, c a) => ForEachType (a ': as) c where
    forEachType i f _ pc = f i (Proxy :: Proxy a) : forEachType (i+1) f (Proxy :: Proxy as) pc

--forEach' :: Proxy (as :: [*])
--         -> (forall (a :: *). Int -> Proxy a -> r)
--         -> [r]
--forEach' p f = forEach 0 f p