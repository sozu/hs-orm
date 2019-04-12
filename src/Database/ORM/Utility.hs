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

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

-- | This class provides a way to declare empty constraint.
--
-- `NoConstraint a` doesn't apply any constraint because every type is an instance of this class.
class NoConstraint a
instance NoConstraint a

-- | Takes a head type from a type level list.
type family Head (as :: [*]) :: * where
    Head (a ': as) = a

-- | Returns a type level list by removing a head type from the argument.
type family Tail (as :: [*]) :: [*] where
    Tail (a ': as) = as

-- | Concatenates two type level lists.
type family Concat (as :: [k]) (bs :: [k]) :: [k] where
    Concat '[] bs = bs
    Concat (a ': as) bs = a ': Concat as bs

-- | Returns a reversed type level list.
type family Reverse (as :: [k]) :: [k] where
    Reverse as = Rev as '[]
type family Rev (as :: [k]) (bs :: [k]) :: [k] where
    Rev '[] as = as
    Rev (x ': xs) as = Rev xs (x ': as)

-- | Gets the length of a type level list.
type family Length (as :: [*]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

-- | Returns a function type which takes arguments of given types.
--
-- > Apply '[A, B, C] F == A -> B -> C -> F
type family Apply (as :: [*]) (f :: *) :: *
type instance Apply '[] f = f
type instance Apply (a ': as) f = a -> Apply as f

-- | Returns a constraint which claims every type in the list `as` conforms to the constraint `c`.
type family ConstraintAll (c :: * -> Constraint) (as :: [*]) :: Constraint
type instance ConstraintAll c '[] = ()
type instance ConstraintAll c (a ': as) = (c a, ConstraintAll c as)

-- | Instances of this class have to provide a method to pop a type from the associated type list.
class PopType c where
    -- | Pop a top type from associated types like a queue.
    --
    -- `h` is any type applied to every type in associated type list. Semantically,
    -- > c h '[A, B, ...] == c '[h A, h B, ...]
    -- > popType (c h '[A, B, ...]) = (h A, c h '[B, ...])
    popType :: c h (a ': as) -- ^ An object of the type.
            -> (h a, c h as) -- ^ Top element and remainders.

-- | This class means `as` contains `a`.
class Contains (as :: [*]) a where
    -- | Returns index of `a` in `as`.
    indexOf :: Int -- ^ Starting index, typically 0.
            -> Proxy as -- ^ Type level list.
            -> Proxy a -- ^ A type to search.
            -> Int -- ^ An index of `a` in `as`.

    -- | Get an item in hetero-typed collections by a type.
    getByType :: (PopType c)
              => c h as -- ^ Hetero-typed collection.
              -> h a -- ^ An element whose type corresponds to `a`.

instance {-# OVERLAPS #-} Contains (a ': as) a where
    indexOf i _ _ = i
    getByType h = fst $ popType h
instance (Contains as a) => Contains (x ': as) a where
    indexOf i _ p = indexOf (i+1) (Proxy :: Proxy as) p
    getByType h = getByType (snd $ popType h)

-- | This class means `as` contains every type in `bs` and each type in `as` conforms to the constraint `c`.
class ContainsAll (as :: [*]) (bs :: [*]) (c :: * -> Constraint) where
    -- | Applies a function to each type in `as` and returns the results.
    mapEach :: (forall a. (c a) => Int -> Proxy a -> r) -- ^ A function to apply each type in `as`.
            -> Proxy as -- ^ Types of `as`.
            -> Proxy bs -- ^ Types of `bs`.
            -> Proxy c -- ^ Type of a constraint.
            -> [r] -- ^ Results of the function.

instance {-# OVERLAPPING #-} ContainsAll as '[] c where
    mapEach _ _ _ _ = []

instance {-# OVERLAPPABLE #-} (Contains as b, ContainsAll as bs c, c b) => ContainsAll as (b ': bs) c where
    mapEach f pa _ pc = let p = Proxy :: Proxy b
                        in f (indexOf 0 pa p) p : mapEach f pa (Proxy :: Proxy bs) pc

instance {-# OVERLAPS #-} (ForEachType (a ': as) c) => ContainsAll (a ': as) (a ': as) c where
    mapEach f pa _ pc = forEachType 0 f pa pc

-- | Type synonym of `ContainsAll` having no constraint.
type ContainsAll' as bs = ContainsAll as bs NoConstraint

-- | Applies a function to each type in `as` and returns the results.
--
-- This function is for the special case where `as` contains `bs` without any constraint.
mapEach' :: (ContainsAll' as bs)
         => (forall (a :: *). Int -> Proxy a -> r) -- ^ A function to apply each type in `as`.
         -> Proxy as -- ^ Types of `as`.
         -> Proxy bs -- ^ Types of `bs`.
         -> [r]
mapEach' f pa pb = mapEach f pa pb (Proxy :: Proxy NoConstraint)

-- | Declares a method to apply a function to each type in a type level list which complies a constraint.
class ForEachType (as :: [*]) (c :: * -> Constraint) where
    -- | Apply a function to each type in `as`.
    forEachType :: Int -- ^ Starting index, typically 0.
                -> (forall (a :: *). (c a) => Int -> Proxy a -> r) -- ^ A function to apply.
                -> Proxy as -- ^ Types of `as`.
                -> Proxy c -- ^ Type of a constraint.
                -> [r] -- ^ Results of the function.

instance {-# OVERLAPPING #-} ForEachType '[] c where
    forEachType i f _ pc = []
instance {-# OVERLAPPING #-} (c a) => ForEachType '[a] c where
    forEachType i f _ pc = f i (Proxy :: Proxy a) : []
instance (ForEachType as c, c a) => ForEachType (a ': as) c where
    forEachType i f _ pc = f i (Proxy :: Proxy a) : forEachType (i+1) f (Proxy :: Proxy as) pc