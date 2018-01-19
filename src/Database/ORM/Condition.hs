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

module Database.ORM.Condition where

import qualified Data.List as L
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Data.Convertible
import Database.HDBC
import Database.ORM.Record

data FormattableCondition (ts :: [*]) = FormattableCondition String String (Proxy ts) [SqlValue]

-- cond @'[@A.Model, B.Model] "??" "3 * ??.val < 7 + ??.d"
cond :: forall ts. (AllRecord (ts :: [*]))
     => String
     -> String
     -> FormattableCondition ts
cond rep fmt = FormattableCondition fmt rep (Proxy :: Proxy ts) []

(?+) :: (Convertible a SqlValue)
     => FormattableCondition ts
     -> a
     -> FormattableCondition ts
(?+) (FormattableCondition fmt rep pts vs) v = FormattableCondition fmt rep pts (vs ++ [convert v])

class ElemIndexes (ts :: [*]) (as :: [*]) where
    elemIndexes :: Proxy ts -> Proxy as -> [Int]

instance ElemIndexes '[] (as :: [*]) where
    elemIndexes _ _ = []

instance (KnownNat (ElemIndex t as), ElemIndexes ts as) => ElemIndexes (t ': ts) (as :: [*]) where
    elemIndexes _ p = fromInteger (natVal (Proxy :: Proxy (ElemIndex t as))) : elemIndexes (Proxy :: Proxy ts) p

format :: (ElemIndexes ts as)
       => FormattableCondition ts
       -> Proxy (as :: [*])
       -> [String]
       -> String
format (FormattableCondition fmt rep pts _) p aliases = replace fmt rep tables
    where
        tables = map (aliases !!) (elemIndexes pts p)
        replace :: String -> String -> [String] -> String
        replace s r vs
            | s == "" = ""
            | L.isPrefixOf r s = (head vs) ++ replace (drop (length r) s) r (tail vs)
            | otherwise = (head s) : replace (tail s) r vs

formatCondition :: (ElemIndexes ts as)
                => FormattableCondition ts
                -> Proxy (as :: [*])
                -> [String]
                -> (String, [SqlValue])
formatCondition fc@(FormattableCondition _ _ _ vs) p aliases = (format fc p aliases, vs)

-- | Declares methods to get where clause and values for placeholders in it.
class Condition c where
    -- | Gets the string of where clause.
    whereClause :: c -- ^ Condition.
                -> String -- ^ Where clause.

    -- | Gets values for placeholders.
    whereValues :: c -- ^ Condition.
                -> [SqlValue] -- ^ Values for placeholders.

instance (Convertible v SqlValue) => Condition (String, [v]) where
    whereClause (c, _) = c
    whereValues (_, vs) = map toSql vs

-- | Gets an empty condition.
unconditional = [] :: [(String, [Int])]

-- ------------------------------------------------------------
-- Type level functions.
-- ------------------------------------------------------------

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

type family AllRecord (as :: [*]) :: Constraint where
    AllRecord (a ': '[]) = RecordWrapper a
    AllRecord (a ': as) = (RecordWrapper a, AllRecord as)