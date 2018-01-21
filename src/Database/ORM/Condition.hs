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
{-# LANGUAGE TypeApplications #-}

module Database.ORM.Condition where

import qualified Data.List as L
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Data.Convertible
import Database.HDBC
import Database.ORM.Record
import Database.ORM.Utility

data Condition (ts :: [*]) = Condition [String] (Proxy ts) [SqlValue]

-- cond @'[@A.Model, B.Model] "??" "3 * ??.val < 7 + ??.d"
cond :: forall ts. (AllRecord (ts :: [*]))
     => String
     -> String
     -> Condition ts
cond rep fmt = Condition (split fmt) (Proxy :: Proxy ts) []
    where
        split s 
            | s == "" = [""]
            | L.isPrefixOf rep s = "" : split (drop (length rep) s)
            | otherwise = let (v:vs) = split (tail s) in (head s:v) : vs

(..?) :: Condition '[]
(..?) = cond @'[] "" ""

(.+) :: (Convertible a SqlValue)
     => Condition ts
     -> a
     -> Condition ts
(.+) (Condition fmt pts vs) v = Condition fmt pts (vs ++ [convert v])

parense :: [String]
        -> [String]
parense (s:ss) = ('(':s) : (init ss ++ [last ss ++ ")"])

(.&) :: forall ts us. Condition ts
     -> Condition us
     -> Condition (Concat ts us)
(.&) (Condition fmt1 pts1 vs1) (Condition fmt2 pts2 vs2) = Condition merged (Proxy :: Proxy (Concat ts us)) (vs1 ++ vs2)
    where
        merged = let p1 = parense fmt1
                     p2 = parense fmt2
                 in init p1 ++ [last p1 ++ " AND " ++ head p2] ++ tail p2

(.|) :: forall ts us. Condition ts
     -> Condition us
     -> Condition (Concat ts us)
(.|) (Condition fmt1 pts1 vs1) (Condition fmt2 pts2 vs2) = Condition merged (Proxy :: Proxy (Concat ts us)) (vs1 ++ vs2)
    where
        merged = let p1 = parense fmt1
                     p2 = parense fmt2
                 in init p1 ++ [last p1 ++ " OR " ++ head p2] ++ tail p2

format :: (ElemIndexes ts as)
       => Condition ts
       -> Proxy (as :: [*])
       -> [String]
       -> String
format (Condition fmt pts _) p aliases = foldl (\s (t, f) -> s ++ t ++ f) "" $ zip ("":tables) fmt
    where
        tables = map (aliases !!) (elemIndexes pts p)

formatCondition :: (ElemIndexes ts as)
                => Condition ts
                -> Proxy (as :: [*])
                -> [String]
                -> (String, [SqlValue])
formatCondition fc@(Condition _ _ vs) p aliases = (format fc p aliases, vs)

-- | Declares methods to get where clause and values for placeholders in it.
class FormattedCondition c where
    -- | Gets the string of where clause.
    whereClause :: c -- ^ Condition.
                -> String -- ^ Where clause.

    -- | Gets values for placeholders.
    whereValues :: c -- ^ Condition.
                -> [SqlValue] -- ^ Values for placeholders.

instance (Convertible v SqlValue) => FormattedCondition (String, [v]) where
    whereClause (c, _) = c
    whereValues (_, vs) = map toSql vs

-- | Gets an empty condition.
unconditional = [] :: [(String, [Int])]

type family AllRecord (as :: [*]) :: Constraint where
    AllRecord '[] = ()
    AllRecord (a ': '[]) = RecordWrapper a
    AllRecord (a ': as) = (RecordWrapper a, AllRecord as)
