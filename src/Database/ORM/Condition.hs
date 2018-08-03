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
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.Condition (
      -- * Conditions
      Condition(..)
      , cond
      , (..?)
      , (.+)
      , (.&), (.|)
      , format, formatCondition
      , FormattedCondition(..)
      -- * Shortcut functions for single column conditions
      , (==?), (!=?)
      , (<?), (>?), (<=?), (>=?)
      , (=@?), (!@?)
      , escapeLike
      , (~=?), (~@?), (~^?), (~$?)
      , (><?), (<>?)
      , (*-)
) where

import qualified Data.List as L
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Data.Convertible
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Query
import Database.ORM.Record
import Database.ORM.Utility

-- ------------------------------------------------------------
-- Conditions
-- ------------------------------------------------------------

data Condition (ts :: [*]) = Condition [String] (Proxy ts) [SqlValue]

-- cond @'[A.Model, B.Model] "??" "3 * ??.val < 7 + ??.d"
cond :: forall ts. (AllRecord (ts :: [*]), KnownNat (Length ts))
     => String
     -> String
     -> Condition ts
cond rep fmt = Condition (split fmt len) (Proxy :: Proxy ts) []
    where
        len = natVal (Proxy :: Proxy (Length ts))
        split s n
            | s == "" = [""]
            | n == 0 = [s]
            | L.isPrefixOf rep s = "" : split (drop (length rep) s) (n-1)
            | otherwise = let (v:vs) = split (tail s) n in (head s:v) : vs

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

-- | Concatenate two conditions by 'AND' operator.
(.&) :: forall ts us. Condition ts -- ^ First condition.
     -> Condition us -- ^ Second condition.
     -> Condition (Concat ts us) -- ^ Concatenated condition.
(.&) (Condition fmt1 pts1 vs1) (Condition fmt2 pts2 vs2) = Condition merged (Proxy :: Proxy (Concat ts us)) (vs1 ++ vs2)
    where
        merged = let p1 = parense fmt1
                     p2 = parense fmt2
                 in init p1 ++ [last p1 ++ " AND " ++ head p2] ++ tail p2

-- | Concatenate two conditions by 'OR' operator.
(.|) :: forall ts us. Condition ts -- ^ First condition.
     -> Condition us -- ^ Second condition.
     -> Condition (Concat ts us) -- ^ Concatenated condition.
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

-- ------------------------------------------------------------
-- Shortcuts for single column
-- ------------------------------------------------------------

-- | Is a column equal to a value?
-- > col = v
(==?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- ^ A value.
      -> Condition '[t] -- ^ Condition.
(==?) col v = cond @'[t] "#" ("#." ++ col ++ " = ?") .+ v

-- | Is not a column equal to a value?
-- > col != v
(!=?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- ^ A value.
      -> Condition '[t] -- ^ Condition.
(!=?) col v = cond @'[t] "#" ("#." ++ col ++ " != ?") .+ v

-- | Is a column smaller than a value?
-- > col < v
(<?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
     => String -- ^ Column name.
     -> a -- ^ A value.
     -> Condition '[t] -- ^ Condition.
(<?) col v = cond @'[t] "#" ("#." ++ col ++ " < ?") .+ v

-- | Is a column larger than a value?
-- > col > v
(>?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
     => String -- ^ Column name.
     -> a -- ^ A value.
     -> Condition '[t] -- ^ Condition.
(>?) col v = cond @'[t] "#" ("#." ++ col ++ " > ?") .+ v

-- | Is a column smaller than or equal to a value?
-- > col <= v
(<=?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- A value.
      -> Condition '[t] -- ^ Condition.
(<=?) col v = cond @'[t] "#" ("#." ++ col ++ " <= ?") .+ v

-- | Is a column larger than or equal to a value?
-- > col >= v
(>=?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- A value.
      -> Condition '[t] -- ^ Condition.
(>=?) col v = cond @'[t] "#" ("#." ++ col ++ " >= ?") .+ v

-- | Is a column value included in values?
-- > col IN (v1, v2, ...)
(=@?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> [a] -- ^ Values.
      -> Condition '[t] -- ^ Condition.
(=@?) col [] = cond @'[t] "#" "FALSE"
(=@?) col vs = foldl (.+) (cond @'[t] "#" ("#." ++ col ++ " IN (" ++ holder (length vs) ++ ")")) vs

-- | Is not a column value included in values?
-- > col NOT IN (v1, v2, ...)
(!@?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> [a] -- ^ Values.
      -> Condition '[t] -- ^ Condition.
(!@?) col [] = cond @'[t] "#" "TRUE"
(!@?) col vs = foldl (.+) (cond @'[t] "#" ("#." ++ col ++ " NOT IN (" ++ holder (length vs) ++ ")")) vs

-- | Escapes a string to be used in LIKE operation.
escapeLike :: String -- ^ A string.
           -> String -- ^ Escaped string.
escapeLike ('_':cs) = '\\' : '_' : escapeLike cs
escapeLike ('%':cs) = '\\' : '%' : escapeLike cs
escapeLike ('\\':cs) = '\\' : '\\' : escapeLike cs
escapeLike s = s

-- | Does a column matches to a pattern?
-- > col LIKE "pattern"
(~=?) :: forall t. (RecordWrapper t)
      => String -- ^ Column name.
      -> String -- ^ A pattern.
      -> Condition '[t] -- ^ Condition.
(~=?) col p = cond @'[t] "#" ("#." ++ col ++ " LIKE ?") .+ p

-- | Does a column has a substring?
-- > col LIKE "%pattern%"
(~@?) :: forall t. (RecordWrapper t)
      => String -- ^ Column name.
      -> String -- ^ A substring.
      -> Condition '[t] -- ^ Condition.
(~@?) col p = cond @'[t] "#" ("#." ++ col ++ " LIKE ?") .+ ('%' : reverse ('%' : reverse (escapeLike p)))

-- | Does a column has a prefix?
-- > col LIKE "prefix%"
(~^?) :: forall t. (RecordWrapper t)
      => String -- ^ Column name.
      -> String -- ^ A prefix.
      -> Condition '[t] -- ^ Condition.
(~^?) col p = cond @'[t] "#" ("#." ++ col ++ " LIKE ?") .+ reverse ('%' : reverse (escapeLike p))

-- | Does a column has a suffix?
-- > col LIKE "%suffix"
(~$?) :: forall t. (RecordWrapper t)
      => String -- ^ Column name.
      -> String -- ^ A suffix.
      -> Condition '[t] -- ^ Condition.
(~$?) col p = cond @'[t] "#" ("#." ++ col ++ " LIKE ?") .+ ('%' : escapeLike p)

-- | Is a column between values?
-- > col BETWEEN l AND r
(><?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- ^ Smaller value.
      -> a -- ^ Larger value.
      -> Condition '[t] -- ^ Condition.
(><?) col l r = cond @'[t] "#" ("#." ++ col ++ " BETWEEN ? AND ?") .+ l .+ r

-- | Is not a column between values?
-- > col NOT BETWEEN l AND r
(<>?) :: forall t a. (RecordWrapper t, Convertible a SqlValue)
      => String -- ^ Column name.
      -> a -- ^ Smaller value.
      -> a -- ^ Larger value.
      -> Condition '[t] -- ^ Condition.
(<>?) col l r = cond @'[t] "#" ("#." ++ col ++ " NOT BETWEEN ? AND ?") .+ l .+ r

-- ----------------------------------------------------------------
-- Conditions using schema
-- ----------------------------------------------------------------

-- | Apply a column name to @r@ for given condition on @t@.
-- > (*-) @From @To id (==? 1)
(*-) :: forall t r db. (WithDB db, RecordWrapper t, RecordWrapper r)
     => (String -> String)
     -> (String -> Condition '[t])
     -> IO (Condition '[t])
(*-) qual c = do
    ts <- readSchema $ getName (Proxy :: Proxy t)
    rs <- readSchema $ getName (Proxy :: Proxy r)
    case L.find (isRef rs) (tableColumns ts) of
            Nothing -> fail $ "Table '" ++ tableName ts ++ "' does not have foreign key to '" ++ tableName rs ++ "'"
            Just rc' -> return $ c (qual $ columnName rc')
    where
        isRef rs c = any ((== tableName rs) . referenceTable) $ relations c
