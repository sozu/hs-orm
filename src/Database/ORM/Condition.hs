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
      , ConditionFormat(..)
      , cond
      , (..?)
      , (.+)
      , (.&), (.|), (.&?), (.|?), (.=&), (.=|)
      , formatCondition
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

--data Condition (ts :: [*]) = Condition [String] (Proxy ts) [SqlValue]
data Condition (ts :: [*]) = Condition ConditionFormat (Proxy ts) [SqlValue]

-- | Holds strings consisting in a condition and consuming number of types.
data ConditionFormat = ConditionFormat [String] Int
                     | AndFormat ConditionFormat ConditionFormat
                     | OrFormat ConditionFormat ConditionFormat
                     deriving (Show, Eq)

-- cond @'[A.Model, B.Model] "??" "3 * ??.val < 7 + ??.d"
cond :: forall ts. (KnownNat (Length ts))
     => String
     -> String
     -> Condition ts
cond rep fmt = Condition (ConditionFormat (split fmt ("", [])) len) (Proxy :: Proxy ts) []
    where
        len :: Int
        len = fromInteger $ natVal (Proxy :: Proxy (Length ts))
        split :: String -> (String, [String]) -> [String]
        split s (cs, ss)
            | s == ""              = L.reverse $ (L.reverse cs):ss
            | rep `L.isPrefixOf` s = split (L.drop (length rep) s) ("", (L.reverse cs):ss)
            | otherwise            = split (tail s) (head s:cs, ss)

(..?) :: Condition '[]
(..?) = cond @'[] "" ""

(.+) :: (Convertible a SqlValue)
     => Condition ts
     -> a
     -> Condition ts
(.+) (Condition fmt pts vs) v = Condition fmt pts (vs ++ [convert v])

-- | Concatenate two conditions by 'AND' operator.
(.&) :: forall ts us. Condition ts -- ^ First condition.
     -> Condition us -- ^ Second condition.
     -> Condition (Concat ts us) -- ^ Concatenated condition.
(.&) (Condition fmt1 pts1 vs1) (Condition fmt2 pts2 vs2) = Condition (AndFormat fmt1 fmt2) (Proxy :: Proxy (Concat ts us)) (vs1 ++ vs2)

-- | Concatenate two conditions by 'OR' operator.
(.|) :: forall ts us. Condition ts -- ^ First condition.
     -> Condition us -- ^ Second condition.
     -> Condition (Concat ts us) -- ^ Concatenated condition.
(.|) (Condition fmt1 pts1 vs1) (Condition fmt2 pts2 vs2) = Condition (OrFormat fmt1 fmt2) (Proxy :: Proxy (Concat ts us)) (vs1 ++ vs2)

(.=&) :: forall t. Condition '[t]
      -> Condition '[t]
      -> Condition '[t]
(.=&) (Condition f1 p vs1) (Condition f2 _ vs2) = Condition (AndFormat f1 $ zeroConsume f2) p (vs1 ++ vs2)

(.=|) :: forall t. Condition '[t]
      -> Condition '[t]
      -> Condition '[t]
(.=|) (Condition f1 p vs1) (Condition f2 _ vs2) = Condition (OrFormat f1 $ zeroConsume f2) p (vs1 ++ vs2)

zeroConsume :: ConditionFormat -> ConditionFormat
zeroConsume (ConditionFormat ss n) = ConditionFormat ss 0
zeroConsume (AndFormat f1 f2)      = AndFormat (zeroConsume f1) (zeroConsume f2)
zeroConsume (OrFormat f1 f2)       = OrFormat (zeroConsume f1) (zeroConsume f2)

(.&?) :: forall ts us. (KnownNat (Length us))
      => Condition ts
      -> Maybe (Condition us)
      -> Condition (Concat ts us)
(.&?) c (Just c') = c .& c'
(.&?) c _         = c .& cond @us "" ""

(.|?) :: forall ts us. (KnownNat (Length us))
      => Condition ts
      -> Maybe (Condition us)
      -> Condition (Concat ts us)
(.|?) c (Just c') = c .| c'
(.|?) c _         = c .| cond @us "" ""

formatCondition :: (ElemIndexes ts as)
                => Condition ts
                -> Proxy (as :: [*])
                -> [String]
                -> (String, [SqlValue])
formatCondition (Condition fmt pts vs) p aliases = (fst $ format' fmt 0, vs)
    where
        tables :: [String]
        tables = map (aliases !!) (elemIndexes pts p)

        format' :: ConditionFormat -> Int -> (String, Int)
        format' (ConditionFormat ss n) index = let alias i = tables !! (if n == 0 then index else if i < n then (index + i) else (index + n - 1))
                                               in (foldl (\a (s, i) -> a ++ alias i ++ s) (head ss) $ zip (tail ss) [0..], index + n)
        format' (AndFormat fmt1 fmt2) index  = let (s1, i1) = format' fmt1 index
                                                   (s2, i2) = format' fmt2 i1
                                               in (merge s1 s2 " AND ", i2)
        format' (OrFormat fmt1 fmt2) index   = let (s1, i1) = format' fmt1 index
                                                   (s2, i2) = format' fmt2 i1
                                               in (merge s1 s2 " OR ", i2)

        merge :: String -> String -> String -> String
        merge "" s2 _  = s2
        merge s1 "" _  = s1
        merge s1 s2 op = L.concat ["(", s1, ")", op, "(", s2, ")"]

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
