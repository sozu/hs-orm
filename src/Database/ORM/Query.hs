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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.ORM.Query (
    -- * Columns
    type (@:), type (@@)
    , AliasModels(..)
    , aliasModels
    , AliasModel(..)
    , GenerateColumns(..)
    -- * Place holder
    , holder
    -- * Sorting
    , SortType(..)
    , OrderBy(..)
    , orderBy
    , (../)
    , (./)
    , formatOrderBy
    , FormattedOrderBy(..)
    , LimitOffset
) where

import qualified Data.List as L
import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Database.ORM.Record
import Database.ORM.Utility

-- ------------------------------------------------------------
-- Columns
-- ------------------------------------------------------------

{- | Phantom type to declare a model and an alias for it.

    This type generate expressions of field names of the model qualified with the alias.

    > type A = "table" :## Record '["col1" :> Int, "col2" :> String]
    > columnExpressions (Proxy :: Proxy (A @: "t")) == "t.col1, t.col2"
-}
data (@:) m (a :: Symbol)

-- | Phantom type to declare a model and its column expression.
data (@@) m (e :: Symbol)

-- | Phantom type to declare models and their aliases. This type is introduced mainly for internal use.
data AliasModels (ms :: [*]) = AliasModels [String]

aliasModels :: forall ms. (AllRecord (ms :: [*]))
            => [String]
            -> AliasModels ms
aliasModels aliases = AliasModels aliases :: AliasModels ms

-- | Declares method to return alias or qualified expressions of the model type (m).
class AliasModel m where
    -- | Get column expression of the model type.
    columnExpressions :: proxy m -- ^ Model type.
                      -> [String] -- ^ Qualified column expressions.

    -- | Get alias string of the model type.
    getAlias :: proxy m -- ^ Model type.
             -> String -- ^ Alias of the type.

instance {-# OVERLAPPABLE #-} (RecordWrapper m) => AliasModel m where
    columnExpressions _ = fieldNames (Proxy :: Proxy (RW'Type m))
    getAlias _ = ""

instance (RecordWrapper m, KnownSymbol a) => AliasModel (m @: (a :: Symbol)) where
    columnExpressions _ = map ((alias ++ ".") ++) $ fieldNames (Proxy :: Proxy (RW'Type m))
        where
            alias = symbolVal (Proxy :: Proxy a)
    getAlias _ = symbolVal (Proxy :: Proxy a)

instance (RecordWrapper m, KnownSymbol e) => AliasModel (m @@ (e :: Symbol)) where
    columnExpressions _ = [symbolVal (Proxy :: Proxy e)]
    getAlias _ = symbolVal (Proxy :: Proxy e)

-- | Declares method to return aliases or qualified expression of the model types @ms@.
class GenerateColumns p (ms :: [*]) where
    -- | Get list of column expressions of model types.
    generateColumns :: p ms -- ^ Some type @p@ constructed with model types @ms@.
                    -> [[String]] -- ^ List where each item denotes column expressions of the model type.

    -- | Get aliases of model types.
    getAliases :: p ms -- ^ Some type @p@ constructed with model types @ms@.
               -> [String] -- ^ Aliases of model types.

instance GenerateColumns p '[] where
    generateColumns _ = []
    getAliases _ = []

instance (AliasModel m, GenerateColumns Proxy ms) => GenerateColumns Proxy (m ': ms) where
    generateColumns _ = columnExpressions (Proxy :: Proxy m) : generateColumns (Proxy :: Proxy ms)
    getAliases _ = getAlias (Proxy :: Proxy m) : getAliases (Proxy :: Proxy ms)

instance (RecordWrapper m, GenerateColumns AliasModels ms) => GenerateColumns AliasModels (m ': ms) where
    generateColumns (AliasModels (a : as)) = map ((a ++ ".") ++) (fieldNames (Proxy :: Proxy (RW'Type m))) : generateColumns (AliasModels as :: AliasModels ms)
    getAliases (AliasModels as) = as

-- ------------------------------------------------------------
-- Place holder
-- ------------------------------------------------------------

-- | Generates query string of given number of placeholders separated by comma.
holder :: Int -- ^ The number of placeholders.
       -> String -- ^ Generated query string.
holder n = L.intercalate ", " (L.replicate n "?")

-- ------------------------------------------------------------
-- Sorting
-- ------------------------------------------------------------

-- | Predefined sorting orders.
data SortType = ASC -- ^ Ascending order.
              | DESC -- ^ Descending order.
              deriving (Eq, Show)

-- | Denotes orders of rows where prior type has higher priority.
data OrderBy (ts :: [*]) = OrderBy (Proxy ts) [String] [SortType]

{- | Create a sorting rule by model type, column and sorting order.

    Model type should be supplied explicitly by TypeApplications extension.

    > let o = orderBy @A "cola" ASC ./ orderBy @B "colb" DESC
    > fomatOrderBy o (Proxy :: Proxy '[A]) ["ta"] == [("ta.cola", ASC), ("tb.colb", DESC)]
-}
orderBy :: forall t. (RecordWrapper t)
        => String -- ^ Column name used for sorting.
        -> SortType -- ^ Sorting order.
        -> OrderBy '[t] -- ^ Sorting rule of the model.
orderBy c st = OrderBy (Proxy :: Proxy '[t]) [c] [st]

-- | Creates empty sorting rule.
(../) :: OrderBy '[]
(../) = OrderBy (Proxy :: Proxy '[]) [] []

-- | Joins two sorting rules. First one is higer priority than second.
(./) :: forall ts us. OrderBy ts -- ^ A sorting rule.
     -> OrderBy us -- ^ Another sorting rule.
     -> OrderBy (Concat ts us) -- ^ Joined sorting rule.
(./) (OrderBy _ cs1 sts1) (OrderBy _ cs2 sts2) =  OrderBy (Proxy :: Proxy (Concat ts us)) (cs1 ++ cs2) (sts1 ++ sts2)

-- | Formats order by clause for a query by inserting aliases according to model types.
formatOrderBy :: (ElemIndexes ts as)
              => OrderBy ts -- ^ Sorting rule.
              -> Proxy (as :: [*]) -- ^ List of model types arranged in the same order as aliases.
              -> [String] -- ^ Aliases of model types.
              -> [(String, SortType)] -- ^ List of pairs of column name and sorting order.
formatOrderBy (OrderBy pts cs sts) p aliases = map (\(t, c, st) -> (t ++ "." ++ c, st)) $ zip3 tables cs sts
    where
        tables = map (aliases !!) (elemIndexes pts p)

-- | Declares methods to get column name for sort and the order.
class FormattedOrderBy o where
    -- | Gets a column name used for sorting.
    orders :: o -- ^ Sorting information.
           -> String -- ^ Column name.

instance FormattedOrderBy [(String, SortType)] where
    orders o = L.intercalate ", " $ map (\(c ,st) -> c ++ " " ++ show st) o

-- ------------------------------------------------------------
-- Others
-- ------------------------------------------------------------

-- | Type synonym where each value corresponds to LIMIT and OFFSET value respectively.
type LimitOffset = Maybe (Int, Int)
