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

module Database.ORM.Query where

import qualified Data.List as L
import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Database.ORM.Record
import Database.ORM.Utility

-- ------------------------------------------------------------
-- Column arrangements
-- ------------------------------------------------------------

data (@:) m (a :: Symbol)
data (@@) m (e :: Symbol)

data AliasModels (ms :: [*]) = AliasModels [String]

aliasModels :: forall ms. (AllRecord (ms :: [*]))
            => [String]
            -> AliasModels ms
aliasModels aliases = AliasModels aliases :: AliasModels ms

class AliasModel m where
    columnExpressions :: proxy m
                      -> [String]

    getAlias :: proxy m
             -> String

instance (RecordWrapper m, KnownSymbol a) => AliasModel (m @: (a :: Symbol)) where
    columnExpressions _ = map ((alias ++ ".") ++) $ fieldNames (Proxy :: Proxy (RW'Type m))
        where
            alias = symbolVal (Proxy :: Proxy a)
    getAlias _ = symbolVal (Proxy :: Proxy a)

instance (RecordWrapper m, KnownSymbol e) => AliasModel (m @@ (e :: Symbol)) where
    columnExpressions _ = [symbolVal (Proxy :: Proxy e)]
    getAlias _ = symbolVal (Proxy :: Proxy e)

class GenerateColumns p (ms :: [*]) where
    generateColumns :: p ms
                    -> [[String]]

    getAliases :: p ms
               -> [String]

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
-- Query holder
-- ------------------------------------------------------------

{- | Generate query string of given number of placeholders separated by comma.
-}
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

data OrderBy (ts :: [*]) = OrderBy (Proxy ts) [String] [SortType]

orderBy :: forall t. (RecordWrapper t)
        => String
        -> SortType
        -> OrderBy '[t]
orderBy c st = OrderBy (Proxy :: Proxy '[t]) [c] [st]

(../) :: OrderBy '[]
(../) = OrderBy (Proxy :: Proxy '[]) [] []

(./) :: forall ts us. OrderBy ts
     -> OrderBy us
     -> OrderBy (Concat ts us)
(./) (OrderBy _ cs1 sts1) (OrderBy _ cs2 sts2) =  OrderBy (Proxy :: Proxy (Concat ts us)) (cs1 ++ cs2) (sts1 ++ sts2)

formatOrderBy :: (ElemIndexes ts as)
              => OrderBy ts
              -> Proxy (as :: [*])
              -> [String]
              -> [(String, SortType)]
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

-- | Gets default sorting.
unordered = [] :: [(String, SortType)]

-- ------------------------------------------------------------
-- Others
-- ------------------------------------------------------------

-- | Type synonym where each value corresponds to LIMIT and OFFSET value respectively.
type LimitOffset = Maybe (Int, Int)
