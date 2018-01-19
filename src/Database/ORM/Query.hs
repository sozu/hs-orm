module Database.ORM.Query (
    holder
) where

import qualified Data.List as L

{- | Generate query string of given number of placeholders separated by comma.
-}
holder :: Int -- ^ The number of placeholders.
       -> String -- ^ Generated query string.
holder n = L.intercalate ", " (L.replicate n "?")