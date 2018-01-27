{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{- | This module exports types and operators to define table models used in database operations.

    The model definition starts with the definition of an extensible fields where each one corresponds to a column.
    For every pair of field and column, their names must be the same and their types must be convertible.

    > -- CREATE TABLE some_table (id INT, name TEXT, created_at DATETIME);
    > type Columns = '["id" :> Int, "name" :> String, "created_at" :> LocalTime]

    Defined column list is used to define TableModel by associating table name and the role of the model to columns.
    You can use operators (^-) (^@) to select columns because it is often the case that only a part of columns is enough or even preferable.
    Additionally, to define TableModel, you have to declare how the model should be use by specifying ModelRole.
    Type operators (:##), (:++) and (://) are available for the brief definition where lhs is the symbol of table name and rhs is extensible record.

    > type SomeModel = "some_table" :## Record Columns
    > type SomeReference = "some_table" :## Record (Columns ^@ ["id"])
    > type SomeInsert = "some_table" :++ Record Columns
    > type SomeUpdate = "some_table" :// Record (Columns ^- ["created_at"])

    > type SomeInsert = "some_table" :++ Record Columns
-}
module Database.ORM.Model (
    -- * Models
    (:##), (:++), (://)
    , ModelRole(..)
    , SqlValueConstraint
    , TableModel(..)
    , ExtraModel(..)
    , ForWhat(..)
    -- * Relations
    , InnerJoin, LeftJoin, RightJoin
    , JoinType(..)
    , JoinTypeable
    , getJoinType
    -- * Columns
    , type (^-)
    , type (^@)
) where

import GHC.TypeLits
import Data.Convertible
import Data.Proxy
import Data.Extensible
import Database.HDBC

-- ------------------------------------------------------------
-- Models.
-- ------------------------------------------------------------

-- | Defines a model type used to select records or indicate referenced record.
type n :## m = TableModel n Select' m '[]
-- | Defines a model type used to insert a record.
type n :++ m = TableModel n Insert' m '[]
-- | Defines a model type used to update a record.
type n :// m = TableModel n Update' m '[]

-- | This type specifies in the operation type a model should be used.
data ModelRole = Select' -- ^ In updating operation, the model is used as just a reference for foreign key column.
                         -- In selecting operation, the model is to store results of query.
               | Insert' -- ^ Denotes this model will be inserted as a new record.
               | Update' -- ^ Denotes this model will update current record.
                         -- To determine which record should be update, this model must have an identifier column.
               | Extra' -- ^ Denotes this model does not represent an existing table.
               deriving (Show)

-- | A class to indicate the value of extensible field has the convertibility to and from SqlValue.
class (Eq v, Convertible v SqlValue, Convertible SqlValue v) => SqlValueConstraint v where
instance (Eq v, Convertible v SqlValue, Convertible SqlValue v) => SqlValueConstraint v where

-- | Data model representing a record of a table.
-- n is a name of the table and r denotes the operation this model should be used.
data TableModel (n :: Symbol) (r :: ModelRole) m (as :: [*]) :: * where {
    -- | Data constructor of a model.
    Model :: (Forall (KeyValue KnownSymbol SqlValueConstraint) xs)
          => Record xs -- ^ An extensitble record where each field corresponds a column.
          -> TableModel n r (Record xs) as -- ^ Created model.
}

deriving instance (Show m) => Show (TableModel (n :: Symbol) (r :: ModelRole) m as)

-- | This type is prepared for dynamically calculated values except for columns by query.
-- Aggregation operations of grouped records will generate those values (ex. COUNT).
-- By adding ExtraModel into a graph, those values can be obtained in the same way as TableModel.
newtype ExtraModel (xs :: [Assoc Symbol *]) (as :: [*]) = ExtraModel { extra :: Record xs }

-- | Declares methods to define if the instance model should accept each operation.
class ForWhat m where
    forInsert :: m -> Bool
    forInsert _ = False
    forUpdate :: m -> Bool
    forUpdate _ = False

instance ForWhat (TableModel n r m as) where
instance ForWhat (TableModel n Insert' m as) where
    forInsert _ = True
instance ForWhat (TableModel n Update' m as) where
    forUpdate _ = True

instance ForWhat (ExtraModel xs as) where
    forInsert _ = False
    forUpdate _ = False

-- ------------------------------------------------------------
-- Relations.
-- ------------------------------------------------------------

-- | A type representing inner join.
data InnerJoin
-- | A type representing left join.
data LeftJoin
-- | A type representing right join.
data RightJoin

-- | Join types used in value level operations.
data JoinType = InnerJoinType | LeftJoinType | RightJoinType | DefaultJoinType deriving (Show)

-- | This type synonym is a constraint the the join type can be obtained from the type level list `rs` which can contain arbitrary types.
-- We need the constraint because join type appears in type level list of the type constructor of `EdgeT`.
type JoinTypeable (rs :: [*]) = (GetJoinType (GetJoinType' rs))

-- | Returns a value of join type obtained from an arbitrary type level list.
getJoinType :: forall rs. (GetJoinType (GetJoinType' rs))
            => Proxy (rs :: [*]) -- ^ Proxy to type level list.
            -> JoinType -- ^ A join type.
getJoinType _ = joinType (Proxy :: Proxy (GetJoinType' rs))

-- | This class declares function to convert each join type from a proxy to a value.
-- Because the join type is defined as a type in a model graph, we need this conversion to handle with it in value level operations.
class GetJoinType j where
    joinType :: Proxy j -> JoinType
instance GetJoinType InnerJoin where
    joinType _ = InnerJoinType
instance GetJoinType LeftJoin where
    joinType _ = LeftJoinType
instance GetJoinType RightJoin where
    joinType _ = RightJoinType

-- | Selects a join type from type level list including arbitrary types.
type family GetJoinType' (rs :: [*]) :: * where
    GetJoinType' ('[]) = InnerJoin
    GetJoinType' (InnerJoin ': rs) = InnerJoin
    GetJoinType' (LeftJoin ': rs) = LeftJoin
    GetJoinType' (RightJoin ': rs) = RightJoin
    GetJoinType' (r ': rs) = GetJoinType' rs

-- ------------------------------------------------------------
-- Columns.
-- ------------------------------------------------------------

-- | This operator removes columns whose names are contained in ys from xs.
type family (^-) (xs :: [k]) (ys :: [Symbol]) :: [k] where
    '[] ^- ys = '[]
    xs ^- '[] = xs
    (x ': '[]) ^- (y ': '[]) = ((^==) x y '[] '[x]) 
    (x ': xs) ^- (y ': ys) = ((^==) x y '[] ((x ': '[]) ^- ys)) ++ (xs ^- (y ': ys))

-- | This operator selects columns whose name are contained in ys from xs.
type family (^@) (xs :: [k]) (ys :: [Symbol]) :: [k] where
    '[] ^@ ys = '[]
    xs ^@ '[] = '[]
    (x ': xs) ^@ (y ': ys) = ((^==) x y '[x] (xs ^@ (y ': '[]))) ++ ((x ': xs) ^@ ys)

-- | Private classes to define equalities between extensible records and symbols.
class Column c where
    type ColName c :: Symbol
    colName :: p c -> String

instance (KnownSymbol k) => Column (k :> v) where
    type ColName (k :> v) = k
    colName _ = symbolVal (Proxy :: Proxy k)

instance (KnownSymbol k) => Column (k :: Symbol) where
    type ColName k = k
    colName _ = symbolVal (Proxy :: Proxy k)

class ColumnEquality k where
    type (^==) (x :: k) (y :: k2) (t :: [k]) (f :: [k]) :: [k]
    type (?:) (o :: Ordering) (t :: [k]) (f :: [k]) :: [k]

instance (Column c) => ColumnEquality c where
    type (^==) x y t f = (?:) (CmpSymbol (ColName x) (ColName y)) t f
    type (?:) EQ t f = t
    type (?:) GT t f = f
    type (?:) LT t f = f
