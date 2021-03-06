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
    (:##), (:++), (://), (:**)
    , ModelRole(..)
    , SqlValueConstraint
    , TableModel(..)
    , type (:^+), type (:^-), type (:^@)
    , type (=#), type (=+), type (=/), type (=*)
    , ExtraModel(..)
    , RoleForWhat(..)
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

-- | Defines a model type used to select records.
type n :## m = TableModel n Select' m '[]
-- | Defines a model type used to insert a record.
type n :++ m = TableModel n Insert' m '[]
-- | Defines a model type used to update a record.
type n :// m = TableModel n Update' m '[]
-- | Defines a model type used to indicate relations of tables.
type n :** m = TableModel n Relate' m '[]

-- | This type specifies in the operation type a model should be used.
data ModelRole = Select' -- ^ Denotes this model will be used just to represent selected records, not update tables.
               | Insert' -- ^ Denotes this model will be inserted as a new record.
               | Update' -- ^ Denotes this model will update current record.
               | Relate' -- ^ Denotes this model will just determine tables to join, not be instantiated by seleced records.
               | Extra' -- ^ Denotes this model does not represent an existing table.
               deriving (Show)

-- | A class to indicate the value of extensible field has the convertibility to and from SqlValue.
class (Eq v, Convertible v SqlValue, Convertible SqlValue v) => SqlValueConstraint v where
instance (Eq v, Convertible v SqlValue, Convertible SqlValue v) => SqlValueConstraint v where

-- | Set model role of a @TableModel@ to @Select'@.
type family (=#) m :: * where
    (=#) (TableModel n _ m as) = TableModel n Select' m as
    (=#) m = m
-- | Set model role of a @TableModel@ to @Insert'@.
type family (=+) m :: * where
    (=+) (TableModel n _ m as) = TableModel n Insert' m as
    (=+) m = m
-- | Set model role of a @TableModel@ to @Update'@.
type family (=/) m :: * where
    (=/) (TableModel n _ m as) = TableModel n Update' m as
    (=/) m = m
-- | Set model role of a @TableModel@ to @Relate'@.
type family (=*) (m :: *) :: * where
    (=*) (TableModel n _ m as) = TableModel n Relate' m as
    (=*) m = m

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

-- | Remove columns from the model by their names.
type family (:^-) m (ys :: [Symbol]) :: * where
    TableModel n r (Record xs) as :^- ys = TableModel n r (Record (xs ^- ys)) as
    ExtraModel xs as :^- ys = ExtraModel (xs ^- ys) as

-- | Select columns of the model by their names.
type family (:^@) m (ys :: [Symbol]) :: * where
    TableModel n r (Record xs) as :^@ ys = TableModel n r (Record (xs ^@ ys)) as
    ExtraModel xs as :^@ ys = ExtraModel (xs ^@ ys) as

-- | Add an appendix type to a model.
type family (:^+) m a :: * where
    TableModel n r m as :^+ a = TableModel n r m (a ': as)
    ExtraModel xs as :^+ a = ExtraModel xs (a ': as)

-- | Declares methods to get availabilities for insertion or update from a @ModelRole@ type.
class RoleForWhat (r :: k) where
    -- | Checks the role can be used for inserting operation.
    roleForInsert :: Proxy r -- ^ A @ModelRole@ type.
                  -> Bool -- ^ Availability for the inserting operation.
    -- | Checks the role can be used for updating operation.
    roleForUpdate :: Proxy r -- ^ A @ModelRole@ type.
                  -> Bool -- ^ Availability for the updating operation.
    roleForRelate :: Proxy r -- ^ A @ModelRole@ type.
                  -> Bool -- ^ Availability for the updating operation.

instance RoleForWhat Select' where
    roleForInsert _ = False
    roleForUpdate _ = False
    roleForRelate _ = False
instance RoleForWhat Insert' where
    roleForInsert _ = True
    roleForUpdate _ = False
    roleForRelate _ = False
instance RoleForWhat Update' where
    roleForInsert _ = False
    roleForUpdate _ = True
    roleForRelate _ = False
instance RoleForWhat Relate' where
    roleForInsert _ = False
    roleForUpdate _ = False
    roleForRelate _ = True
instance RoleForWhat Extra' where
    roleForInsert _ = False
    roleForUpdate _ = False
    roleForRelate _ = False

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
