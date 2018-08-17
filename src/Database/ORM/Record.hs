{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.Record (
    -- * Records
    KeyConstraint
    , RecordWrapper(..)
    , Rewrap(..)
    , rw'type
    , FieldNames(..)
    -- * Fields
    , recordFields
    , recordValues
    , fieldValue
    , setFieldValue
    -- * Relations
    , resolveRelations
    , EdgeMap(..)
    , AllRecord
    -- * Identity
    , Identifiable(..)
    , PK(..)
    , FK(..)
    , ColExp
    , GetExpression(..)
) where

import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Functor.Identity
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Rep
import Control.Comonad
import Data.Proxy
import Data.Aeson (ToJSON(..))
import Data.Extensible
import Data.Extensible.Internal
import Data.Extensible.Internal.Rig
import Data.Model.Graph
import Database.HDBC
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Utility

-- ------------------------------------------------------------
-- Records.
-- ------------------------------------------------------------

-- | This class is introduces to declare the constraint just for the key of extensible record.
class (pk (AssocKey kv)) => KeyConstraint pk kv where
instance (KnownSymbol k) => KeyConstraint KnownSymbol (k :> v) where

-- | Declares methods to deal with @a@ as a record model of table.
class ( FieldNames (RW'Type a), KnownSymbol (RW'Name a)
      , Forall (KeyConstraint KnownSymbol) (RW'Type a)
      , Forall (KeyValue KnownSymbol SqlValueConstraint) (RW'Type a)
      , GetExpression (RW'Spec a)
      ) => RecordWrapper a where
    -- | Determines a name of the table in the form of Symbol.
    type RW'Name a :: Symbol
    -- | Determines a extensible fields corresponding to records of the table.
    type RW'Type a :: [Assoc Symbol *]
    -- | Determines a ModelRole this record conforms to.
    type RW'Role a :: ModelRole
    type RW'Spec a :: [*]

    newRecord :: [SqlValue] -> a

    -- | Get an extensible record from this record model.
    getRecord :: a -- ^ A record model.
              -> Record (RW'Type a) -- ^ An extensible record.

    wrapRecord :: Record (RW'Type a)
               -> a

    -- | Replace the extensible record of this record model.
    updateRecord :: a -- ^ A record model.
                 -> Record (RW'Type a) -- ^ New extensible record replacing current one.
                 -> a -- ^ New record model with replaced extensible record.

    -- | Get a name of the table.
    getName :: proxy a -- ^ Proxy to this record model type.
            -> String -- ^ Table name of @a@.
    getName p = symbolVal (Proxy :: Proxy (RW'Name a))

instance (RecordWrapper a, ToJSON (Record (RW'Type a))) => ToJSON a where
    toJSON = toJSON . getRecord
    toEncoding = toEncoding . getRecord

instance ( IsLabel x (p rep (f rep) -> p s (f s))
         , rep ~ Repr (Field Identity) (x :> v)
         , Profunctor p
         , Functor f
         , Associate x v (RW'Type r)
         , Corepresentable p
         , Comonad (Corep p)
         , RecordWrapper r
         , s ~ (Field Identity :* RW'Type r)
         , r ~ TableModel n rr (Record xs) as
         ) => IsLabel (x :: Symbol) (p rep (f rep) -> p (TableModel n rr (Record (xs :: [Assoc Symbol *])) as) (f (TableModel n rr (Record xs) as))) where
    fromLabel = app <$> (itemAssoc (Proxy :: Proxy x) :: Optic' p f (Field Identity :* xs) (Repr (Field Identity) (x :> v)))
        where
            app :: p s (f s) -> p r (f r)
            app = dimap getRecord (wrapRecord <$>)

instance ( IsLabel x (p rep (f rep) -> p s (f s))
         , rep ~ Repr (Field Identity) (x :> v)
         , Profunctor p
         , Functor f
         , Associate x v (RW'Type r)
         , Corepresentable p
         , Comonad (Corep p)
         , RecordWrapper r
         , s ~ (Field Identity :* RW'Type r)
         , r ~ ExtraModel xs as
         ) => IsLabel (x :: Symbol) (p rep (f rep) -> p (ExtraModel (xs :: [Assoc Symbol *]) as) (f (ExtraModel xs as))) where
    fromLabel = app <$> (itemAssoc (Proxy :: Proxy x) :: Optic' p f (Field Identity :* xs) (Repr (Field Identity) (x :> v)))
        where
            app :: p s (f s) -> p r (f r)
            app = dimap getRecord (wrapRecord <$>)

rw'type :: forall a. (RecordWrapper a)
        => Proxy a
        -> Proxy (RW'Type a)
rw'type _ = Proxy :: Proxy (RW'Type a)

class FieldNames (as :: [Assoc Symbol *]) where
    fieldNames :: proxy as -> [String]

instance FieldNames '[] where
    fieldNames _ = []

instance (KnownSymbol k, FieldNames as) => FieldNames ((k :> v) ': as) where
    fieldNames _ = symbolVal (Proxy :: Proxy k) : fieldNames (Proxy :: Proxy as)

instance ( FieldNames xs
         , KnownSymbol n
         , Forall (KeyConstraint KnownSymbol) xs
         , Forall (KeyValue KnownSymbol SqlValueConstraint) xs
         , GetExpression as
         ) => RecordWrapper (TableModel n r (Record xs) as) where
    type RW'Name (TableModel n r (Record xs) as) = n
    type RW'Type (TableModel n r (Record xs) as) = xs
    type RW'Role (TableModel n r (Record xs) as) = r
    type RW'Spec (TableModel n r (Record xs) as) = as
    getRecord (Model m) = m
    wrapRecord = Model
    updateRecord (Model m) v = Model v
    newRecord vs = Model $ htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol SqlValueConstraint))
                            $ \m -> Field $ pure (fromSql (vs !! getMemberId m))

instance ( FieldNames xs
         , Forall (KeyConstraint KnownSymbol) xs
         , Forall (KeyValue KnownSymbol SqlValueConstraint) xs
         , GetExpression as
         ) => RecordWrapper (ExtraModel xs as) where
    type RW'Name (ExtraModel xs as) = ""
    type RW'Type (ExtraModel xs as) = xs
    type RW'Role (ExtraModel xs as) = 'Extra'
    type RW'Spec (ExtraModel xs as) = as
    getRecord (ExtraModel r) = r
    wrapRecord = ExtraModel
    updateRecord (ExtraModel _) r = ExtraModel r
    newRecord vs = ExtraModel $ htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol SqlValueConstraint))
                                $ \m -> Field $ pure (fromSql (vs !! getMemberId m))

class (RecordWrapper s, RecordWrapper t) => Rewrap s t where
    (~/) :: s -> (Record (RW'Type s) -> Record (RW'Type t)) -> t

instance ( RecordWrapper (TableModel n r1 (Record xs) as)
         , RecordWrapper (TableModel n r2 (Record ys) bs)
         , FieldNames xs
         , FieldNames ys
         ) => Rewrap (TableModel n r1 (Record xs) as) (TableModel n r2 (Record ys) bs) where
    (~/) s f = Model (f $ getRecord s)
instance ( RecordWrapper (ExtraModel xs as)
         , RecordWrapper (ExtraModel ys bs)
         ) => Rewrap (ExtraModel xs as) (ExtraModel ys bs) where
    (~/) s f = ExtraModel (f $ getRecord s)

-- ------------------------------------------------------------
-- Fields.
-- ------------------------------------------------------------

-- | Get names of fields from an extensible record.
-- The order of fields in returned list is same as the definition of the record.
recordFields :: (Forall (KeyConstraint KnownSymbol) xs)
             => Record xs -- ^ An extensible record.
             -> [String] -- ^ Field names.
recordFields = hfoldMapFor (Proxy :: Proxy (KeyConstraint KnownSymbol)) (\v -> [symbolVal (proxyAssocKey v)])

-- | Get values of fields from an extensible record.
-- The order of fields in returned list is same as the definition of the record.
recordValues :: (Forall (KeyValue KnownSymbol SqlValueConstraint) xs)
             => (Record xs) -- ^ An extensible record.
             -> [SqlValue] -- ^ A list of values where each @SqlValue@ is converted from the value of a field respectively.
recordValues = hfoldMapFor (Proxy :: Proxy (KeyValue KnownSymbol SqlValueConstraint)) ((:[]) . toSql . runIdentity . getField)

-- | Get a value of field specified by string.
fieldValue :: (Forall (KeyValue KnownSymbol SqlValueConstraint) xs)
           => Record xs -- ^ A record having the field.
           -> String -- ^ Field name.
           -> Maybe SqlValue -- ^ @Just (field value)@ if it exists. otherwise Nothing.
fieldValue r n = getFirst $ accessor r
    where
        accessor = hfoldMapFor
                    (Proxy :: Proxy (KeyValue KnownSymbol SqlValueConstraint))
                    (\f -> if n == symbolVal (proxyAssocKey f) then First (Just (toSql $ runIdentity $ getField f)) else First Nothing)

-- | Set a value to the field specified by string.
setFieldValue :: (Forall (KeyValue KnownSymbol SqlValueConstraint) xs)
              => Record xs -- ^ A record having the field.
              -> String -- ^ Field name.
              -> SqlValue -- ^ A @SqlValue@ converted and set to the field.
              -> Record xs -- ^ New record where the field is updated.
setFieldValue r n value = htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol SqlValueConstraint))
                            $ \m -> let f = hlookup m r
                                    in if symbolVal (proxyAssocKey f) == n
                                        then Field $ pure (fromSql value)
                                        else f

-- ------------------------------------------------------------
-- Relations.
-- ------------------------------------------------------------

-- | Gets relation informations of an edge in the graph.
resolveRelations :: forall a b g. (GraphContainer g a, GraphContainer g b, GraphContainer g (Edge a b), RecordWrapper a, RecordWrapper b)
                 => TableMeta -- ^ A schema of the table from which the edge starts.
                 -> g -- ^ A graph having the edge.
                 -> Proxy (Edge a b) -- ^ A proxy representing a type of the edge.
                 -> Maybe (String, Cursor a -> Maybe SqlValue) -- ^ Relation information of the edge. First element is a name of referencing column.
                                                               -- Second element is a function to get referenced value by the cursor to referencing node.
resolveRelations ta graph p = case relcols of
                                ((n, r):_) -> Just (n, accessor (referenceColumn r))
                                _ -> Nothing
    where
        tableName = symbolVal (Proxy :: Proxy (RW'Name b))
        -- TODO
        -- Just take the first relation from a to b.
        -- This implementation can't deal with multiple relations between a pair of tables.
        relcols = relationsTo ta tableName
        accessor k c = case map (\cb -> (cb @< graph)) (c @*< graph :: [Cursor b]) of
                        [v] -> let r = getRecord v in fieldValue r k
                        _ -> Nothing

-- | This class declares a method to traverse edge list and apply a function to each edge.
class (GraphContainer g a, RecordWrapper a) => EdgeMap g xs a where
    -- | Map function to types of edges starting from a node.
    mapEdges :: g -- ^ A graph
             -> Proxy a -- ^ A proxy specifying the type of node.
             -> Proxy xs -- ^ A type level list of types of edges.
             -> (forall (b :: *). (GraphContainer g b, RecordWrapper b, GraphContainer g (Edge a b)) => g -> Proxy (Edge a b) -> r) -- ^ A function to apply each edge type.
             -> [r] -- ^ A list of function results.

instance (GraphContainer g a, GraphContainer g b, GraphContainer g (Edge a b), EdgeMap g xs a, RecordWrapper a, RecordWrapper b) => EdgeMap g (xs :><: Edge a b) a where
    mapEdges graph p _ f = f graph (Proxy :: Proxy (Edge a b)) : mapEdges graph p (Proxy :: Proxy xs) f

instance (GraphContainer g a, RecordWrapper a, EdgeMap g xs a) => EdgeMap g (xs :><: x) a where
    mapEdges graph p _ f = mapEdges graph p (Proxy :: Proxy xs) f

instance (GraphContainer g a, GraphContainer g b, RecordWrapper a, GraphContainer g (Edge a b), RecordWrapper b) => EdgeMap g (Edge a b) a where
    mapEdges graph p _ f = f graph (Proxy :: Proxy (Edge a b)) : []

instance (GraphContainer g a, RecordWrapper a) => EdgeMap g (Graph x) a where
    mapEdges graph p _ f = []

type family AllRecord (as :: [*]) :: Constraint where
    AllRecord '[] = ()
    AllRecord (a ': '[]) = RecordWrapper a
    AllRecord (a ': as) = (RecordWrapper a, AllRecord as)

-- ------------------------------------------------------------
-- Identity.
-- ------------------------------------------------------------

class (RecordWrapper a, ReadSymbols (RW'Key a)) => Identifiable a where
    type RW'Key a :: [Symbol]
    type RW'KeyTypes a :: [*]

    getKeyNames :: proxy a
                -> [String]
    getKeyNames p = readSymbols (Proxy :: Proxy (RW'Key a))

    ident :: a
          -> a
          -> Bool
    ident r1 r2 = keyValues r1 == keyValues r2
        where
            keys = getKeyNames (Proxy :: Proxy a)
            keyValues r = map (fieldValue $ getRecord r) keys

class ReadSymbols rs where
    readSymbols :: proxy rs
                -> [String]

instance ReadSymbols '[] where
    readSymbols _ = []

instance (KnownSymbol a, ReadSymbols as) => ReadSymbols (a ': as :: [Symbol]) where
    readSymbols _ = symbolVal (Proxy :: Proxy a) : readSymbols (Proxy :: Proxy as)

data PK (pk :: [Symbol])
data FK (fk :: [Assoc Symbol *])

instance (RecordWrapper (TableModel n r m as), ReadSymbols (FindPK as)) => Identifiable (TableModel n r m as) where
    type RW'Key (TableModel n r m as) = FindPK as
    type RW'KeyTypes (TableModel n r m as) = FindFieldTypes (FindPK as) (Concat (RW'Type (TableModel n r m as)) (FindFK as))

type family FindPK (as :: [*]) :: [Symbol] where
    FindPK (PK pk ': as) = pk
    FindPK (a ': as) = FindPK as

type family FindFK (as :: [*]) :: [Assoc Symbol *] where
    FindFK '[] = '[]
    FindFK (FK fk ': as) = fk
    FindFK (a ': as) = FindFK as

type family FindFieldTypes (ss :: [Symbol]) (fs :: [Assoc Symbol *]) :: [*] where
    FindFieldTypes '[] _ = '[]
    FindFieldTypes (s ': ss) fs = FindFieldType s fs ': FindFieldTypes ss fs

type family FindFieldType (s :: Symbol) (fs :: [Assoc Symbol *]) :: * where
    FindFieldType s ('(:>) s t ': fs) = t
    FindFieldType s (f ': fs) = FindFieldType s fs

data ColExp (col :: Symbol) (exp :: Symbol)

class GetExpression (as :: [*]) where
    getExpression :: Proxy as -> [(String, String)]

instance GetExpression '[] where
    getExpression _ = []

instance (KnownSymbol col, KnownSymbol exp, GetExpression as) => GetExpression (ColExp col exp ': as) where
    getExpression _ = (symbolVal (Proxy :: Proxy col), symbolVal (Proxy :: Proxy exp)) : getExpression (Proxy :: Proxy as)

instance {-# OVERLAPPABLE #-} (GetExpression as) => GetExpression (a ': as) where
    getExpression _ = getExpression (Proxy :: Proxy as)