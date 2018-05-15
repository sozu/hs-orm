{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Database.ORM.TH where

import Control.Applicative
import Language.Haskell.TH
import Data.Maybe (isNothing)
import Data.Extensible
import Data.Resource
import Database.ORM.HDBC
import Database.ORM.Model

-- | Declares a method to interpret a type defined in DB into a data type in Haskell.
--
-- TH requires @DBSettings@ to implement this class to generate model definition in compile time.
class TypeMappable db where
    mapColumnType :: db -- ^ @DBSettings@ object.
                  -> String -- ^ Data type name defined in DB.
                  -> String -- ^ User type name defined in DB.
                  -> TypeQ -- ^ Data type in Haskell.

-- -- | Generates definition which declares a model of a table.
-- --
-- -- > data [columns type name] = '["col1" :> typ1, "col2" :> typ2, ...]
-- -- > data [model type name] = 
-- declareModel :: forall db. (ContextResources (Refs '[DBContext db]) (Refs '[DBResource db]), Resource (DBResource db), ResourceContext (DBContext db), DBSettings db, TypeMappable db)
--              => db -- ^ DB settings.
--              -> String -- ^ Table name.
--              -> String -- ^ Type name.
--              -> Q [Dec]

-- | Generates definition of columns in a table as follows.
--
-- > data [type name] = '["col1" :> typ1, "col2" :> typ2, ...]
declareColumns :: forall db. (ContextResources (Refs '[DBContext db]) (Refs '[DBResource db]), Resource (DBResource db), ResourceContext (DBContext db), DBSettings db, TypeMappable db)
               => db -- ^ DB settings.
               -> String -- ^ Table name.
               -> String -- ^ Type name.
               -> Q [Dec] -- ^ Declarations of the model.
declareColumns settings table name = do
    r <- runIO $ newResource settings
    let resources = r `RCons` RNil
    ts <- runIO $ fst <$> withContext @'[DBContext db] resources (readSchema table)
    let cols = map (columnDefinition settings) (filter (\c -> hasRelation c) $ tableColumns ts)
    let defs = foldl (\v c -> appT (appT promotedConsT c) v) promotedNilT (reverse cols)
    (: []) <$> tySynD (mkName name) [] defs

-- | Represents a column in the form available for the model definition.
--
-- When the column is not nullable, this function returns @"column_name" :> a@.
-- otherwise, @"column_name" :> Maybe a@ where @a@ denotes the data type of the column.
columnDefinition :: (TypeMappable db)
                 => db -- ^ DB settings.
                 -> ColumnMeta -- ^ Column information.
                 -> TypeQ -- ^ Type which represents the column.
columnDefinition settings (ColumnMeta {..}) = infixT (litT $ strTyLit columnName) (mkName $ ":>") (ct isNullable)
    where
        ct :: Bool -> TypeQ
        ct True = appT (conT ''Maybe) (mapColumnType settings columnType userType)
        ct False = mapColumnType settings columnType userType