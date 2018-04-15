{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Database.ORM.TH where

import Control.Applicative
import Language.Haskell.TH
import Data.Extensible
import Data.Resource
import Database.ORM.HDBC
import Database.ORM.Model

class TypeMappable db where
    mapColumnType :: db
                  -> String
                  -> TypeQ

generateModel :: forall db. (ContextResources (Refs '[DBContext db]) (Refs '[DBResource db]), Resource (DBResource db), ResourceContext (DBContext db), DBSettings db, TypeMappable db)
              => db
              -> String
              -> String
              -> Q [Dec]
generateModel settings table name = do
    r <- runIO $ newResource settings
    let resources = r `RCons` RNil
    ts <- runIO $ fst <$> withContext @'[DBContext db] resources (readSchema table)
    let cols = map (columnDefinition settings) (tableColumns ts)
    let defs = foldl (\v c -> appT (appT promotedConsT c) v) promotedNilT (reverse cols)
    (: []) <$> tySynD (mkName name) [] defs

columnDefinition :: (TypeMappable db)
                 => db
                 -> ColumnMeta
                 -> TypeQ
columnDefinition settings (ColumnMeta {..}) = infixT (litT $ strTyLit columnName) (mkName $ ":>") (mapColumnType settings columnType)