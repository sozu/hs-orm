{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.TH where

import GHC.TypeLits
import Control.Applicative
import Control.Monad
import Data.Proxy
import Language.Haskell.TH
import Data.Maybe (isNothing)
import Data.Extensible
import Data.Resource
import Data.Pool
import Data.IORef
import Database.ORM.HDBC
import Database.ORM.Model
import Database.ORM.Record

-- | Declares a method to interpret a type defined in DB into a data type in Haskell.
--
-- TH requires @DBSettings@ to implement this class to generate model definition in compile time.
class TypeMappable db where
    mapColumnType :: db -- ^ @DBSettings@ object.
                  -> String -- ^ Data type name defined in DB.
                  -> String -- ^ User type name defined in DB.
                  -> TypeQ -- ^ Data type in Haskell.

withTable :: forall db. (
             ContextResources (Refs '[DBContext db]) (Refs '[DBResource db])
           , Resource (DBResource db)
           , ResourceContext (DBContext db)
           , DBSettings db
           , TypeMappable db)
           => db
           -> String
           -> (TableMeta -> Q [Dec])
           -> Q [Dec]
withTable settings table f = do
    r <- runIO $ newResource settings
    let resources = r `RCons` RNil
    ts <- runIO $ fst <$> withContext @'[DBContext db] resources (readSchema table)
    decs <- f ts
    runIO $ do
        (DBResource _ _ p) <- readIORef r
        case p of
            ConnectionPool p' -> destroyAllResources p'
            FixedConnection _ _ -> return ()
    return decs

decsOfColumns :: (TypeMappable db)
              => db
              -> Name
              -> TableMeta
              -> Q [Dec]
decsOfColumns settings name ts = do
    let cols = map (columnDefinition settings) (filter (\c -> not $ hasRelation c) $ tableColumns ts)
    let defs = foldl (\v c -> appT (appT promotedConsT c) v) promotedNilT (reverse cols)
    sig <- appT listT $ appT (appT (conT ''Assoc) (conT ''Symbol)) (return StarT)
    (: []) <$> tySynD name [] (sigT defs sig)

decsOfModel :: (TypeMappable db)
            => db
            -> String
            -> TableMeta
            -> Q [Dec]
decsOfModel settings name ts = do
    let colsName = mkName $ name ++ "'"
    columns <- decsOfColumns settings colsName ts
    let pks = map columnName $ filter isPrimary (tableColumns ts)
    fks <- mapM (\c -> infixT (litT $ strTyLit $ columnName c) '(:>) (mapColumnType settings (columnType c) (userType c)))
                $ filter hasRelation (tableColumns ts)
    let appPKs = \s -> if length pks > 0 then appT (appT (promotedT '(:)) (appT (conT ''PK) (strLits pks))) s else s
    let appFKs = \s -> if length fks > 0 then appT (appT (promotedT '(:)) (appT (conT ''FK) (typeList fks))) s else s
    model <- tySynD (mkName name) [] [t| TableModel $(litT $ strTyLit (tableName ts)) Select' (Record $(conT colsName)) $(appPKs $ appFKs promotedNilT) |]
    exts <- shrinkDefinition (mkName name) ts
    return $ (reverse $ model : columns) ++ exts
    where
        strLits :: [String] -> TypeQ
        strLits [] = promotedNilT
        strLits (k : ks) = appT (appT promotedConsT (litT $ strTyLit k)) (strLits ks)

        typeList :: [Type] -> TypeQ
        typeList [] = promotedNilT
        typeList (t : ts) = appT (appT promotedConsT (return t)) (typeList ts)

-- | Generates definition of selctable model of a table as follows.
--
-- > type [model_name] = "table" :## Record [colums_type]
declareModels :: forall db. (ContextResources (Refs '[DBContext db]) (Refs '[DBResource db]), Resource (DBResource db), ResourceContext (DBContext db), DBSettings db, TypeMappable db)
              => db -- ^ DB settings.
              -> String -- ^ Table name.
              -> String -- ^ Type name.
              -> Q [Dec] -- ^ Declarations of a model and its columns.
declareModels settings table name = withTable settings table $ decsOfModel settings name

-- | Generates definition of columns in a table as follows.
--
-- > type [type name] = '["col1" :> typ1, "col2" :> typ2, ...] :: '[Assoc Symbol *]
declareColumns :: forall db. (ContextResources (Refs '[DBContext db]) (Refs '[DBResource db]), Resource (DBResource db), ResourceContext (DBContext db), DBSettings db, TypeMappable db)
               => db -- ^ DB settings.
               -> String -- ^ Table name.
               -> String -- ^ Type name.
               -> Q [Dec] -- ^ Declarations of columns.
declareColumns settings table name = withTable settings table $ decsOfColumns settings (mkName name)

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

-- | Generates instance declaration of @ExtensibleModel@.
--
-- > type SomeModel = TableModel "table" Select' (Record '["id" :> Int, "name" :> String]) '[]
-- > instance ExtensibleModel SomeModel where
-- >     shrinkFor m [] (p :: Proxy ks) f = f (m ~/ shrink :: TM :^@ ks)
-- >     shrinkFor m ("id":cs) (p :: Proxy ks) f = shrinkFor m cs (Proxy :: Proxy ("id" ': ks)) f
-- >     shrinkFor m ("name":cs) (p :: Proxy ks) f = shrinkFor m cs (Proxy :: Proxy ("name" ': ks)) f
shrinkDefinition :: Name
                 -> TableMeta
                 -> Q [Dec]
shrinkDefinition n t = do
    let nas = mkName "as"
    let nm = mkName "m"
    let nks = mkName "ks"
    let c = cxt []
    let funcs = funD 'shrinkFor $
                        (clause [varP $ mkName "m", listP [], sigP (varP $ mkName "p") (appT (conT ''Proxy) (varT nks)), varP $ mkName "f"]
                                (normalB [| f (m ~/ shrink :: $(conT n) :^@ $(varT nks)) |])
                                []
                        ) : ((`map` tableColumns t) $ \c ->
                            clause [varP $ mkName "m", infixP (litP $ stringL $ columnName c) ('(:)) (varP $ mkName "cs"), sigP (varP $ mkName "p") (appT (conT ''Proxy) (varT nks)), varP $ mkName "f"]
                                   (normalB [| $(varE 'shrinkFor) m cs (Proxy :: Proxy ($(litT $ strTyLit $ columnName c) ': $(varT nks))) f |])
                                   []
                        )
    (:[]) <$> instanceD c (appT (conT ''ExtensibleModel) (conT n)) [funcs]

