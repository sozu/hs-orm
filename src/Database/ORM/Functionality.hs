{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.ORM.Functionality (
    RecordLock
  , QualifyRecordLock(..)
  , ApplyRecordLock(..)
) where

import Data.Kind
import Data.Proxy
import Database.ORM.HDBC

data RecordLock db (mode :: RecordLockMode (DialectType db))

class QualifyRecordLock mode where
    qualifyRecordLock :: Proxy mode -> String -> String

class ApplyRecordLock db (as :: [Type]) where
    recordLockQuery :: Proxy db -> Proxy as -> String -> String

instance (DBSettings db) => ApplyRecordLock db '[] where
    recordLockQuery _ _ query = query

instance (DBSettings db, QualifyRecordLock mode) => ApplyRecordLock db (RecordLock db (mode :: RecordLockMode (DialectType db)) ': as) where
    recordLockQuery _ _ query = qualifyRecordLock (Proxy :: Proxy mode) query

instance {-# OVERLAPPABLE #-} (DBSettings db, ApplyRecordLock db as) => ApplyRecordLock db (a ': as) where
    recordLockQuery p _ query = recordLockQuery p (Proxy :: Proxy as) query