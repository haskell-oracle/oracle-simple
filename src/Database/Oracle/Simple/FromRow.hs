{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Oracle.Simple.FromRow where

import Control.Exception hiding (TypeError)
import Control.Monad
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Proxy
import Data.Word
import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal
import GHC.Generics
import GHC.TypeLits

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (GFromRow (Rep a), Generic a) => RowParser a
  fromRow = to <$> gFromRow
  
  defineColumnTypes :: RowParser a
  default defineColumnTypes :: (GFromRow (Rep a), Generic a) => RowParser a
  defineColumnTypes = to <$> gDefineColumnTypes

instance FromField a => FromRow (Only a)

instance FromField a => FromRow (Identity a)

instance (FromField a, FromField b) => FromRow (a, b)

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c)

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g)
  => FromRow (a, b, c, d, e, f, g)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h)
  => FromRow (a, b, c, d, e, f, g, h)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i)
  => FromRow (a, b, c, d, e, f, g, h, i)

instance
  ( FromField a
  , FromField b
  , FromField c
  , FromField d
  , FromField e
  , FromField f
  , FromField g
  , FromField h
  , FromField i
  , FromField j
  )
  => FromRow (a, b, c, d, e, f, g, h, i, j)

class GFromRow f where
  gFromRow :: RowParser (f a)
  gDefineColumnTypes :: RowParser (f a)

instance (GFromRow m) => GFromRow (D1 i m) where
  gFromRow = M1 <$> gFromRow
  gDefineColumnTypes = M1 <$> gDefineColumnTypes

instance (GFromRow m) => GFromRow (C1 i m) where
  gFromRow = M1 <$> gFromRow
  gDefineColumnTypes = M1 <$> gDefineColumnTypes

instance (GFromRow m) => GFromRow (S1 i m) where
  gFromRow = M1 <$> gFromRow
  gDefineColumnTypes = M1 <$> gDefineColumnTypes

instance (GFromRow l, GFromRow r) => GFromRow (l :*: r) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow
  gDefineColumnTypes = (:*:) <$> gDefineColumnTypes <*> gDefineColumnTypes

instance (TypeError ('Text "Sum types not supported")) => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"
  gDefineColumnTypes = error "Sum types not supported"

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> readField
  gDefineColumnTypes = K1 <$> defineColTypes

newtype RowParser a = RowParser {runRowParser :: DPIStmt -> StateT Column IO a}

instance Functor RowParser where
  fmap f g = RowParser $ \dpiStmt -> f <$> runRowParser g dpiStmt

instance Applicative RowParser where
  pure a = RowParser $ \_ -> pure a
  fn <*> g = RowParser $ \dpiStmt -> do
    f <- runRowParser fn dpiStmt
    f <$> runRowParser g dpiStmt

instance Monad RowParser where
  return = pure
  f >>= g = RowParser $ \dpiStmt -> do
    f' <- runRowParser f dpiStmt
    runRowParser (g f') dpiStmt

-- | Retrieve the currently fetched row.
getRow :: forall a. (FromRow a) => DPIStmt -> IO a
getRow stmt = evalStateT (runRowParser fromRow stmt) 0

rDefineColumnTypes :: forall a. (FromRow a) => DPIStmt -> IO ()
rDefineColumnTypes stmt = void $ evalStateT (runRowParser @a defineColumnTypes stmt) 0

defineColTypes :: FromField a => RowParser a
defineColTypes = defineColTypes' fromField 

defineColTypes' :: forall a. FromField a => FieldParser a -> RowParser a
defineColTypes' FieldParser{..} = RowParser $ \dpiStmt -> do
  pos <- modify (+ 1) >> get
  liftIO $ do
    let nativeType = dpiNativeType (Proxy @a)
    -- if the type needs it, perform a manual cast
    whenJust (dpiTypeOverride (Proxy @a)) $ \oracleType -> dpiDefineValue dpiStmt pos oracleType nativeType
  pure undefined
 where whenJust mg f = maybe (pure ()) f mg

-- | Derive a @RowParser@ for a field at the specified column position.
readField :: (FromField a) => RowParser a
readField = fieldWith fromField

-- | Derive a 'RowParser' for a field at the specified column position
-- using the supplied 'FieldParser'.
fieldWith :: forall a. (FromField a) => FieldParser a -> RowParser a
fieldWith FieldParser{..} = RowParser $ \dpiStmt -> do
  pos <- modify (+ 1) >> get
  liftIO $ do
    let nativeType = dpiNativeType (Proxy @a)
    (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral $ getColumn pos)
    unless (gotType == nativeType) $
      throwIO $
        TypeMismatch nativeType gotType pos
    readDPIDataBuffer dataBuf

data RowParseError
  = -- | We encountered a type that we were not expecting.
    TypeMismatch
    { expectedType :: DPINativeType
    -- ^ The DPI native type we were expecting
    , gotType :: DPINativeType
    -- ^ The DPI native type we got
    , column :: Column
    -- ^ Column position where type mismatch was encountered (1-indexed)
    }
  deriving (Show)

instance Exception RowParseError where
  displayException (TypeMismatch{..}) =
    "Row parse error due to type mismatch: At column "
      <> show column
      <> ", expected "
      <> show expectedType
      <> " but got "
      <> show gotType
      <> "."
