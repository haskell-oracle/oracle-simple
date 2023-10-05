{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.FromRow where

import Foreign.Storable
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

  castColumnType :: a -> RowParser ()
  default castColumnType :: (GFromRow (Rep a), Generic a) => a -> RowParser ()
  castColumnType = gCastColumnType . from

instance (FromField a) => FromRow (Only a)

instance (FromField a) => FromRow (Identity a)

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
  gCastColumnType :: f a -> RowParser ()

instance (GFromRow m) => GFromRow (D1 i m) where
  gFromRow = M1 <$> gFromRow
  gCastColumnType (M1 x) = gCastColumnType x

instance (GFromRow m) => GFromRow (C1 i m) where
  gFromRow = M1 <$> gFromRow
  gCastColumnType (M1 x) = gCastColumnType x

instance (GFromRow m) => GFromRow (S1 i m) where
  gFromRow = M1 <$> gFromRow
  gCastColumnType (M1 x) = gCastColumnType x

instance (GFromRow l, GFromRow r) => GFromRow (l :*: r) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow
  gCastColumnType (l :*: r) = gCastColumnType l >> gCastColumnType r

instance (TypeError ('Text "Sum types not supported")) => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"
  gCastColumnType = error "Sum types not supported"

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> readField
  gCastColumnType (K1 x) = void (castColumnType' (Proxy @a))

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

defineColumnTypes :: forall a. (FromRow a) => DPIStmt -> IO ()
defineColumnTypes stmt = void $ evalStateT (runRowParser (castColumnType (undefined :: a)) stmt) 0

castColumnType' :: forall a. (HasDPINativeType a) => Proxy a -> RowParser ()
castColumnType' proxy = RowParser $ \dpiStmt -> do
  pos <- modify (+ 1) >> get
  liftIO $ do
    let nativeType = dpiNativeType proxy
    -- if the type needs it, perform a manual cast
    whenJust (dpiTypeOverride proxy) $ \oracleType -> dpiDefineValue dpiStmt pos oracleType nativeType
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
    readDPIDataBuffer dataBuf `catch` (\(e :: FieldParseError) -> throwIO $ FieldParseError (displayException e) pos)

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
  | -- | Failed to parse a field
    FieldParseError
    { errorMessage :: String
    -- ^ Error message returned by the field parser
    , column :: Column
    -- ^ Column position where the field parse error occured (1-indexed)
    }
  deriving (Show)

instance Exception RowParseError where
  displayException TypeMismatch{..} =
    "Row parse error due to type mismatch: At column "
      <> show column
      <> ", expected "
      <> show expectedType
      <> " but got "
      <> show gotType
      <> "."
  displayException FieldParseError{..} = "Field parsing error at column " <> show column <> ": " <> errorMessage
