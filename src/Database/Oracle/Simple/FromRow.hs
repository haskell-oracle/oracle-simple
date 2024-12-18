{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Database.Oracle.Simple.FromRow
  ( FromRow (..),
    getRow,
  )
where

import Control.Exception hiding (TypeError)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.Generics
import GHC.TypeLits

import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal

-- | A type class for types that can be converted from a database row.
-- This class allows for flexible, type-safe extraction of data from rows,
-- using a 'RowParser' to define how each field should be parsed.
class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (GFromRow (Rep a), Generic a) => RowParser a
  fromRow = to <$> gFromRow

instance FromField a => FromRow (Only a)

instance FromField a => FromRow (Identity a)

instance (FromField a, FromField b) => FromRow (a, b)

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c)

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g) =>
  FromRow (a, b, c, d, e, f, g)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h) =>
  FromRow (a, b, c, d, e, f, g, h)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i) =>
  FromRow (a, b, c, d, e, f, g, h, i)

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
  ) =>
  FromRow (a, b, c, d, e, f, g, h, i, j)

class GFromRow f where
  gFromRow :: RowParser (f a)

instance (GFromRow m) => GFromRow (D1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow m) => GFromRow (C1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow m) => GFromRow (S1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow l, GFromRow r) => GFromRow (l :*: r) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow

instance (() ~ TypeError ('Text "Sum types not supported")) => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> readField

newtype RowParser a = RowParser {runRowParser :: DPIStmt -> StateT Word32 IO a}

instance Functor RowParser where
  fmap f g = RowParser $ fmap f . runRowParser g

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

-- | Derive a @RowParser@ for a field at the specified column position.
readField :: (FromField a) => RowParser a
readField = fieldWith fromField

{- | Derive a 'RowParser' for a field at the specified column position
using the supplied 'FieldParser'.
-}
fieldWith :: forall a. (FromField a) => FieldParser a -> RowParser a
fieldWith FieldParser {..} = RowParser $ \dpiStmt -> do
  pos <- modify (+ 1) >> get
  liftIO $ do
    (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral pos)
    let typ = fromDPINativeType (Proxy @a)
    unless (gotType == typ) $
      throwIO $
        TypeMismatch typ gotType (Column pos)
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
  displayException (TypeMismatch {..}) =
    "Row parse error due to type mismatch: At column "
      <> show column
      <> ", expected "
      <> show expectedType
      <> " but got "
      <> show gotType
      <> "."
