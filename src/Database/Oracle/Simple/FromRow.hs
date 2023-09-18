{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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

module Database.Oracle.Simple.FromRow where

import Control.Exception hiding (TypeError)
import Control.Monad
import Control.Monad.State.Strict
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

instance (TypeError ('Text "Sum types not supported")) => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> readField

newtype RowParser a = RowParser {runRowParser :: DPIStmt -> StateT Word32 IO a}

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

-- | Derive a @RowParser@ for a field at the specified column position.
readField :: (FromField a) => RowParser a
readField = fieldWith fromField

-- | Derive a 'RowParser' for a field at the specified column position
-- using the supplied 'FieldParser'.
fieldWith :: forall a. (FromField a) => FieldParser a -> RowParser a
fieldWith FieldParser{..} = RowParser $ \dpiStmt -> do
  pos <- modify (+ 1) >> get
  liftIO $ do
    (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral pos)
    let typ = dpiNativeType (Proxy @a)
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
  displayException (TypeMismatch{..}) =
    "Row parse error due to type mismatch: At column "
      <> show column
      <> ", expected "
      <> show expectedType
      <> " but got "
      <> show gotType
      <> "."

query :: (FromRow row) => DPIConn -> String -> IO [row]
query conn sql = do
  stmt <- prepareStmt conn sql
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt [] found
 where
  loop stmt xs n | n < 1 = pure xs
  loop stmt xs _ = do
    tsVal <- getRow stmt
    found <- fetch stmt
    loop stmt (xs ++ [tsVal]) found
