{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
module Database.Oracle.Simple.FromRow where

import Control.Monad.State
import Control.Exception hiding (TypeError)
import Control.Monad
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

instance GFromRow m => GFromRow (D1 i m) where
  gFromRow = M1 <$> gFromRow

instance GFromRow m => GFromRow (C1 i m) where
  gFromRow = M1 <$> gFromRow

instance GFromRow m => GFromRow (S1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow l, GFromRow r) => GFromRow (l :*: r) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow

instance TypeError ('Text "Sum types not supported") => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"

instance FromField a => GFromRow (K1 i a) where
  gFromRow = K1 <$> field

newtype RowParser a = RowParser { runRowParser :: DPIStmt -> StateT Int IO a }

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

-- | Column position, starting with 1 for the first column.
newtype Column = Column {getColumn :: Int}
  deriving newtype (Num, Show)

-- | Derive a @RowParser@ for a field at the specified column position.
field :: (FromField a) => RowParser a
field = fieldWith fromField

-- | Derive a 'RowParser' for a field at the specified column position
-- using the supplied 'FieldParser'.
fieldWith :: FieldParser a -> RowParser a
fieldWith FieldParser{..} = RowParser $ \dpiStmt -> do
  pos <- modify (+1) >> get
  liftIO $ do
    (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral pos)
    unless (gotType == dpiNativeType) $
      throwIO $
        TypeMismatch dpiNativeType gotType (Column pos)
    readDPIDataBuffer dataBuf

data RowParseError
  = -- | We encountered a type that we were not expecting.
    TypeMismatch
    { expectedType :: DPINativeTypeNum
    -- ^ The DPI native type we were expecting
    , gotType :: DPINativeTypeNum
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

query :: FromRow row => DPIConn -> String -> IO [row]
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
