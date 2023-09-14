{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Oracle.Simple.FromRow where

import Control.Exception
import Control.Monad
import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal

class FromRow a where
  fromRow :: RowParser a

newtype RowParser a = RowParser {runRowParser :: DPIStmt -> IO a}

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
getRow stmt = runRowParser fromRow stmt

-- | Column position, starting with 1 for the first column.
newtype Column = Column {getColumn :: Int}
  deriving newtype (Num, Show)

-- | Derive a @RowParser@ for a field at the specified column position.
field :: (FromField a) => Column -> RowParser a
field pos = fieldWith pos fromField

-- | Derive a 'RowParser' for a field at the specified column position
-- using the supplied 'FieldParser'.
fieldWith :: Column -> FieldParser a -> RowParser a
fieldWith pos FieldParser{..} = RowParser $ \dpiStmt -> do
  (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral $ getColumn pos)
  unless (gotType == dpiNativeType) $
    throwIO $
      TypeMismatch dpiNativeType gotType pos
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
