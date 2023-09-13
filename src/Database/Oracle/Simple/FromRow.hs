{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Oracle.Simple.FromRow where

import Control.Monad
import Control.Exception
import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal

class FromRow a where
  fromRow :: RowParser a

newtype RowParser a = RowParser { runRowParser :: DPIStmt -> IO a }

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

data RowParseError
  = TypeMismatch
  -- ^ We encountered a type that we were not expecting.
  { expectedType :: DPINativeTypeNum
  -- ^ The DPI native type we were expecting
  , gotType :: DPINativeTypeNum
  -- ^ The DPI native type we got
  , rowPosition :: Position
  -- ^ Row position where type mismatch was encountered (1-indexed)
  } deriving Show

instance Exception RowParseError

getRow :: forall a. FromRow a => DPIStmt -> IO a
getRow stmt = runRowParser fromRow stmt

-- | Derive a @RowParser@ for a field at the specified position.
field :: FromField a => Position -> RowParser a
field pos = fieldWith pos fromField

fieldWith :: Position -> FieldParser a -> RowParser a
fieldWith pos FieldParser{..} = RowParser $ \dpiStmt -> do
  (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral $ getPosition pos)
  unless (gotType == dpiNativeType) $
    throwIO $ TypeMismatch dpiNativeType gotType pos
  readDPIDataBuffer dataBuf
