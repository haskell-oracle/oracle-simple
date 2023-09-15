{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Oracle.Simple.FromField where

import Data.Word
import Data.Proxy
import GHC.Generics
import Control.Monad
import Data.Coerce
import Data.Int
import Data.Text
import Database.Oracle.Simple.Internal
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Generic

-- | A type that may be parsed from a database field.
class FromField a where
  fieldType :: Proxy a -> DPINativeTypeNum
  fromField :: FieldParser a

instance Functor FieldParser where
  fmap f FieldParser{..} = FieldParser (fmap f <$> readDPIDataBuffer)

instance FromField Double where
  fieldType Proxy = DPI_NATIVE_TYPE_DOUBLE
  fromField = FieldParser getDouble

instance FromField Float where
  fieldType Proxy = DPI_NATIVE_TYPE_FLOAT
  fromField = FieldParser getFloat

instance FromField DPITimeStamp where
  fieldType Proxy = DPI_NATIVE_TYPE_TIMESTAMP
  fromField = FieldParser getTimestamp

instance FromField Text where
  fieldType Proxy = DPI_NATIVE_TYPE_BYTES
  fromField = FieldParser getText

instance FromField String where
  fieldType Proxy = DPI_NATIVE_TYPE_BYTES
  fromField = FieldParser getString

instance FromField Int64 where
  fieldType Proxy = DPI_NATIVE_TYPE_INT64
  fromField = FieldParser getInt64

instance FromField Word64 where
  fieldType Proxy = DPI_NATIVE_TYPE_UINT64
  fromField = FieldParser getWord64

instance FromField Bool where
  fieldType Proxy = DPI_NATIVE_TYPE_BOOLEAN
  fromField = FieldParser getBool

instance FromField Int where
  fieldType Proxy = fieldType (Proxy @Int64)
  fromField = fromIntegral <$> fromField @Int64

instance FromField a => FromField (Maybe a) where
  fieldType Proxy = fieldType (Proxy @a)
  fromField = FieldParser $ \ptr -> do
    result <- dpiData_getIsNull ptr
    if result == 1
      then pure Nothing
      else Just <$> readDPIDataBuffer (fromField @a) ptr

-- instance FromField UTCTime where
--   fieldType Proxy = fieldType (Proxy @DPITimeStamp)
--   fromField = do
--     DPITimeStamp {..} <- fromField
--     let
--       yymmdd = fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
--       hhmmss = 0
--     pure (UTCTime yymmdd hhmmss)

-- | Encapsulates all information needed to parse a field as a Haskell value.
newtype FieldParser a = FieldParser
  { readDPIDataBuffer :: ReadDPIBuffer a
    -- ^ A function that retrieves a value of type @a@ from the DPI data buffer.
  }

instance Applicative FieldParser where
  pure x = FieldParser $ \ptr -> pure x
  FieldParser f <*> FieldParser g = FieldParser $ \ptr -> do
    f' <- f ptr
    x <- g ptr
    pure (f' x)

instance Monad FieldParser where
  FieldParser g >>= f = FieldParser $ \ptr -> do
    x <- g ptr
    readDPIDataBuffer (f x) ptr

-- | Alias for a function that retrieves a value of type @a@ from the DPI data buffer
type ReadDPIBuffer a = Ptr DPIData -> IO a

-- ** @ReadDPIBuffer@s for common types

-- | Get a Double value from the data buffer
getDouble :: ReadDPIBuffer Double
getDouble = coerce <$> dpiData_getDouble

-- | Get a Float value from the data buffer
getFloat :: ReadDPIBuffer Float
getFloat = coerce <$> dpiData_getFloat

-- | Get an Int64 value from the data buffer.
getInt64 :: ReadDPIBuffer Int64
getInt64 = dpiData_getInt64

-- | Get a Word64 value from the data buffer.
getWord64 :: ReadDPIBuffer Word64
getWord64 = dpiData_getUInt64

-- | Get an Int64 value from the data buffer.
getBool :: ReadDPIBuffer Bool
getBool ptr = (==1) <$> dpiData_getBool ptr

-- | Get Text from the data buffer
getText :: ReadDPIBuffer Text
getText = fmap pack <$> getString

-- | Get String from the data buffer
getString :: ReadDPIBuffer String
getString = buildString <=< peek <=< dpiData_getBytes
 where
   buildString DPIBytes{..} =
     peekCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)

-- | Get a `DPITimeStamp` from the buffer
getTimestamp :: ReadDPIBuffer DPITimeStamp
getTimestamp = peek <=< dpiData_getTimestamp
