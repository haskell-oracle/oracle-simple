{-# LANGUAGE RecordWildCards #-}

module Database.Oracle.Simple.FromField where

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
  fromField :: FieldParser a

instance Functor FieldParser where
  fmap f FieldParser{..} = FieldParser dpiNativeType (fmap f <$> readDPIDataBuffer)

instance FromField Double where
  fromField = FieldParser DPI_NATIVE_TYPE_DOUBLE getDouble

instance FromField DPITimeStamp where
  fromField = FieldParser DPI_NATIVE_TYPE_TIMESTAMP getTimestamp

instance FromField Text where
  fromField = FieldParser DPI_NATIVE_TYPE_BYTES getText

instance FromField Int64 where
  fromField = FieldParser DPI_NATIVE_TYPE_DOUBLE getInt64

-- | Encapsulates all information needed to parse a field as a Haskell value.
data FieldParser a = FieldParser
  { dpiNativeType :: DPINativeTypeNum
  -- ^ The native DPI type for the field.
  , readDPIDataBuffer :: ReadDPIBuffer a
  -- ^ A function that retrieves a value of type @a@ from the DPI data buffer.
  }

-- | Alias for a function that retrieves a value of type @a@ from the DPI data buffer
type ReadDPIBuffer a = Ptr DPIData -> IO a

-- ** @ReadDPIBuffer@s for common types

-- | Get a Double value from the data buffer
getDouble :: ReadDPIBuffer Double
getDouble = coerce <$> dpiData_getDouble

-- | Get an Int64 value from the data buffer.
-- All values that correspond to the NUMBER type in Oracle are stored as a
-- `double` value under the `asDouble` member of the `dpiData` union.
-- This will truncate any digits after the decimal. To ensure that there is no
-- loss of precision, this must only be used with NUMBERs that have a scale of 0.
getInt64 :: ReadDPIBuffer Int64
getInt64 = fmap floor . getDouble

-- | Get Text from the data buffer
getText :: ReadDPIBuffer Text
getText = buildString <=< peek <=< dpiData_getBytes
 where
  buildString DPIBytes{..} =
    pack <$> peekCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)

-- | Get a `DPITimeStamp` from the buffer
getTimestamp :: ReadDPIBuffer DPITimeStamp
getTimestamp = peek <=< dpiData_getTimestamp
