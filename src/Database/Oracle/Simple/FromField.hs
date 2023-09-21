{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Oracle.Simple.FromField where

import Control.Monad
import Data.Coerce
import Data.Fixed
import Data.Int
import Data.Proxy
import Data.Text
import Data.Time
import Data.Word
import Database.Oracle.Simple.Internal
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Generics

-- | A type that may be parsed from a database field.
class (HasDPINativeType a) => FromField a where
  fromField :: FieldParser a

instance Functor FieldParser where
  fmap f FieldParser{..} = FieldParser (fmap f <$> readDPIDataBuffer)

instance FromField Double where
  fromField = FieldParser getDouble

instance FromField Float where
  fromField = FieldParser getFloat

instance FromField DPITimestamp where
  fromField = FieldParser getTimestamp

instance FromField Text where
  fromField = FieldParser getText

instance FromField String where
  fromField = FieldParser getString

instance FromField Int64 where
  fromField = FieldParser getInt64

instance FromField Word64 where
  fromField = FieldParser getWord64

instance FromField Bool where
  fromField = FieldParser getBool

instance FromField Int where
  fromField = fromIntegral <$> fromField @Int64

instance (FromField a) => FromField (Maybe a) where
  fromField = FieldParser $ \ptr -> do
    result <- dpiData_getIsNull ptr
    if result == 1
      then pure Nothing
      else Just <$> readDPIDataBuffer (fromField @a) ptr

instance FromField UTCTime where
  fromField = dpiTimeStampToUTCTime <$> fromField

dpiTimeStampToUTCDPITimeStamp :: DPITimestamp -> DPITimestamp
dpiTimeStampToUTCDPITimeStamp dpi = utcDpi
  where
    offsetInMinutes = (tzHourOffset dpi * 60) + tzMinuteOffset dpi
    currentMinutes = (hour dpi * 60) + minute dpi
    (hours, minutes) =  (currentMinutes - fromIntegral offsetInMinutes) `quotRem` 60
    utcDpi
      = dpi
      { tzHourOffset = 0
      , tzMinuteOffset = 0
      , hour = hours
      , minute = minutes
      }

dpiTimeStampToUTCTime :: DPITimestamp -> UTCTime
dpiTimeStampToUTCTime dpi =
  let
    DPITimestamp {..} = dpiTimeStampToUTCDPITimeStamp dpi
    tz = utc { timeZoneMinutes = fromIntegral $ (tzHourOffset * 60) + tzMinuteOffset }
    local = LocalTime d tod
    d = fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
    tod = TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second + picos)
    picos = MkFixed (fromIntegral fsecond) :: Pico
  in
    localTimeToUTC tz local

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
type ReadDPIBuffer a = Ptr (DPIData ReadBuffer) -> IO a

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
getWord64 = dpiData_getUint64

-- | Get a boolean value from the data buffer.
getBool :: ReadDPIBuffer Bool
getBool ptr = (== 1) <$> dpiData_getBool ptr

-- | Get Text from the data buffer
getText :: ReadDPIBuffer Text
getText = fmap pack <$> getString

-- | Get String from the data buffer
getString :: ReadDPIBuffer String
getString = buildString <=< peek <=< dpiData_getBytes
 where
  buildString DPIBytes{..} =
    peekCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)

-- | Get a `DPITimestamp` from the buffer
getTimestamp :: ReadDPIBuffer DPITimestamp
getTimestamp = peek <=< dpiData_getTimestamp
