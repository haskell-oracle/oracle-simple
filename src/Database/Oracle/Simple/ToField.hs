{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Oracle.Simple.ToField
  ( ToField (..),
    utcTimeToDPITimestamp,
  )
where

import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime (..), ZonedTime (..), toGregorian, utc, utcToZonedTime)
import Foreign.Marshal.Utils (fromBool)
import Numeric.Natural (Natural)

import Database.Oracle.Simple.Internal

class ToField a where
  toDPINativeType :: Proxy a -> DPINativeType
  -- ^ The DPI native type of the value written to the buffer.

  toField :: a -> IO WriteBuffer
  -- ^ Write a value of type @a@ to the data buffer.

instance ToField Bool where
  toDPINativeType _ = DPI_NATIVE_TYPE_BOOLEAN
  toField = pure . AsBoolean . fromBool

instance ToField Double where
  toDPINativeType _ = DPI_NATIVE_TYPE_DOUBLE
  toField = pure . AsDouble

instance ToField T.Text where
  toDPINativeType _ = DPI_NATIVE_TYPE_BYTES
  toField = fmap AsBytes . mkDPIBytesUTF8 . T.unpack

instance ToField String where
  toDPINativeType _ = DPI_NATIVE_TYPE_BYTES
  toField = fmap AsBytes . mkDPIBytesUTF8

instance ToField Int64 where
  toDPINativeType _ = DPI_NATIVE_TYPE_INT64
  toField = pure . AsInt64

instance ToField Int32 where
  toDPINativeType _ = toDPINativeType (Proxy @Int64)
  toField = pure . AsInt64 . fromIntegral

instance ToField Int where
  toDPINativeType _ = toDPINativeType (Proxy @Int64)
  toField = pure . AsInt64 . fromIntegral

instance ToField Natural where
  toDPINativeType _ = toDPINativeType (Proxy @Int64)
  toField = pure . AsInt64 . fromIntegral

instance (ToField a) => ToField (Maybe a) where
  toDPINativeType _ = toDPINativeType (Proxy @a)
  toField (Just val) = toField val
  toField Nothing = pure AsNull

instance ToField DPITimestamp where
  toDPINativeType _ = DPI_NATIVE_TYPE_TIMESTAMP
  toField = pure . AsTimestamp

instance ToField UTCTime where
  toDPINativeType _ = DPI_NATIVE_TYPE_TIMESTAMP
  toField utcTime = pure $ AsTimestamp (utcTimeToDPITimestamp utcTime)

utcTimeToDPITimestamp :: UTCTime -> DPITimestamp
utcTimeToDPITimestamp utcTime = dpiTimeStampToUTCDPITimeStamp dpiTs
  where
    ZonedTime {..} = utcToZonedTime utc utcTime
    LocalTime {..} = zonedTimeToLocalTime
    (year, month, day) = toGregorian localDay
    TimeOfDay {..} = localTimeOfDay
    TimeZone {..} = zonedTimeZone
    (seconds, fractionalSeconds) = properFraction todSec
    (hourOffset, minuteOffset) = timeZoneMinutes `quotRem` 60
    dpiTs =
      DPITimestamp
        { year = fromIntegral year
        , month = fromIntegral month
        , day = fromIntegral day
        , hour = fromIntegral todHour
        , minute = fromIntegral todMin
        , second = seconds
        , fsecond = truncate (fractionalSeconds * 1e9)
        , tzHourOffset = fromIntegral hourOffset
        , tzMinuteOffset = fromIntegral minuteOffset
        }
