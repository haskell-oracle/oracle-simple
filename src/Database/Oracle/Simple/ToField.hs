{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Oracle.Simple.ToField where

import Data.Fixed
import Data.Time
import Data.Int
import Data.Text
import Database.Oracle.Simple.Internal

class (HasDPINativeType a) => ToField a where
  toField :: a -> IO WriteBuffer

instance ToField Double where
  toField = pure . AsDouble

instance ToField Text where
  toField = fmap AsBytes . mkDPIBytesUTF8 . unpack

instance ToField String where
  toField = fmap AsBytes . mkDPIBytesUTF8

instance ToField Int64 where
  toField = pure . AsInt64

instance ToField Int where
  toField = pure . AsInt64 . fromIntegral

instance ToField DPITimestamp where
  toField = pure . AsTimestamp

instance ToField UTCTime where
  toField utcTime = pure $ AsTimestamp (utcTimeToDPITimestamp utcTime)

utcTimeToDPITimestamp :: UTCTime -> DPITimestamp
utcTimeToDPITimestamp utcTime = dpiTs
  where
    ZonedTime {..} = utcToZonedTime utc utcTime
    LocalTime {..} = zonedTimeToLocalTime
    (year, month, day) = toGregorian localDay
    TimeOfDay {..} = localTimeOfDay
    TimeZone {..} = zonedTimeZone
    (seconds, fractionalSeconds) = properFraction todSec
    (minuteOffset, hourOffset) = timeZoneMinutes `quotRem` 60
    dpiTs = DPITimestamp
      { year           = fromIntegral year
      , month          = fromIntegral month
      , day            = fromIntegral day
      , hour           = fromIntegral todHour
      , minute         = fromIntegral todMin
      , second         = seconds
      , fsecond        = truncate (fractionalSeconds * 1_000_000_000_000)
      , tzHourOffset   = fromIntegral hourOffset
      , tzMinuteOffset = fromIntegral timeZoneMinutes
      }
