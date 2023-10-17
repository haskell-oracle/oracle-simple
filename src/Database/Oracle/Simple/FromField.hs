{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Oracle.Simple.FromField where

import qualified Data.Aeson as Aeson
import qualified Data.List as L
import Foreign
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Fixed
import Data.Int
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Word
import Database.Oracle.Simple.Internal
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Generics
import Database.Oracle.Simple.JSON

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

instance FromField Aeson.Value where
  fromField = FieldParser getJSON 

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

dpiTimeStampToUTCTime :: DPITimestamp -> UTCTime
dpiTimeStampToUTCTime dpi =
  let DPITimestamp{..} = dpiTimeStampToUTCDPITimeStamp dpi
      local = LocalTime d tod
      d = fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
      tod = TimeOfDay (fromIntegral hour) (fromIntegral minute) (fromIntegral second + picos)
      picos = MkFixed (fromIntegral fsecond * 1000) :: Pico
   in localTimeToUTC utc local

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

getJSON :: ReadDPIBuffer Aeson.Value 
getJSON = parseNode <=< peek <=< getValue <=< dpiData_getJSON

-- | Get a Word64 value from the data buffer.
getWord64 :: ReadDPIBuffer Word64
getWord64 = dpiData_getUint64

-- | Get a boolean value from the data buffer.
getBool :: ReadDPIBuffer Bool
getBool ptr = (== 1) <$> dpiData_getBool ptr

-- | Get Text from the data buffer.
-- Supports ASCII, UTF-8 and UTF-16 big- and little-endian encodings.
-- Throws 'FieldParseError' if any other encoding is encountered.
getText :: ReadDPIBuffer Text
getText = buildText <=< peek <=< dpiData_getBytes

buildText :: DPIBytes -> IO Text
buildText DPIBytes{..} = do
    gotBytes <- BS.packCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)
    encoding <- peekCString dpiBytesEncoding
    decodeFn <- case encoding of
      "ASCII" -> pure decodeASCII
      "UTF-8" -> pure decodeUtf8
      "UTF-16BE" -> pure decodeUtf16BE
      "UTF-16LE" -> pure decodeUtf16LE
      otherEnc -> throwIO $ UnsupportedEncoding otherEnc
    evaluate (decodeFn gotBytes)
      `catch` ( \(e :: SomeException) -> throwIO (ByteDecodeError encoding (displayException e))
              )

-- | Get Text from the data buffer
getString :: ReadDPIBuffer String
getString = fmap unpack <$> getText

-- | Get a `DPITimestamp` from the buffer
getTimestamp :: ReadDPIBuffer DPITimestamp
getTimestamp = peek <=< dpiData_getTimestamp

-- | Errors encountered when parsing a database field.
data FieldParseError
  = -- | We encountered an encoding other than ASCII, UTF-8 or UTF-16
    UnsupportedEncoding {fpeOtherEncoding :: String}
  | -- | Failed to decode bytes using stated encoding
    ByteDecodeError {fpeEncoding :: String, fpeErrorMsg :: String}
  deriving (Show)

instance Exception FieldParseError where
  displayException UnsupportedEncoding{..} =
    "Field Parse Error: Encountered unsupported text encoding '"
      <> fpeOtherEncoding
      <> "'. Supported encodings: ASCII, UTF-8, UTF-16BE, UTF-16LE."
  displayException ByteDecodeError{..} =
    "Field Parse Error: Failed to decode bytes as " <> fpeEncoding <> ": " <> fpeErrorMsg

newtype JsonWrapper = JsonWrapper { unwrap :: () }
  deriving Show
