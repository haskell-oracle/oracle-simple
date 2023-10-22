{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.JSON (JsonDecodeError (..)) where

import Data.ByteString.Lazy (toStrict)
import Control.Exception (Exception (displayException), SomeException, catch, evaluate, throwIO)
import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.ByteString (packCStringLen)
import qualified Data.ByteString.Char8 as C8
import Data.List as L
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (fromFloatDigits)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import Foreign (Ptr, Storable, alloca, peek, peekArray)
import Foreign.C (CDouble (CDouble), CInt (CInt), CString, CUInt (CUInt), peekCStringLen)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)

import Database.Oracle.Simple.ToField (ToField(toField))
import Database.Oracle.Simple.FromField (FieldParser (FieldParser), FromField (fromField), ReadDPIBuffer)
import Database.Oracle.Simple.Internal
  ( DPIBytes (DPIBytes, dpiBytesLength, dpiBytesPtr)
  , DPIData
  , DPINativeType
    ( DPI_NATIVE_TYPE_BOOLEAN
    , DPI_NATIVE_TYPE_BYTES
    , DPI_NATIVE_TYPE_DOUBLE
    , DPI_NATIVE_TYPE_JSON
    , DPI_NATIVE_TYPE_JSON_ARRAY
    , DPI_NATIVE_TYPE_JSON_OBJECT
    , DPI_NATIVE_TYPE_NULL
    )
  , DPIOracleType (DPI_ORACLE_TYPE_NUMBER)
  , ReadDPINativeType (readAs), WriteDPINativeType (writeAs)
  , ReadBuffer, WriteBuffer(AsBytes), mkDPIBytesUTF8
  )

instance Aeson.ToJSON a => WriteDPINativeType a where
  writeAs Proxy = DPI_NATIVE_TYPE_BYTES

instance {-# OVERLAPPABLE #-} Aeson.ToJSON a => ToField a where

  -- Oracle allows JSON data to be inserted using the character API.
  toField = fmap AsBytes . mkDPIBytesUTF8 . C8.unpack . toStrict . Aeson.encode

instance Aeson.FromJSON a => ReadDPINativeType a where
  readAs Proxy = DPI_NATIVE_TYPE_JSON

instance {-# OVERLAPPABLE #-} Aeson.FromJSON a => FromField a where
  
  -- ODPI does not support casting from DPI_ORACLE_TYPE_JSON to DPI_NATIVE_TYPE_BYTES.
  -- This means we need to build an aeson Value from the top-level DPIJsonNode.
  fromField = FieldParser getJson

getJson :: Aeson.FromJSON a => ReadDPIBuffer a
getJson = parseJson <=< peek <=< dpiJson_getValue <=< dpiData_getJson
 where
  parseJson topNode = do
    aesonValue <- buildValue topNode
    case Aeson.fromJSON aesonValue of
      Aeson.Error msg -> throwIO $ ParseError msg
      Aeson.Success a -> pure a

  -- Build Aeson values for various cases:

  -- Object
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_JSON_OBJECT nodeValue) = do
    DPIJsonObject{..} <- peek =<< dpiDataBuffer_getAsJsonObject nodeValue
    fieldNamePtrs <- peekArray (fromIntegral djoNumFields) djoFieldNames
    fieldNameLengths <- fmap fromIntegral <$> peekArray (fromIntegral djoNumFields) djoFieldNameLengths
    keys <- mapM (fmap fromString . peekCStringLen) (L.zip fieldNamePtrs fieldNameLengths)
    values <- mapM buildValue =<< peekArray (fromIntegral djoNumFields) djoFields
    pure $ Aeson.Object $ KeyMap.fromList (L.zip keys values)

  -- Array
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_JSON_ARRAY nodeValue) = do
    DPIJsonArray{..} <- peek =<< dpiDataBuffer_getAsJsonArray nodeValue
    values <- mapM buildValue =<< peekArray (fromIntegral djaNumElements) djaElements
    pure $ Aeson.Array $ Vector.fromList values

  -- Number returned as DPIBytes
  buildValue (DPIJsonNode DPI_ORACLE_TYPE_NUMBER DPI_NATIVE_TYPE_BYTES nodeValue) = do
    DPIBytes{..} <- peek =<< dpiDataBuffer_getAsBytes nodeValue
    bytes <- packCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)
    let numStr = C8.unpack bytes
    number <- evaluate (read numStr) `catch` (\(_ :: SomeException) -> throwIO $ InvalidNumber numStr)
    pure $ Aeson.Number number

  -- String
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_BYTES nodeValue) = do
    DPIBytes{..} <- peek =<< dpiDataBuffer_getAsBytes nodeValue
    bytes <- packCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)
    pure $ Aeson.String (decodeUtf8 bytes)

  -- Number encoded as Double (will not fire as dpiJsonOptions_numberAsString is set)
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_DOUBLE nodeValue) = do
    doubleVal <- dpiDataBuffer_getAsDouble nodeValue
    pure $ Aeson.Number $ fromFloatDigits doubleVal

  -- Boolean literals (true, false)
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_BOOLEAN nodeValue) = do
    intVal <- dpiDataBuffer_getAsBoolean nodeValue
    pure $ Aeson.Bool (intVal == 1)

  -- Null literal (null)
  buildValue (DPIJsonNode _ DPI_NATIVE_TYPE_NULL _) = pure Aeson.Null
  -- All other DPI native types
  buildValue (DPIJsonNode _ nativeType _) = throwIO $ UnsupportedDPINativeType nativeType

newtype DPIJson = DPIJson (Ptr DPIJson)
  deriving (Show, Eq)
  deriving newtype (Storable)

data DPIJsonNode = DPIJsonNode
  { djnOracleTypeNumber :: DPIOracleType
  , djnNativeTypeNumber :: DPINativeType
  , djnValue :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

data DPIJsonArray = DPIJsonArray
  { djaNumElements :: CUInt
  , djaElements :: Ptr DPIJsonNode
  , djaElementValues :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

data DPIJsonObject = DPIJsonObject
  { djoNumFields :: CUInt
  , djoFieldNames :: Ptr CString
  , djoFieldNameLengths :: Ptr CUInt
  , djoFields :: Ptr DPIJsonNode
  , fieldValues :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

foreign import ccall "dpiData_getJson"
  dpiData_getJson :: Ptr (DPIData ReadBuffer) -> IO DPIJson

foreign import ccall "dpiJson_getValue"
  dpiJson_getValue' :: DPIJson -> CUInt -> Ptr (Ptr DPIJsonNode) -> IO CInt

dpiJson_getValue :: DPIJson -> IO (Ptr DPIJsonNode)
dpiJson_getValue dpiJson = alloca $ \ptr -> do
  dpiJson_getValue' dpiJson dpiJsonOptions_numberAsString ptr
  peek ptr
 where
  dpiJsonOptions_numberAsString = 0x01 -- return data from numeric fields as DPIBytes

foreign import ccall "dpiDataBuffer_getAsJsonObject"
  dpiDataBuffer_getAsJsonObject :: Ptr ReadBuffer -> IO (Ptr DPIJsonObject)

foreign import ccall "dpiDataBuffer_getAsJsonArray"
  dpiDataBuffer_getAsJsonArray :: Ptr ReadBuffer -> IO (Ptr DPIJsonArray)

foreign import ccall "dpiDataBuffer_getAsBytes"
  dpiDataBuffer_getAsBytes :: Ptr ReadBuffer -> IO (Ptr DPIBytes)

foreign import ccall "dpiDataBuffer_getAsBoolean"
  dpiDataBuffer_getAsBoolean :: Ptr ReadBuffer -> IO CInt

foreign import ccall "dpiDataBuffer_getAsDouble"
  dpiDataBuffer_getAsDouble :: Ptr ReadBuffer -> IO CDouble

data JsonDecodeError = InvalidNumber String | ParseError String | UnsupportedDPINativeType DPINativeType
  deriving (Show)

instance Exception JsonDecodeError where
  displayException (ParseError msg) = "Failed to parse JSON: " <> msg
  displayException (InvalidNumber numStr) =
    "While parsing JSON node, encountered invalid numeric value '" <> numStr <> "'"
  displayException (UnsupportedDPINativeType nativeType) =
    "While parsing JSON node, encountered unsupported DPI native type " <> show nativeType
