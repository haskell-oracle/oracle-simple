{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Oracle.Simple.JSON where

import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.ByteString (packCStringLen)
import qualified Data.ByteString.Char8 as C8
import Data.List as L
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as Vector
import Foreign (Ptr, Storable, alloca, peek, peekArray)
import Foreign.C (CInt (CInt), CString, CUInt (CUInt), peekCStringLen)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)

import Database.Oracle.Simple.Internal
  ( DPIBytes (DPIBytes, dpiBytesLength, dpiBytesPtr)
  , DPIData
  , DPINativeType
    ( DPI_NATIVE_TYPE_BOOLEAN
    , DPI_NATIVE_TYPE_BYTES
    , DPI_NATIVE_TYPE_JSON_ARRAY
    , DPI_NATIVE_TYPE_JSON_OBJECT
    , DPI_NATIVE_TYPE_NULL
    )
  , ReadBuffer
  , uintToDPINativeType
  )

newtype DPIJson = DPIJson (Ptr DPIJson)
  deriving (Show, Eq)
  deriving newtype (Storable)

data DPIJsonNode = DPIJsonNode
  { dpiOracleTypeNum :: CUInt
  , dpiNativeTypeNumber :: CUInt
  , dpiValue :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

data DPIJsonArray = DPIJsonArray
  { numElements :: CUInt
  , elements :: Ptr DPIJsonNode
  , elementValues :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

data DPIJsonObject = DPIJsonObject
  { oNumElements :: CUInt
  , fieldNames :: Ptr CString
  , fieldNameLengths :: Ptr CUInt
  , fields :: Ptr DPIJsonNode
  , fieldValues :: Ptr ReadBuffer
  }
  deriving (Generic)
  deriving anyclass (GStorable)

foreign import ccall "dpiData_getJson"
  dpiData_getJSON :: Ptr (DPIData ReadBuffer) -> IO DPIJson

foreign import ccall "dpiData_getJsonObject"
  dpiData_getJSONObject :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPIJsonObject)

foreign import ccall "dpiJson_getValue"
  dpiJson_getValue :: DPIJson -> CUInt -> Ptr (Ptr DPIJsonNode) -> IO CInt

getValue :: DPIJson -> IO (Ptr DPIJsonNode)
getValue dpiJson = alloca $ \ptr -> do
  dpiJson_getValue dpiJson 1 ptr
  peek ptr

foreign import ccall "getJsonObject"
  getJsonObject :: Ptr ReadBuffer -> IO (Ptr DPIJsonObject)

foreign import ccall "getJsonArray"
  getJsonArray :: Ptr ReadBuffer -> IO (Ptr DPIJsonArray)

foreign import ccall "getDpiBytes"
  getBytes :: Ptr ReadBuffer -> IO (Ptr DPIBytes)

foreign import ccall "dpiDataBuffer_getAsBoolean"
  dpiDataBuffer_getAsBoolean :: Ptr ReadBuffer -> IO CInt

parseNode :: DPIJsonNode -> IO Aeson.Value
parseNode DPIJsonNode{..} = do
  case uintToDPINativeType dpiNativeTypeNumber of
    -- object
    Just DPI_NATIVE_TYPE_JSON_OBJECT -> do
      DPIJsonObject{..} <- peek =<< getJsonObject dpiValue
      fieldNamesArray <- peekArray (fromIntegral oNumElements) fieldNames
      fieldNameLengthsArray <- (fmap fromIntegral) <$> peekArray (fromIntegral oNumElements) fieldNameLengths
      keys <- mapM (fmap fromString . peekCStringLen) (L.zip fieldNamesArray fieldNameLengthsArray)
      values <- mapM parseNode =<< peekArray (fromIntegral oNumElements) fields
      pure $ Aeson.Object $ KeyMap.fromList (L.zip keys values)

    -- array
    Just DPI_NATIVE_TYPE_JSON_ARRAY -> do
      DPIJsonArray{..} <- peek =<< getJsonArray dpiValue
      values <- mapM parseNode =<< peekArray (fromIntegral numElements) elements
      pure $ Aeson.Array $ Vector.fromList values

    -- string and number
    Just DPI_NATIVE_TYPE_BYTES -> do
      DPIBytes{..} <- peek =<< getBytes dpiValue
      bytes <- packCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)

      case dpiOracleTypeNum of
        -- for numbers encoded as strings
        2010 -> pure $ Aeson.Number (read $ C8.unpack bytes)
        -- for all strings
        _ -> pure $ Aeson.String (decodeUtf8 bytes)

    -- true or false literals
    Just DPI_NATIVE_TYPE_BOOLEAN -> pure . Aeson.Bool . (== 1) =<< dpiDataBuffer_getAsBoolean dpiValue
    -- null
    Just DPI_NATIVE_TYPE_NULL -> pure Aeson.Null
    -- Just DPI_NATIVE_TYPE_TIMESTAMP -> putStrLn "got timestamp"
    -- Just DPI_NATIVE_TYPE_DOUBLE -> putStrLn "got double"

    Just dpiNativeType -> error $ "Unimplemented type: " <> show dpiNativeType
    _ -> error $ "oops! got: " <> show dpiNativeTypeNumber
