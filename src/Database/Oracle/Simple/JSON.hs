{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.JSON where

import Control.Monad ((<=<))
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
import Data.Proxy (Proxy(Proxy))

import Database.Oracle.Simple.FromField (FromField(fromField), ReadDPIBuffer, FieldParser(FieldParser))
import Database.Oracle.Simple.Internal
  ( DPIBytes (DPIBytes, dpiBytesLength, dpiBytesPtr)
  , DPIData
  , DPINativeType
    ( DPI_NATIVE_TYPE_BOOLEAN
    , DPI_NATIVE_TYPE_BYTES
    , DPI_NATIVE_TYPE_JSON_ARRAY
    , DPI_NATIVE_TYPE_JSON_OBJECT
    , DPI_NATIVE_TYPE_NULL, DPI_NATIVE_TYPE_JSON
    )
  , ReadBuffer
  , uintToDPINativeType, HasDPINativeType(dpiNativeType)
  )

instance {-# OVERLAPPABLE #-} Aeson.FromJSON a => HasDPINativeType a where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_JSON

instance {-# OVERLAPPABLE #-} Aeson.FromJSON a => FromField a where
  fromField = FieldParser getJson

getJson :: Aeson.FromJSON a => ReadDPIBuffer a
getJson = fromValue <=< buildValue <=< peek <=< dpiJson_getValue <=< dpiData_getJson
 where
  fromValue value = case Aeson.fromJSON value of
                      Aeson.Error msg -> error msg
                      Aeson.Success a -> pure a

newtype DPIJson = DPIJson (Ptr DPIJson)
  deriving (Show, Eq)
  deriving newtype (Storable)

data DPIJsonNode = DPIJsonNode
  { djnOracleTypeNumber :: CUInt
  , djnNativeTypeNumber :: CUInt
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
  dpiJson_getValue' dpiJson 1 ptr
  peek ptr

foreign import ccall "getJsonObject"
  getJsonObject :: Ptr ReadBuffer -> IO (Ptr DPIJsonObject)

foreign import ccall "getJsonArray"
  getJsonArray :: Ptr ReadBuffer -> IO (Ptr DPIJsonArray)

foreign import ccall "getDpiBytes"
  getBytes :: Ptr ReadBuffer -> IO (Ptr DPIBytes)

foreign import ccall "dpiDataBuffer_getAsBoolean"
  dpiDataBuffer_getAsBoolean :: Ptr ReadBuffer -> IO CInt

buildValue :: DPIJsonNode -> IO Aeson.Value
buildValue DPIJsonNode{..} = do
  case uintToDPINativeType djnNativeTypeNumber of
    -- object
    Just DPI_NATIVE_TYPE_JSON_OBJECT -> do
      DPIJsonObject{..} <- peek =<< getJsonObject djnValue
      djoFieldNamesArray <- peekArray (fromIntegral djoNumFields) djoFieldNames
      djoFieldNameLengthsArray <- (fmap fromIntegral) <$> peekArray (fromIntegral djoNumFields) djoFieldNameLengths
      keys <- mapM (fmap fromString . peekCStringLen) (L.zip djoFieldNamesArray djoFieldNameLengthsArray)
      values <- mapM buildValue =<< peekArray (fromIntegral djoNumFields) djoFields
      pure $ Aeson.Object $ KeyMap.fromList (L.zip keys values)

    -- array
    Just DPI_NATIVE_TYPE_JSON_ARRAY -> do
      DPIJsonArray{..} <- peek =<< getJsonArray djnValue
      values <- mapM buildValue =<< peekArray (fromIntegral djaNumElements) djaElements
      pure $ Aeson.Array $ Vector.fromList values

    -- string and number
    Just DPI_NATIVE_TYPE_BYTES -> do
      DPIBytes{..} <- peek =<< getBytes djnValue
      bytes <- packCStringLen (dpiBytesPtr, fromIntegral dpiBytesLength)

      case djnOracleTypeNumber of
        -- for numbers encoded as strings
        2010 -> pure $ Aeson.Number (read $ C8.unpack bytes)
        -- for all strings
        _ -> pure $ Aeson.String (decodeUtf8 bytes)

    -- true or false literals
    Just DPI_NATIVE_TYPE_BOOLEAN -> pure . Aeson.Bool . (== 1) =<< dpiDataBuffer_getAsBoolean djnValue 
    -- null
    Just DPI_NATIVE_TYPE_NULL -> pure Aeson.Null
    -- Just DPI_NATIVE_TYPE_TIMESTAMP -> putStrLn "got timestamp"
    -- Just DPI_NATIVE_TYPE_DOUBLE -> putStrLn "got double"

    Just dpiNativeType -> error $ "Unimplemented type: " <> show dpiNativeType
    _ -> error $ "oops! got: " <> show djnNativeTypeNumber
