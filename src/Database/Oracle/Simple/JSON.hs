{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
module Database.Oracle.Simple.JSON where


import Foreign.Storable.Generic
import GHC.Generics
import Database.Oracle.Simple.Internal
import Foreign
import Foreign.C

data DPIJsonNode =
  DPIJsonNode
  { dpiOracleTypeNum :: CUInt
  , dpiNativeTypeNumber :: CUInt 
  , dpiValue :: Ptr ReadBuffer 
  }
  deriving (Generic, GStorable)

data DPIJsonArray =
  DPIJsonArray
  { numElements :: CUInt
  , elements :: Ptr DPIJsonNode
  , elementValues :: Ptr ReadBuffer
  }

data DPIJsonObject =
  DPIJsonObject
  { oNumElements :: CUInt
  , fieldNames :: Ptr CString
  , fieldNameLengths :: Ptr CUInt
  , fields :: Ptr DPIJsonNode
  , fieldValues :: Ptr ReadBuffer
  }

walkNode :: DPIJsonNode -> IO ()
walkNode DPIJsonNode{..} = do
  case uintToDPINativeType dpiNativeTypeNumber of
    Just DPI_NATIVE_TYPE_JSON_OBJECT -> putStrLn "got object"
    Just DPI_NATIVE_TYPE_JSON_ARRAY -> putStrLn "got array"
    Just DPI_NATIVE_TYPE_BYTES -> putStrLn "got bytes"
    Just DPI_NATIVE_TYPE_DOUBLE -> putStrLn "got double"
    Just DPI_NATIVE_TYPE_BOOLEAN -> putStrLn "got boolean"
    Just DPI_NATIVE_TYPE_NULL -> putStrLn "got null"
    Just DPI_NATIVE_TYPE_TIMESTAMP -> putStrLn "got timestamp"
    Nothing -> putStrLn $ "oops! got: " <> show dpiNativeTypeNumber

newtype DPIJson = DPIJson (Ptr DPIJson)
  deriving (Show, Eq)
  deriving newtype (Storable)

foreign import ccall "dpiData_getJson"
  dpiData_getJSON :: Ptr (DPIData ReadBuffer) -> IO DPIJson

foreign import ccall "dpiJson_getValue"
  dpiJson_getValue :: DPIJson -> CUInt -> Ptr (Ptr DPIJsonNode) -> IO CInt

runGetValue :: DPIJson -> IO DPIJsonNode
runGetValue dpiJson = alloca $ \ptr -> do
  dpiJson_getValue dpiJson 0 ptr
  i <- peek ptr
  peek i
