{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Oracle.Simple.Object (
    genObject,
    getObjectType,
    getObjAttributes,
    setObjAttribute,
    getObjAttribute,
    releaseObject,
    getObjectInfo,
    getAttributeInfo,
    DPIObjectType (..),
    DPIObject (..),
    ObjectTypeInfo (numAttributes, isCollection ),
) where

import Database.Oracle.Simple.Internal
import Foreign (alloca, withForeignPtr, peekArray)
import Foreign.Storable.Generic (Storable (..))
import Foreign.C.Types (CInt (..), CUInt (..), CUShort(..), CShort, CSChar, CUChar) 
import Foreign.Ptr (Ptr)
import Foreign.C.String
import Data.Char (toUpper)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics
import Database.Oracle.Simple.ToField
import Database.Oracle.Simple.FromField
import Data.Proxy (Proxy (..))

-- | `DPIObjectType` is a newtype wrapper around a pointer to a DPI objectType.
-- | It is primarily used for representing and interacting with Objects
newtype DPIObjectType = DPIObjectType (Ptr DPIObjectType)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | `DPIObject` is a newtype wrapper around a pointer to a DPI object.
-- | It is primarily used for representing and interacting with objects
newtype DPIObject = DPIObject (Ptr DPIObject)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | `DPIObjectAttr` is a newtype wrapper around a pointer to a DPI Object Attribute.
-- | It is primarily used for representing and interacting with objects
newtype DPIObjectAttr = DPIObjectAttr (Ptr DPIObjectAttr)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | `DPIObjectAttrInfo` is a newtype wrapper around a pointer to a DPI Object Attribute info.
-- | It is primarily used for representing and interacting with objects
data ObjectAttrInfo = ObjectAttrInfo {
    name :: CString
  , nameLength :: CUInt
  , typeInfo :: DPIDataTypeInfo
} deriving (Show, Eq, Generic, GStorable)

data DPIDataTypeInfo = DPIDataTypeInfo {
    oracleTypeNum :: CUInt
    , defaultNativeTypeNum :: CUInt
    , ociTypeCode :: CUShort
    , dbSizeInBytes :: CUInt
    , clientSizeInBytes :: CUInt
    , sizeInChars :: CUInt
    , precision :: CShort
    , scale :: CSChar
    , fsPrecision :: CUChar
    , objectType :: DPIObjectType
    , isJson :: CInt
    , domainSchema :: CString
    , domainSchemaLength :: CUInt
    , domainName :: CString
    , domainNameLength :: CUInt
    , numAnnotations :: CUInt
    , annotations :: Ptr ()
    , isOson :: CInt
    , vectorDimensions :: CUInt
    , vectorFormat :: CUChar
    , vectorFlags :: CUChar
} deriving (Eq, Show, Generic, GStorable)

-- | This data structure contains metadata about the object type
data ObjectTypeInfo = ObjectTypeInfo {
    schema :: CString
  ,  schemaLength :: CInt
  ,  name :: CString
  ,  nameLength :: CInt
  ,  isCollection :: Bool
  ,  elementTypeInfo :: DPIDataTypeInfo
  ,  numAttributes :: CUShort
  ,  packageName :: CString
  ,  packageNameLength :: CUInt
} deriving (Eq, Show, GStorable, Generic)

-- | Returns the value of one of the object’s attributes.
getObjAttribute :: forall a. (FromField a) => DPIObject -> DPIObjectAttr -> IO a
getObjAttribute obj objTypeAttr = do
  alloca $ \dpiDataPtr -> do
    throwOracleError =<< dpiObject_getAttributeValue 
                                obj 
                                objTypeAttr 
                                (dpiNativeTypeToUInt (fromDPINativeType (Proxy @a)))
                                dpiDataPtr
    readDPIDataBuffer (fromField @a) dpiDataPtr

foreign import ccall unsafe "dpiObject_getAttributeValue"
    dpiObject_getAttributeValue ::
    -- | dpiObject *
    DPIObject ->
    -- | dpiObjectAttr* 
    DPIObjectAttr ->
    -- | dpiNativeTypeNum
    CUInt ->
    -- | dpiData *
    Ptr (DPIData ReadBuffer) ->
    IO CInt

-- | Sets the value of one of the object’s attributes.
setObjAttribute :: forall a. (ToField a) => DPIObject -> DPIObjectAttr -> a -> IO ()
setObjAttribute obj objTypeAttr val = do
  dataValue <- toField val
  let dataIsNull = case dataValue of
                            AsNull -> 1
                            _ -> 0
  alloca $ \dpiDataPtr -> do
    let dpiData = DPIData{..}
    poke dpiDataPtr (dpiData :: DPIData WriteBuffer)
    throwOracleError =<< dpiObject_setAttributeValue 
                                obj 
                                objTypeAttr 
                                (dpiNativeTypeToUInt (toDPINativeType (Proxy @a)))
                                dpiDataPtr

foreign import ccall unsafe "dpiObject_setAttributeValue"
    dpiObject_setAttributeValue ::
    -- | dpiObject *
    DPIObject ->
    -- | dpiObjectAttr* 
    DPIObjectAttr ->
    -- | dpiNativeTypeNum
    CUInt ->
    -- | dpiData *
    Ptr (DPIData WriteBuffer) ->
    IO CInt

-- | Returns the list of attributes that belong to the object type.
getObjAttributes :: DPIObjectType -> Int -> IO [DPIObjectAttr]
getObjAttributes objType n = do
  -- objInfo <- getObjectInfo objType
  alloca $ \objAttrsPtr -> do
    throwOracleError =<< dpiObjectType_getAttributes objType (fromIntegral n) objAttrsPtr
    peekArray n objAttrsPtr

foreign import ccall unsafe "dpiObjectType_getAttributes"
    dpiObjectType_getAttributes ::
    -- | dpiObjectType *
    DPIObjectType ->
    -- | int16_t numAttributes
    CUShort ->
    -- | dpiObjectAttr *
    Ptr DPIObjectAttr ->
    IO CInt

-- | Returns information about the object type.
getObjectInfo :: DPIObjectType -> IO ObjectTypeInfo
getObjectInfo objType= do
  alloca $ \objectTypeInfoPtr -> do
    throwOracleError =<< dpiObjectType_getInfo objType objectTypeInfoPtr
    peek objectTypeInfoPtr

foreign import ccall unsafe "dpiObjectType_getInfo"
    dpiObjectType_getInfo ::
    -- | dpiObjectType *
    DPIObjectType ->
    -- | dpiObjectTypeInfo **
    Ptr ObjectTypeInfo ->
    IO CInt

{-
The name is uppercased! Because here Oracle seems to be case-sensitive.
-}
-- | Looks up an object type by name in the database and returns a reference to it.
-- | The name is uppercased! Because here Oracle seems to be case-sensitive.
getObjectType :: Connection -> String -> IO DPIObjectType
getObjectType (Connection fptr) objectName_ = do
  let objectName = map toUpper objectName_ 
  withForeignPtr fptr $ \conn -> do
    withCStringLen objectName $ \(objectNameC, fromIntegral -> objectNameLen) -> do
      alloca $ \objectTypePtr -> do
        throwOracleError =<< dpiConn_getObjectType conn objectNameC objectNameLen objectTypePtr
        peek objectTypePtr

foreign import ccall unsafe "dpiConn_getObjectType"
    dpiConn_getObjectType ::
    -- | dpiConn *
    Ptr DPIConn ->
    -- | char * name
    CString ->
    -- | cuint32_t nameLength
    CUInt -> 
    -- | dpiObjectType ** objType
    Ptr DPIObjectType ->
    IO CInt

-- | Creates an object of the specified type and returns a reference to it.
genObject :: DPIObjectType -> IO DPIObject
genObject objType = do
  alloca $ \objectPtr -> do
    throwOracleError =<< dpiObjectType_createObject objType objectPtr
    peek objectPtr

foreign import ccall unsafe "dpiObjectType_createObject"
    dpiObjectType_createObject ::
    -- | dpiObjectType *
    DPIObjectType ->
    -- | dpiObject ** obj
    Ptr DPIObject ->
    IO CInt

-- | Returns information about the attribute.
getAttributeInfo :: DPIObjectAttr -> IO ObjectAttrInfo
getAttributeInfo objAttr = do
  alloca $ \objectAttrInfoPtr -> do
    throwOracleError =<< dpiObjectAttr_getInfo objAttr objectAttrInfoPtr
    peek objectAttrInfoPtr

foreign import ccall unsafe "dpiObjectAttr_getInfo"
    dpiObjectAttr_getInfo ::
    -- | dpiObjectAttr *
    DPIObjectAttr ->
    -- | dpiObjectAttrInfo *
    Ptr ObjectAttrInfo ->
    IO CInt

-- | Releases a reference to the object. 
releaseObject :: DPIObject -> IO ()
releaseObject obj = do
  throwOracleError =<< dpiObject_release obj

foreign import ccall unsafe "dpiObject_release"
    dpiObject_release ::
    -- | dpiObject *
    DPIObject ->
    IO CInt