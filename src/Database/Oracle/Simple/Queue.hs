{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Oracle.Simple.Queue (
    DPIQueue (..)
  , DPIMsgProps (..)
  , DPIDeqOptions (..)
  , DPIEnqOptions (..)
  , DPIObjectType (..)
  , deqMany
  , deqOne
  , enqMany
  , enqOne 
  , getDeqOptions
  , getEnqOptions
  , queueRelease 
  , genQueueJSON
  , genQueueObject
  , genMsgProps
  , genQueue 
  , genJSON 
  , getMsgPropsNumOfAttempts 
  , getMsgPropsDelay
  , getMsgPropsPayLoadBytes
  , getMsgPropsPayLoadJson 
  , getMsgPropsPayLoadObject 
  , setMsgPropsPayLoadBytes 
  , setMsgPropsPayLoadJSON 
  , setMsgPropsPayLoadObject
  , objectAppendElement 
  , getObjectElementByIdx
  , setObjectElementByIdx
  , getJsonFromText 
  , dpiJsonToVal 
) where

import Foreign (alloca, withArray, withForeignPtr, nullPtr)
import Foreign.Storable.Generic (Storable (..))
import Foreign.C.Types (CInt (..), CUInt (..), CULong(..)) 
import Foreign.Ptr (Ptr)
import Foreign.C.String
import Database.Oracle.Simple.Internal
import Database.Oracle.Simple.Object
import Database.Oracle.Simple.ToField
import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.JSON
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Char8 as BSC
import Data.Proxy (Proxy (..))

newtype DPIQueue = DPIQueue (Ptr DPIQueue)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIMsgProps = DPIMsgProps (Ptr DPIMsgProps)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIDeqOptions = DPIDeqOptions (Ptr DPIDeqOptions)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIEnqOptions = DPIEnqOptions (Ptr DPIEnqOptions)
  deriving (Show, Eq)
  deriving newtype (Storable)

deqMany :: DPIQueue -> Int -> IO DPIMsgProps
deqMany dpiQueue numProps = do
  alloca $ \dpiMsgPropsPtr -> do
    alloca $ \numPropsPtr -> do
      poke numPropsPtr (fromIntegral numProps)
      throwOracleError =<<
        dpiQueue_deqMany dpiQueue numPropsPtr dpiMsgPropsPtr
      peek dpiMsgPropsPtr

foreign import ccall unsafe "dpiQueue_deqMany"
  dpiQueue_deqMany ::
    -- | dpiQueue *
    DPIQueue ->
    -- | numProps *
    Ptr CUInt ->
    -- | props **
    Ptr DPIMsgProps ->
    IO CInt
 
deqOne :: DPIQueue -> IO DPIMsgProps
deqOne dpiQueue = do
  alloca $ \dpiMsgPropsPtr -> do
    throwOracleError =<<
      dpiQueue_deqOne dpiQueue dpiMsgPropsPtr
    peek dpiMsgPropsPtr
            
foreign import ccall unsafe "dpiQueue_deqOne"
  dpiQueue_deqOne ::
    -- | dpiQueue *
    DPIQueue ->
    -- | props **
    Ptr DPIMsgProps ->
    IO CInt

{- |
Warning: calling this function in parallel on different connections acquired from
the same pool may fail due to Oracle bug 29928074. Ensure that this function is not
run in parallel, use standalone connections or connections from different pools, or
make multiple calls to dpiQueue_enqOne() instead. The function dpiQueue_deqMany() call is not affected.
-}
enqMany :: DPIQueue -> [DPIMsgProps] -> IO ()
enqMany dpiQueue dpiMsgPropss = do
  let numOfProps = length dpiMsgPropss
  withArray dpiMsgPropss $ \dpiMsgPropsPtr -> do
    alloca $ \numPropsPtr -> do
      poke numPropsPtr (fromIntegral numOfProps)
      throwOracleError =<<
        dpiQueue_enqMany dpiQueue numPropsPtr dpiMsgPropsPtr

foreign import ccall unsafe "dpiQueue_enqMany"
  dpiQueue_enqMany ::
    -- | dpiQueue *
    DPIQueue ->
    -- | numProps *
    Ptr CUInt ->
    -- | props **
    Ptr DPIMsgProps ->
    IO CInt

enqOne :: DPIQueue -> DPIMsgProps -> IO ()
enqOne dpiQueue dpiMsgProps = 
    throwOracleError =<<
        dpiQueue_enqOne dpiQueue dpiMsgProps

foreign import ccall unsafe "dpiQueue_enqOne"
  dpiQueue_enqOne ::
    -- | dpiQueue *
    DPIQueue ->
    -- | props *
    DPIMsgProps ->
    IO CInt

getDeqOptions :: DPIQueue -> IO DPIDeqOptions
getDeqOptions dpiQueue = do
  alloca $ \dpiDeqOptionsPtr -> do
    throwOracleError =<< dpiQueue_getDeqOptions dpiQueue dpiDeqOptionsPtr
    peek dpiDeqOptionsPtr

foreign import ccall unsafe "dpiQueue_getDeqOptions"
  dpiQueue_getDeqOptions ::
    -- | dpiQueue *
    DPIQueue ->
    -- | options **
    Ptr DPIDeqOptions ->
    IO CInt

getEnqOptions :: DPIQueue -> IO DPIEnqOptions
getEnqOptions dpiQueue = do 
  alloca $ \dpiEnqOptionsPtr -> do
    throwOracleError =<< dpiQueue_getEnqOptions dpiQueue dpiEnqOptionsPtr
    peek dpiEnqOptionsPtr

foreign import ccall unsafe "dpiQueue_getEnqOptions"
  dpiQueue_getEnqOptions ::
    -- | dpiQueue *
    DPIQueue ->
    -- | options **
    Ptr DPIEnqOptions ->
    IO CInt

queueRelease :: DPIQueue -> IO ()
queueRelease dpiQueue = throwOracleError =<< dpiQueue_release dpiQueue

foreign import ccall unsafe "dpiQueue_release"
  dpiQueue_release ::
    -- | dpiQueue *
    DPIQueue ->
    IO CInt

genQueueJSON :: Connection -> String -> IO DPIQueue
genQueueJSON (Connection fptr) queueName = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiQueuePtr -> do
      withCStringLen queueName $ \(queueNameC , fromIntegral -> queueNameLen) -> do
        throwOracleError =<< dpiConn_newJsonQueue conn queueNameC queueNameLen dpiQueuePtr
        peek dpiQueuePtr

foreign import ccall unsafe "dpiConn_newJsonQueue"
  dpiConn_newJsonQueue ::
    -- | dpiConn *
    Ptr DPIConn ->
    -- | char* name 
    CString ->
    -- | name Length
    CUInt ->
    -- | dpiQueue **
    Ptr DPIQueue ->
    IO CInt

genMsgProps :: Connection -> IO DPIMsgProps
genMsgProps (Connection fptr) = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiMsgPropsPtr -> do
      throwOracleError =<< dpiConn_newMsgProps conn dpiMsgPropsPtr
      peek dpiMsgPropsPtr

foreign import ccall unsafe "dpiConn_newMsgProps"
  dpiConn_newMsgProps ::
    -- | dpiConn *
    Ptr DPIConn ->
    -- | dpiMsgProps **
    Ptr DPIMsgProps ->
    IO CInt

genQueue :: Connection -> String -> IO DPIQueue
genQueue (Connection fptr) queueName = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiQueuePtr -> do
      withCStringLen queueName $ \(queueNameC , fromIntegral -> queueNameLen) -> do
        throwOracleError =<< dpiConn_newQueue conn queueNameC queueNameLen nullPtr dpiQueuePtr
        peek dpiQueuePtr

genQueueObject :: Connection -> String -> DPIObjectType -> IO DPIQueue
genQueueObject (Connection fptr) queueName (DPIObjectType objectType) = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiQueuePtr -> do
      withCStringLen queueName $ \(queueNameC , fromIntegral -> queueNameLen) -> do
        throwOracleError =<< dpiConn_newQueue conn queueNameC queueNameLen objectType dpiQueuePtr
        peek dpiQueuePtr

foreign import ccall unsafe "dpiConn_newQueue"
  dpiConn_newQueue ::
    -- | dpiConn *
    Ptr DPIConn ->
    -- | char* name 
    CString ->
    -- | name Length
    CUInt ->
    -- | dpiObjectType *
    Ptr DPIObjectType ->
    -- | dpiQueue **
    Ptr DPIQueue ->
    IO CInt

-----x DPI MsgProps related functions x-----

getMsgPropsNumOfAttempts :: DPIMsgProps -> IO Int
getMsgPropsNumOfAttempts dpiMsgProps = do
  alloca $ \numPtr -> do
    throwOracleError =<< dpiMsgProps_getNumAttempts dpiMsgProps numPtr
    fromIntegral <$> peek numPtr

foreign import ccall unsafe "dpiMsgProps_getNumAttempts"
  dpiMsgProps_getNumAttempts ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | Number of Attempts that will be read.
    Ptr CUInt ->
    IO CInt

getMsgPropsDelay :: DPIMsgProps -> IO Int
getMsgPropsDelay dpiMsgProps = do
  alloca $ \numPtr -> do
    throwOracleError =<< dpiMsgProps_getDelay dpiMsgProps numPtr
    fromIntegral <$> peek numPtr

foreign import ccall unsafe "dpiMsgProps_getDelay"
  dpiMsgProps_getDelay ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | Number of delayed seconds from given Message prop.
    Ptr CUInt ->
    IO CInt

{-
This function internally calls getPayLoad which either returns payLoad in either Object or in bytes.
Hence, the result might be null.
-}
getMsgPropsPayLoadBytes :: DPIMsgProps -> IO (Maybe BSC.ByteString)
getMsgPropsPayLoadBytes dpiMsgProps = do
  alloca $ \dpiObjectPtr -> do
      alloca $ \cStringPtr -> do
        alloca $ \cStringLengthptr -> do
            throwOracleError =<< dpiMsgProps_getPayload dpiMsgProps dpiObjectPtr cStringPtr cStringLengthptr
            cStr <- peek cStringPtr
            if cStr == nullPtr 
              then return Nothing
            else Just . BSC.pack <$> peekCString cStr

getMsgPropsPayLoadObject :: DPIMsgProps -> IO (Maybe DPIObject)
getMsgPropsPayLoadObject dpiMsgProps = 
  alloca $ \dpiObjectPtr -> do
        throwOracleError =<< dpiMsgProps_getPayload dpiMsgProps dpiObjectPtr nullPtr nullPtr
        if dpiObjectPtr  == nullPtr
            then return Nothing
        else Just <$> peek dpiObjectPtr 

foreign import ccall unsafe "dpiMsgProps_getPayload"
  dpiMsgProps_getPayload ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiObject **
    Ptr DPIObject ->
    -- | const char ** value
    Ptr CString ->
    -- | valueLength
    Ptr CUInt ->
    IO CInt

getMsgPropsPayLoadJson :: FromJSON a => DPIMsgProps -> IO (Maybe a)
getMsgPropsPayLoadJson dpiMsgProps = do
  alloca $ \dpiJsonPtr -> do
    throwOracleError =<< dpiMsgProps_getPayloadJson dpiMsgProps dpiJsonPtr
    if (dpiJsonPtr == nullPtr) then return Nothing
    else do 
      dpiJson <- peek dpiJsonPtr
      res <- dpiJsonToVal dpiJson
      return $ Just res

foreign import ccall unsafe "dpiMsgProps_getPayloadJson"
  dpiMsgProps_getPayloadJson ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiJson **
    Ptr  DPIJson ->
    IO CInt

setMsgPropsPayLoadBytes :: DPIMsgProps -> BSC.ByteString -> IO ()
setMsgPropsPayLoadBytes dpiMsgProps payLoad = do
  withCStringLen (BSC.unpack payLoad) $ \(payLoadC , fromIntegral -> payLoadLen) -> do
    throwOracleError =<< dpiMsgProps_setPayloadBytes dpiMsgProps payLoadC payLoadLen

foreign import ccall unsafe "dpiMsgProps_setPayloadBytes"
    dpiMsgProps_setPayloadBytes ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | const char * value
    CString ->
    -- | uint32 valueLength
    CUInt ->
    IO CInt

setMsgPropsPayLoadObject :: DPIMsgProps -> DPIObject-> IO ()
setMsgPropsPayLoadObject dpiMsgProps obj = do
  throwOracleError =<< dpiMsgProps_setPayloadObject dpiMsgProps obj

foreign import ccall unsafe "dpiMsgProps_setPayloadObject"
    dpiMsgProps_setPayloadObject ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiObject* obj
    DPIObject ->
    IO CInt

setMsgPropsPayLoadJSON :: ToJSON a => Connection -> DPIMsgProps -> a -> IO ()
setMsgPropsPayLoadJSON c dpiMsgProps jsonData = do
  dpiJson <- getJsonFromType c jsonData
  throwOracleError =<< dpiMsgProps_setPayloadJson dpiMsgProps dpiJson

foreign import ccall unsafe "dpiMsgProps_setPayloadJson"
    dpiMsgProps_setPayloadJson ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiJson * 
    DPIJson ->
    IO CInt

objectAppendElement :: forall a. (ToField a) => DPIObject -> a -> IO ()
objectAppendElement obj val = do
    dataValue <- toField val
    let dataIsNull = case dataValue of
                            AsNull -> 1
                            _ -> 0
    alloca $ \dpiDataPtr -> do
      let dpiData = DPIData{..}
      poke dpiDataPtr (dpiData :: DPIData WriteBuffer)
      throwOracleError =<< 
        dpiObject_appendElement 
            obj 
            (dpiNativeTypeToUInt (toDPINativeType (Proxy @a))) 
            (dpiDataPtr :: Ptr (DPIData WriteBuffer))

foreign import ccall unsafe "dpiObject_appendElement"
    dpiObject_appendElement ::
    -- | dpiObject *
    DPIObject ->
    -- | dpiNativeTypeNum
    CUInt ->
    -- | dpiData* val
    Ptr (DPIData WriteBuffer) ->
    IO CInt

getObjectElementByIdx 
    :: forall a. (FromField a) => 
    DPIObject -> 
    Int -> 
    IO a
getObjectElementByIdx obj idx = do
    alloca $ \dpiDataPtr -> do
      throwOracleError =<< 
        dpiObject_getElementValueByIndex 
            obj 
            (CInt $ fromIntegral idx)
            (dpiNativeTypeToUInt (fromDPINativeType (Proxy @a)))
            dpiDataPtr
      readDPIDataBuffer (fromField @a) dpiDataPtr

foreign import ccall unsafe "dpiObject_getElementValueByIndex"
    dpiObject_getElementValueByIndex ::
    -- | dpiObject *
    DPIObject ->
    -- | int32_t index
    CInt ->
    -- | dpiNativeTypeNum
    CUInt ->
    -- | dpiData *
    Ptr (DPIData ReadBuffer) ->
    IO CInt

setObjectElementByIdx 
    :: forall a. (ToField a) => 
    DPIObject -> 
    Int ->
    a ->
    IO ()
setObjectElementByIdx obj idx val = do
    dataValue <- toField val
    let dataIsNull = case dataValue of
                            AsNull -> 1
                            _ -> 0
    alloca $ \dpiDataPtr -> do
      let dpiData = DPIData{..}
      poke dpiDataPtr (dpiData :: DPIData WriteBuffer)
      throwOracleError =<< 
        dpiObject_setElementValueByIndex 
            obj 
            (CInt $ fromIntegral idx)
            (dpiNativeTypeToUInt (toDPINativeType (Proxy @a)))
            dpiDataPtr

foreign import ccall unsafe "dpiObject_setElementValueByIndex"
    dpiObject_setElementValueByIndex ::
    -- | dpiObject *
    DPIObject ->
    -- | int32_t index
    CInt ->
    -- | dpiNativeTypeNum
    CUInt ->
    -- | dpiData *
    Ptr (DPIData WriteBuffer) ->
    IO CInt

genJSON :: Connection -> IO DPIJson
genJSON (Connection fptr) = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \jsonPtr -> do
      throwOracleError =<< dpiConn_newJson conn jsonPtr
      peek jsonPtr

foreign import ccall unsafe "dpiConn_newJson"
  dpiConn_newJson ::
    -- | dpiConn *
    Ptr DPIConn ->
    -- | dpiJSON **
    Ptr DPIJson ->
    IO CInt

getJsonFromType :: forall a. (ToJSON a) => Connection -> a -> IO DPIJson
getJsonFromType c jsonData = do
  (AsBytes res) <- toField ( AesonField jsonData)
  res_ <- mkStringFromDPIBytesUTF8 res
  getJsonFromText c res_

dpiJsonToVal :: FromJSON a => DPIJson -> IO a
dpiJsonToVal dpiJson = do
  jsonNodePtr <- dpiJson_getValue dpiJson
  jsonNode <- (peek jsonNodePtr)
  parseJson jsonNode

getJsonFromText :: Connection  -> String -> IO DPIJson
getJsonFromText c jsonVal = do
    json <- genJSON c
    withCStringLen jsonVal $ \ (jsonString, jsonStringLen) -> do
      throwOracleError =<< dpiJson_setFromText json jsonString (fromIntegral jsonStringLen) 0
      return json

foreign import ccall unsafe "dpiJson_setFromText"
  dpiJson_setFromText ::
    -- | dpiJson *
    DPIJson ->
    -- | const char *value
    CString ->
    -- | uint64_t
    CULong ->
    -- | flags
    CUInt -> 
    IO CInt
