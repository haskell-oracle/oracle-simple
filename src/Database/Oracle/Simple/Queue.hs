{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Oracle.Simple.Queue (
    genQueue 
  , genQueueObject
  , genQueueJSON
  , genMsgProps
  , enqOne 
  , enqMany
  , deqOne
  , deqMany
  , setMsgPropsPayLoadBytes 
  , getMsgPropsPayLoadBytes
  , setMsgPropsPayLoadObject
  , getMsgPropsPayLoadObject 
  , setMsgPropsPayLoadJSON 
  , getMsgPropsPayLoadJson 
  , queueRelease 
  , getEnqOptions
  , getDeqOptions
  , getMsgPropsDelay
  , genJSON 
  , getMsgPropsNumOfAttempts 
  , objectAppendElement 
  , getObjectElementByIdx
  , setObjectElementByIdx
  , setTextInJson
  , dpiJsonToVal 
  , releaseDpiJson
  , setValInJSON
  , DPIQueue (..)
  , DPIMsgProps (..)
  , DPIDeqOptions (..)
  , DPIEnqOptions (..)
  , DPIObjectType (..)
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
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Proxy (Proxy (..))

newtype DPIQueue = DPIQueue (Ptr DPIQueue)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | This type represents a pointer to a `DPIMsgProps` struct,
-- | which contains metadata about an Oracle message, including its
-- | properties and attributes.
newtype DPIMsgProps = DPIMsgProps (Ptr DPIMsgProps)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | This type represents a pointer to a `DPIDeqOptions` struct.
newtype DPIDeqOptions = DPIDeqOptions (Ptr DPIDeqOptions)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | This type represents a pointer to a `DPIEnqOptions` struct.
newtype DPIEnqOptions = DPIEnqOptions (Ptr DPIEnqOptions)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | Dequeues multiple messages from the queue.
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

-- | Dequeues a single message from the queue.
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
Enqueues multiple messages into the queue.

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

-- | Enqueues a single mesasge into the queue.
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

-- | Returns a reference to the dequeue options associated with the queue. These options affect how messages are dequeued.
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

-- | Returns a reference to the enqueue options associated with the queue. These options affect how messages are enqueued.
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

-- | Releases a reference to the queue.
queueRelease :: DPIQueue -> IO ()
queueRelease dpiQueue = throwOracleError =<< dpiQueue_release dpiQueue

foreign import ccall unsafe "dpiQueue_release"
  dpiQueue_release ::
    -- | dpiQueue *
    DPIQueue ->
    IO CInt

-- | Returns a reference to a new queue which enqueues and dequeues messages from Advanced Queueing (AQ) with a JSON payload. 
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

-- | Returns a reference to a new set of message properties, used in enqueuing and dequeuing objects in a queue.
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

-- | Returns a reference to a new queue which may be used to enqueue and dequeue messages from Advanced Queuing (AQ) queues.
genQueue :: Connection -> String -> IO DPIQueue
genQueue (Connection fptr) queueName = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiQueuePtr -> do
      withCStringLen queueName $ \(queueNameC , fromIntegral -> queueNameLen) -> do
        throwOracleError =<< dpiConn_newQueue conn queueNameC queueNameLen nullPtr dpiQueuePtr
        peek dpiQueuePtr

-- | Returns a reference to a new queue which may be used to 
-- | enqueue and dequeue messages from Advanced Queuing (AQ) queues with Object as Payload type.
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

-- | Returns the number of attempts that have been made to dequeue a message.
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

-- | Returns the number of seconds the enqueued message will be delayed.
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

-- | Returns the payload associated with the message properties in bytes. 
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

-- | Returns the payload associated with the message properties in Object type.
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

-- | Returns the payload associated with the message properties, The payload must be a JSON object
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

-- | Sets the payload for the message as a series of bytes. 
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

-- | Sets the payload for the message as a object. 
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

-- | Sets the payload for the message as a JSON object. 
setMsgPropsPayLoadJSON :: DPIMsgProps -> DPIJson -> IO ()
setMsgPropsPayLoadJSON dpiMsgProps dpiJson =
  throwOracleError =<< dpiMsgProps_setPayloadJson dpiMsgProps dpiJson

foreign import ccall unsafe "dpiMsgProps_setPayloadJson"
    dpiMsgProps_setPayloadJson ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiJson * 
    DPIJson ->
    IO CInt

-- | Appends an element with the specified value to the collection.
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

-- | Returns the value of the element found at the specified index.
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

-- | Sets the value of the element found at the specified index.
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

-- | Returns a reference to a new JSON object.
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

-- | Helper function that inserts any type with ToJSON instance into DPIJson.
setValInJSON :: forall a. (ToJSON a) => DPIJson -> a -> IO DPIJson
setValInJSON dpiJson jsonData = do
  let res_ = BSLC.unpack $ encode jsonData
  setTextInJson dpiJson res_

-- | Helper function that takes value from DPIJson into any type with FromJSON Instance.
dpiJsonToVal :: FromJSON a => DPIJson -> IO a
dpiJsonToVal dpiJson = do
  jsonNodePtr <- dpiJson_getValue dpiJson
  jsonNode <- (peek jsonNodePtr)
  parseJson jsonNode

-- | Helper function that inserts JSON string into DPIJson.
setTextInJson :: DPIJson -> String -> IO DPIJson
setTextInJson dpiJson jsonVal = do
    withCStringLen jsonVal $ \ (jsonString, jsonStringLen) -> do
      throwOracleError =<< 
        dpiJson_setFromText dpiJson jsonString (fromIntegral jsonStringLen) 0
      return dpiJson

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

-- | Releases a reference to the JSON value. 
releaseDpiJson :: DPIJson -> IO ()
releaseDpiJson dpiJson = do
  throwOracleError =<< dpiJson_release dpiJson

foreign import ccall unsafe "dpiJson_release"
  dpiJson_release ::
    -- | dpiJson *
    DPIJson ->
    IO CInt
