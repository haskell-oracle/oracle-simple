{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Queue (
    DPIQueue (..)
  , DPIMsgProps (..)
  , DPIDeqOptions (..)
  , DPIEnqOptions (..)
  , DPIObjectType (..)
  , ObjectType (..)
  , deqMany
  , deqOne
  , enqMany
  , enqOne 
  , getDeqOptions
  , getEnqOptions
  , queueRelease 
  , genJSONQueue
  , genMsgProps
  , genQueue 
  , getMsgPropsNumOfAttempts 
  , getMsgPropsDelay
  , getMsgPropsPayLoadBytes
  , getMsgPropsPayLoadJson 
  , setMsgPropsPayLoadBytes 
  , setMsgPropsPayLoadJSON 
) where

import Foreign (alloca, withArray, withForeignPtr, nullPtr)
import Foreign.Storable.Generic (Storable (..))
import Foreign.C.Types (CInt (..), CUInt (..)) 
import Foreign.Ptr (Ptr)
import Foreign.C.String
import Database.Oracle.Simple.Internal
import qualified Data.ByteString.Char8 as BSC

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

newtype DPIObjectType = DPIObjectType (Ptr DPIObjectType)
  deriving (Show, Eq)
  deriving newtype (Storable)

data ObjectType = JSON | Raw
    deriving (Show, Eq)

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

genJSONQueue :: Connection -> String -> IO DPIQueue
genJSONQueue (Connection fptr) queueName = do
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

-- For now, Passing objectType will not work :( use setPayLoadType.
genQueue :: Connection -> String -> IO DPIQueue
genQueue (Connection fptr) queueName = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiQueuePtr -> do
      withCStringLen queueName $ \(queueNameC , fromIntegral -> queueNameLen) -> do
        throwOracleError =<< dpiConn_newQueue conn queueNameC queueNameLen nullPtr dpiQueuePtr
        -- TODO: Accomodate ObjectType
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
    Ptr () ->
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
      
foreign import ccall unsafe "dpiMsgProps_getPayload"
  dpiMsgProps_getPayload ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiObject **
    Ptr  DPIObjectType ->
    -- | const char ** value
    Ptr CString ->
    -- | valueLength
    Ptr CUInt ->
    IO CInt

getMsgPropsPayLoadJson :: DPIMsgProps -> IO DPIJson
getMsgPropsPayLoadJson dpiMsgProps = do
  alloca $ \dpiJsonPtr -> do
    throwOracleError =<< dpiMsgProps_getPayloadJson dpiMsgProps dpiJsonPtr
    peek dpiJsonPtr

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

setMsgPropsPayLoadJSON :: DPIMsgProps -> DPIJson -> IO ()
setMsgPropsPayLoadJSON dpiMsgProps payLoadJson = do
  throwOracleError =<< dpiMsgProps_setPayloadJson dpiMsgProps payLoadJson

foreign import ccall unsafe "dpiMsgProps_setPayloadJson"
    dpiMsgProps_setPayloadJson ::
    -- | dpiMsgProps *
    DPIMsgProps ->
    -- | dpiJson * 
    DPIJson ->
    IO CInt
