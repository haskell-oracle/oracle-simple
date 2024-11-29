{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Avoid restricted function" -}

module Database.Oracle.Simple.Internal
  ( DPIContextCreateParams (..),
    DPIIntervalYM(..),
    DPIIntervalDS (..),
    DPICreateMode (..),
    DPIShardingKeyColumn (..),
    DPIPurity (..),
    DPIAuthMode (..),
    ConnectionCreateParams (..),
    DPIPoolGetMode (..),
    DPIAppContext (..),
    DPINativeType (..),
    DPIData (..),
    DPIBytes (..),
    DPIStmt (..),
    DPIModeExec (..),
    DPIConn (..),
    DPIContext (..),
    DPITimestamp (..),
    DPIOracleType (..),
    DPICommonCreateParams (..),
    DPIPoolCreateParams (..),
    AdditionalConnectionParams (..),
    DPIPool (..),
    WriteBuffer (..),
    ReadBuffer (..),
    Column (..),
    Only (..),
    Connection (..),
    ConnectionParams (..),
    OracleError (..),
    ErrorInfo (..),
    VersionInfo (..),
    renderErrorInfo,
    ping,
    fetch,
    close,
    connect,
    withConnection,
    withConnCreateParams,
    getClientVersion,
    getServerVersion,
    globalContext,
    withDefaultPoolCreateParams,
    defaultAdditionalConnectionParams,
    dpiExecute,
    getRowCount,
    getQueryValue,
    prepareStmt,
    bindValueByPos,
    freeWriteBuffer,
    mkDPIBytesUTF8,
    isHealthy,
    dpiTimeStampToUTCDPITimeStamp,
    throwOracleError,
    dpiData_getIsNull,
    dpiData_getDouble,
    dpiData_getFloat,
    dpiData_getInt64,
    dpiData_getUint64,
    dpiData_getBytes,
    dpiData_getBool,
    dpiData_getTimestamp,
    dpiConn_close_finalizer,
    dpiConn_release_finalizer,
  )
where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, (<=<))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Int (Int16, Int64, Int8)
import qualified Data.Time as Time
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, newCString, newCStringLen, peekCString, peekCStringLen, withCStringLen)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.ForeignPtr (ForeignPtr, addForeignPtrFinalizer, finalizeForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable.Generic (GStorable, Storable (..))
import GHC.Generics (Generic)
import GHC.TypeLits (Natural)
import System.IO.Unsafe (unsafePerformIO)

newtype DPIStmt = DPIStmt (Ptr DPIStmt)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIConn = DPIConn (Ptr DPIConn)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype Connection = Connection (ForeignPtr DPIConn)
  deriving (Show, Eq)

newtype DPIPool = DPIPool (Ptr DPIPool)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIContext = DPIContext (Ptr DPIContext)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIShardingKeyColumn = DPIShardingKeyColumn (Ptr DPIShardingKeyColumn)
  deriving (Show, Eq)
  deriving newtype (Storable)

data AdditionalConnectionParams = AdditionalConnectionParams
  { minSessions :: Natural
  , maxSessions :: Natural
  , sessionIncrement :: Natural
  , pingInterval :: Natural
  , pingTimeout :: Natural
  , homogeneous :: Natural
  , getMode :: DPIPoolGetMode
  , timeout :: Natural
  , waitTimeout :: Natural
  , maxLifetimeSession :: Natural
  , maxSessionsPerShard :: Natural
  }
  deriving (Eq, Ord, Show)

defaultAdditionalConnectionParams :: AdditionalConnectionParams
defaultAdditionalConnectionParams =
  AdditionalConnectionParams
    { minSessions = 1
    , maxSessions = 1
    , sessionIncrement = 0
    , pingInterval = 60
    , pingTimeout = 5000
    , homogeneous = 1
    , getMode = DPI_MODE_POOL_GET_NOWAIT
    , timeout = 0
    , waitTimeout = 0
    , maxLifetimeSession = 0
    , maxSessionsPerShard = 0
    }

data ConnectionParams = ConnectionParams
  { user :: String
  , pass :: String
  , connString :: String
  , additionalParams :: Maybe AdditionalConnectionParams
  }
  deriving (Eq, Ord, Show)

connectDPI ::
  ConnectionParams ->
  IO DPIConn
connectDPI params = do
  ctx <- readIORef globalContext
  alloca $ \connPtr -> do
    withCStringLen (user params) $ \(userCString, fromIntegral -> userLen) ->
      withCStringLen (pass params) $ \(passCString, fromIntegral -> passLen) ->
        withCStringLen (connString params) $ \(connCString, fromIntegral -> connLen) -> do
          throwOracleError
            =<< dpiConn_create ctx userCString userLen passCString passLen connCString connLen nullPtr nullPtr connPtr
          peek connPtr

{- | The order that the finalizers are declared in is very important
The close must be defined /last/ so it can run /first/
Per the docs, "The finalizer will run before all other finalizers for the same object which have already been registered."
-}
connect :: ConnectionParams -> IO Connection
connect params = do
  DPIConn connPtr <- connectDPI params
  fptr <- newForeignPtr_ connPtr
  addForeignPtrFinalizer dpiConn_release_finalizer fptr
  addForeignPtrFinalizer dpiConn_close_finalizer fptr
  pure (Connection fptr)

-- | Brackets a computation between opening and closing a connection.
withConnection :: ConnectionParams -> (Connection -> IO c) -> IO c
withConnection params = bracket (connect params) close

foreign import ccall unsafe "dpiConn_create"
  dpiConn_create ::
    -- | const dpiContext *context
    DPIContext ->
    -- | const char *userName
    CString ->
    -- | uint32_t userNameLength
    CUInt ->
    -- | const char *password
    CString ->
    -- | uint32_t passwordLength
    CUInt ->
    -- | const char *connectString
    CString ->
    -- | uint32_t conn length
    CUInt ->
    -- | const dpiCommonCreateParams *commonParams
    Ptr DPICommonCreateParams ->
    -- | const dpiConnCreateParams *createParams
    Ptr ConnectionCreateParams ->
    -- | dpi * conn
    Ptr DPIConn ->
    IO CInt

-- | typedef uint32_t dpiAuthMode;
data DPIAuthMode
  = DPI_MODE_AUTH_DEFAULT -- 0x00000000
  | DPI_MODE_AUTH_SYSDBA -- 0x00000002
  | DPI_MODE_AUTH_SYSOPER -- 0x00000004
  | DPI_MODE_AUTH_PRELIM -- 0x00000008
  | DPI_MODE_AUTH_SYSASM -- 0x00008000
  | DPI_MODE_AUTH_SYSBKP -- 0x00020000
  | DPI_MODE_AUTH_SYSDGD -- 0x00040000
  | DPI_MODE_AUTH_SYSKMT -- 0x00080000
  | DPI_MODE_AUTH_SYSRAC -- 0x00100000
  deriving (Show, Eq)

toDPIAuthMode :: DPIAuthMode -> CInt
toDPIAuthMode DPI_MODE_AUTH_DEFAULT = 0x00000000
toDPIAuthMode DPI_MODE_AUTH_SYSDBA = 0x00000002
toDPIAuthMode DPI_MODE_AUTH_SYSOPER = 0x00000004
toDPIAuthMode DPI_MODE_AUTH_PRELIM = 0x00000008
toDPIAuthMode DPI_MODE_AUTH_SYSASM = 0x00008000
toDPIAuthMode DPI_MODE_AUTH_SYSBKP = 0x00020000
toDPIAuthMode DPI_MODE_AUTH_SYSDGD = 0x00040000
toDPIAuthMode DPI_MODE_AUTH_SYSKMT = 0x00080000
toDPIAuthMode DPI_MODE_AUTH_SYSRAC = 0x00100000

fromDPIAuthMode :: CInt -> Maybe DPIAuthMode
fromDPIAuthMode 0x00000000 = Just DPI_MODE_AUTH_DEFAULT
fromDPIAuthMode 0x00000002 = Just DPI_MODE_AUTH_SYSDBA
fromDPIAuthMode 0x00000004 = Just DPI_MODE_AUTH_SYSOPER
fromDPIAuthMode 0x00000008 = Just DPI_MODE_AUTH_PRELIM
fromDPIAuthMode 0x00008000 = Just DPI_MODE_AUTH_SYSASM
fromDPIAuthMode 0x00020000 = Just DPI_MODE_AUTH_SYSBKP
fromDPIAuthMode 0x00040000 = Just DPI_MODE_AUTH_SYSDGD
fromDPIAuthMode 0x00080000 = Just DPI_MODE_AUTH_SYSKMT
fromDPIAuthMode 0x00100000 = Just DPI_MODE_AUTH_SYSRAC
fromDPIAuthMode _ = Nothing

instance Storable DPIAuthMode where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mbMode <- fromDPIAuthMode <$> peek (castPtr ptr)
    case mbMode of
      Nothing -> fail "DPIAuthMode.peek: Invalid create mode"
      Just mode -> pure mode
  poke ptr mode =
    poke (castPtr ptr) (toDPIAuthMode mode)

-- typedef uint32_t dpiPurity;
data DPIPurity
  = DPI_PURITY_DEFAULT
  | DPI_PURITY_NEW
  | DPI_PURITY_SELF
  deriving (Show, Eq, Ord, Enum, Generic)

instance Storable DPIPurity where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mbMode <- fromDPIPurity <$> peek (castPtr ptr)
    case mbMode of
      Nothing -> fail "DPIPurity.peek: Invalid create mode"
      Just mode -> pure mode
  poke ptr mode =
    poke (castPtr ptr) (toDPIPurity mode)

toDPIPurity :: DPIPurity -> CUInt
toDPIPurity = fromIntegral . fromEnum

data DPIModeConnClose
  = DPI_MODE_CONN_CLOSE_DEFAULT -- 0x0000
  | DPI_MODE_CONN_CLOSE_DROP -- 0x0001
  | DPI_MODE_CONN_CLOSE_RETAG -- 0x0002
  deriving (Show, Eq)

foreign import ccall "&finalize_connection_default"
  dpiConn_close_finalizer :: FunPtr (Ptr DPIConn -> IO ())

foreign import ccall "&dpiConn_release"
  dpiConn_release_finalizer :: FunPtr (Ptr DPIConn -> IO ())

-- | An explicit call to 'close' will invoke the finalizers before the GC does
close :: Connection -> IO ()
close (Connection conn) = finalizeForeignPtr conn

fromDPIPurity :: CUInt -> Maybe DPIPurity
fromDPIPurity 0 = Just DPI_PURITY_DEFAULT
fromDPIPurity 1 = Just DPI_PURITY_NEW
fromDPIPurity 2 = Just DPI_PURITY_SELF
fromDPIPurity _ = Nothing

foreign import ccall "dpiContext_initPoolCreateParams"
  dpiContext_initPoolCreateParams ::
    DPIContext ->
    Ptr DPIPoolCreateParams ->
    IO Int

withDefaultPoolCreateParams :: (Ptr DPIPoolCreateParams -> IO a) -> IO a
withDefaultPoolCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \poolCreateParamsPtr -> do
    status <- dpiContext_initPoolCreateParams ctx poolCreateParamsPtr
    unless (status == 0) $ do
      error $ "pool create params status wasn't 0" <> show status
    f poolCreateParamsPtr

data DPIPoolCreateParams = DPIPoolCreateParams
  { dpi_minSessions :: CUInt
  , dpi_maxSessions :: CUInt
  , dpi_sessionIncrement :: CUInt
  , dpi_pingInterval :: CInt
  , dpi_pingTimeout :: CInt
  , dpi_homogeneous :: CInt
  , dpi_externalAuth :: CInt
  , dpi_getMode :: DPIPoolGetMode
  , dpi_outPoolName :: CString
  , dpi_outPoolNameLength :: CUInt
  , dpi_timeout :: CUInt
  , dpi_waitTimeout :: CUInt
  , dpi_maxLifetimeSession :: CUInt
  , dpi_plsqlFixupCallback :: CString
  , dpi_plsqlFixupCallbackLength :: CUInt
  , dpi_maxSessionsPerShard :: CUInt
  , dpi_accessTokenCallback :: FunPtr ()
  , dpi_accessTokenCallbackContext :: Ptr ()
  }
  deriving (Show, Eq)

instance Storable DPIPoolCreateParams where
    sizeOf _ = (9 * sizeOf (undefined :: CUInt))
              + (4 * sizeOf (undefined :: CInt))
              + sizeOf (undefined :: DPIPoolGetMode)
              + sizeOf (undefined :: FunPtr ())
              + sizeOf (undefined :: Ptr ())
              + (2 * sizeOf (undefined :: CString))

    alignment _ = alignment (undefined :: CUInt)

    peek ptr = do
      let base = castPtr ptr
      DPIPoolCreateParams
        <$> peek base                           -- dpi_minSessions
        <*> peek (base `plusPtr`     sizeOf (undefined :: CUInt)) -- dpi_maxSessions
        <*> peek (base `plusPtr` (2 * sizeOf (undefined :: CUInt))) -- dpi_sessionIncrement
        <*> peek (base `plusPtr`( 3 * sizeOf (undefined :: CUInt))) -- dpi_pingInterval
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr`     sizeOf (undefined :: CInt)) -- dpi_pingTimeout
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` (2 * sizeOf (undefined :: CInt))) -- dpi_homogeneous
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` (3 * sizeOf (undefined :: CInt))) -- dpi_externalAuth
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))) -- dpi_getMode
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)) -- dpi_outPoolName
        <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr`     sizeOf (undefined :: CString)) -- dpi_outPoolNameLength
        <*> peek (base `plusPtr` (4 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr`     sizeOf (undefined :: CString)) -- dpi_timeout
        <*> peek (base `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr`     sizeOf (undefined :: CString)) -- dpi_waitTimeout
        <*> peek (base `plusPtr` (6 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr`     sizeOf (undefined :: CString)) -- dpi_maxLifetimeSession
        <*> peek (base `plusPtr` (7 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr`     sizeOf (undefined :: CString)) -- dpi_plsqlFixupCallback
        <*> peek (base `plusPtr` (7 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr` (2 * sizeOf (undefined :: CString))) -- dpi_plsqlFixupCallbackLength
        <*> peek (base `plusPtr` (8 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr` (2 * sizeOf (undefined :: CString))) -- dpi_maxSessionsPerShard
        <*> peek (base `plusPtr` (9 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr` (2 * sizeOf (undefined :: CString))) -- dpi_accessTokenCallback
        <*> peek (base `plusPtr` (10 * sizeOf (undefined :: CUInt))
                       `plusPtr` (4 * sizeOf (undefined :: CInt))
                       `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                       `plusPtr` (2 * sizeOf (undefined :: CString))) -- dpi_accessTokenCallbackContext

    poke ptr DPIPoolCreateParams{..} = do
      let base = castPtr ptr
      poke base dpi_minSessions
      poke (base `plusPtr`     sizeOf (undefined :: CUInt)) dpi_maxSessions
      poke (base `plusPtr` (2 * sizeOf (undefined :: CUInt))) dpi_sessionIncrement
      poke (base `plusPtr`( 3 * sizeOf (undefined :: CUInt))) dpi_pingInterval
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr`     sizeOf (undefined :: CInt)) dpi_pingTimeout
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr` (2 * sizeOf (undefined :: CInt))) dpi_homogeneous
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr` (3 * sizeOf (undefined :: CInt))) dpi_externalAuth
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))) dpi_getMode
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)) dpi_outPoolName
      poke (base `plusPtr` (3 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr`     sizeOf (undefined :: CString)) dpi_outPoolNameLength
      poke (base `plusPtr` (4 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr`     sizeOf (undefined :: CString)) dpi_timeout
      poke (base `plusPtr` (5 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr`     sizeOf (undefined :: CString)) dpi_waitTimeout
      poke (base `plusPtr` (6 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr`     sizeOf (undefined :: CString)) dpi_maxLifetimeSession
      poke (base `plusPtr` (7 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr`     sizeOf (undefined :: CString)) dpi_plsqlFixupCallback
      poke (base `plusPtr` (7 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr` (2 * sizeOf (undefined :: CString))) dpi_plsqlFixupCallbackLength
      poke (base `plusPtr` (8 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr` (2 * sizeOf (undefined :: CString))) dpi_maxSessionsPerShard
      poke (base `plusPtr` (9 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr` (2 * sizeOf (undefined :: CString))) dpi_accessTokenCallback
      poke (base `plusPtr` (10 * sizeOf (undefined :: CUInt))
                 `plusPtr` (4 * sizeOf (undefined :: CInt))
                 `plusPtr`     sizeOf (undefined :: DPIPoolGetMode)
                 `plusPtr` (2 * sizeOf (undefined :: CString))) dpi_accessTokenCallbackContext

data DPIPoolGetMode
  = DPI_MODE_POOL_GET_FORCEGET
  | DPI_MODE_POOL_GET_NOWAIT
  | DPI_MODE_POOL_GET_TIMEDWAIT
  | DPI_MODE_POOL_GET_WAIT
  deriving (Show, Eq, Ord, Enum, Generic)

toDPIPoolGetMode :: DPIPoolGetMode -> CUInt
toDPIPoolGetMode DPI_MODE_POOL_GET_FORCEGET = 0x0000
toDPIPoolGetMode DPI_MODE_POOL_GET_NOWAIT = 0x0001
toDPIPoolGetMode DPI_MODE_POOL_GET_TIMEDWAIT = 0x0002
toDPIPoolGetMode DPI_MODE_POOL_GET_WAIT = 0x0003

fromDPIPoolGetMode :: CUInt -> Maybe DPIPoolGetMode
fromDPIPoolGetMode 0 = Just DPI_MODE_POOL_GET_FORCEGET
fromDPIPoolGetMode 1 = Just DPI_MODE_POOL_GET_NOWAIT
fromDPIPoolGetMode 2 = Just DPI_MODE_POOL_GET_TIMEDWAIT
fromDPIPoolGetMode 3 = Just DPI_MODE_POOL_GET_WAIT
fromDPIPoolGetMode _ = Nothing

instance Storable DPIPoolGetMode where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mbPoolGetMode <- fromDPIPoolGetMode <$> peek (castPtr ptr)
    case mbPoolGetMode of
      Nothing -> fail "DPIPoolGetMode.peek: Invalid get pool mode"
      Just mode -> pure mode
  poke ptr mode =
    poke (castPtr ptr) (toDPIPoolGetMode mode)

data ConnectionCreateParams = ConnectionCreateParams
  { authMode :: DPIAuthMode
  , connectionClass :: CString
  , connectionClassLength :: CUInt
  , purity :: DPIPurity
  , newPassword :: CString
  , newPasswordLength :: CUInt
  , appContenxt :: DPIAppContext
  , numAppContext :: CUInt
  , externalAuth :: CInt
  , externalHandle :: Ptr ()
  , pool :: DPIPool
  , tag :: CString
  , tagLength :: CUInt
  , matchAnyTag :: CInt
  , outTag :: CString
  , outTagLength :: CUInt
  , outTagFound :: CInt
  , shardingKeyColumn :: DPIShardingKeyColumn
  , numShardingKeyColumns :: Word8
  , superShardingKeyColumns :: DPIShardingKeyColumn
  , numSuperShardingKeyColumns :: Word8
  , outNewSession :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPICommonCreateParams = DPICommonCreateParams
  { createMode :: DPICreateMode
  , encoding :: CString
  , nencoding :: CString
  , edition :: CString
  , editionLength :: CInt
  , driverName :: CString
  , driverNameLength :: CInt
  , sodaMetadataCache :: Int
  , stmtCacheSize :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- | typedef uint32_t dpiCreateMode;
data DPICreateMode
  = DPI_MODE_CREATE_DEFAULT --  0x00000000
  | DPI_MODE_CREATE_THREADED -- 0x00000001
  | DPI_MODE_CREATE_EVENTS --   0x00000004
  deriving (Show, Eq)

instance Storable DPICreateMode where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    mbMode <- fromDPICreateMode <$> peek (castPtr ptr)
    case mbMode of
      Nothing -> fail "DPICreateMode.peek: Invalid create mode"
      Just mode -> pure mode
  poke ptr mode =
    poke (castPtr ptr) (toDPICreateMode mode)

toDPICreateMode :: DPICreateMode -> CInt
toDPICreateMode DPI_MODE_CREATE_DEFAULT = 0x00000000
toDPICreateMode DPI_MODE_CREATE_THREADED = 0x00000001
toDPICreateMode DPI_MODE_CREATE_EVENTS = 0x00000004

fromDPICreateMode :: CInt -> Maybe DPICreateMode
fromDPICreateMode 0x00000000 = Just DPI_MODE_CREATE_DEFAULT
fromDPICreateMode 0x00000001 = Just DPI_MODE_CREATE_THREADED
fromDPICreateMode 0x00000004 = Just DPI_MODE_CREATE_EVENTS
fromDPICreateMode _ = Nothing

foreign import ccall unsafe "context_create"
  dpiContext_create ::
    -- | major version
    CInt ->
    -- | minor version
    CInt ->
    -- | context return
    Ptr DPIContext ->
    -- | error info struct
    Ptr ErrorInfo ->
    IO Int

globalContext :: IORef DPIContext
{-# NOINLINE globalContext #-}
globalContext = unsafePerformIO (newIORef =<< createContext)

foreign import ccall "getMajorVersion" getMajorVersion :: IO CInt
foreign import ccall "getMinorVersion" getMinorVersion :: IO CInt

createContext ::
  IO DPIContext
createContext = do
  alloca $ \contextPtr -> do
    alloca $ \errorInfoPtr -> do
      majorVersion <- getMajorVersion
      minorVersion <- getMinorVersion
      statusCode <-
        dpiContext_create
          majorVersion
          minorVersion
          contextPtr
          errorInfoPtr
      if statusCode == 0
        then peek contextPtr
        else (throwIO <=< toOracleError <=< peek) errorInfoPtr

renderErrorInfo :: ErrorInfo -> IO ()
renderErrorInfo ErrorInfo {errorInfoCode, errorInfoMessage} = do
  putStrLn $ "Error code: " <> show errorInfoCode
  unless (errorInfoMessage == nullPtr) $ do
    str <- peekCString errorInfoMessage
    putStrLn $ "Error msg: " <> str

data ErrorInfo = ErrorInfo
  { errorInfoCode :: CInt
  , errorInfoOffset16 :: Word16
  , errorInfoMessage :: CString
  , errorInfoMessageLength :: CUInt
  , errorInfoEncoding :: CString
  , errorInfoFnName :: CString
  , errorInfoAction :: CString
  , errorInfoSqlState :: CString
  , errorInfoIsRecoverable :: CInt
  , errorInfoIsWarning :: CInt
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (GStorable)

data OracleError = OracleError
  { oracleErrorFnName :: String
  -- ^ The public ODPI-C function name which was called in which the error took place.
  , oracleErrorAction :: String
  -- ^ The internal action that was being performed when the error took place.
  , oracleErrorMessage :: String
  -- ^ The error message as a byte string.
  , oracleErrorSqlState :: String
  -- ^ The SQLSTATE associated with the error.
  , oracleErrorCode :: Int
  , -- , oracleErrorOffset16 :: Word16
    -- , oracleErrorMessageLength :: Int
    oracleErrorIsRecoverable :: Bool
  -- ^ A boolean value indicating if the error is recoverable.
  , oracleErrorIsWarning :: Bool
  -- ^ A boolean value indicating if the error information is for a warning returned
  -- by Oracle that does not prevent the request operation from proceeding.
  }
  deriving (Show, Eq, Typeable)

toOracleError :: ErrorInfo -> IO OracleError
toOracleError ErrorInfo {..} = do
  oracleErrorFnName <- peekCString errorInfoFnName
  oracleErrorAction <- peekCString errorInfoAction
  oracleErrorMessage <- peekCStringLen (errorInfoMessage, fromIntegral errorInfoMessageLength)
  oracleErrorSqlState <- peekCString errorInfoSqlState
  let oracleErrorCode = fromIntegral errorInfoCode
  let oracleErrorIsRecoverable = intToBool $ fromIntegral errorInfoIsRecoverable
  let oracleErrorIsWarning = intToBool $ fromIntegral errorInfoIsWarning
  pure OracleError {..}
  where
    intToBool :: Int -> Bool
    intToBool 0 = False
    intToBool 1 = True
    intToBool i = error $ "boolean encoded as integer not 0 or 1: " <> show i

throwOracleError :: CInt -> IO ()
throwOracleError returnCode = do
  unless (returnCode == 0) $
    (throwIO =<< toOracleError =<< getErrorInfo)

instance Exception OracleError

data VersionInfo = VersionInfo
  { versionNum :: CInt
  , releaseNum :: CInt
  , updateNum :: CInt
  , portReleaseNum :: CInt
  , portUpdateNum :: CInt
  , fullVersionNum :: CUInt
  }
  deriving (Show, Eq)

instance Storable VersionInfo where
    sizeOf _ = sizeOf (undefined :: CInt) * 6
    alignment _ = alignment (undefined :: CInt)
    peek p = do
        let basePtr = castPtr p
        versionNum <- peekByteOff basePtr 0
        releaseNum <- peekByteOff basePtr 4
        updateNum <- peekByteOff basePtr 8
        portReleaseNum <- peekByteOff basePtr 12
        portUpdateNum <- peekByteOff basePtr 16
        fullVersionNum <- peekByteOff basePtr 20
        return VersionInfo{..}
    poke p VersionInfo{..} = do
        let basePtr = castPtr p
        pokeByteOff basePtr 0 versionNum
        pokeByteOff basePtr 4 releaseNum
        pokeByteOff basePtr 8 updateNum
        pokeByteOff basePtr 12 portReleaseNum
        pokeByteOff basePtr 16 portUpdateNum
        pokeByteOff basePtr 20 fullVersionNum

getClientVersion ::
  IO VersionInfo
getClientVersion = do
  ctx <- readIORef globalContext
  alloca $ \versionPtr -> do
    statusCode <- dpiContext_getClientVersion ctx versionPtr
    if statusCode == 0
      then peek versionPtr
      else error ("getClientVersion: " <> show statusCode)

foreign import ccall "dpiContext_getClientVersion"
  dpiContext_getClientVersion ::
    DPIContext ->
    Ptr VersionInfo ->
    IO Int

foreign import ccall "dpiConn_getServerVersion"
  dpiContext_getServerVersion ::
    Ptr DPIConn ->
    Ptr CString ->
    CInt ->
    Ptr VersionInfo ->
    IO Int

getServerVersion ::
  Connection ->
  VersionInfo ->
  IO String
getServerVersion (Connection fptr) versionInfo = do
  withForeignPtr fptr $ \conn ->
    alloca $ \releaseStringPtr -> do
      alloca $ \versionInfoPtr -> do
        poke versionInfoPtr versionInfo
        status <-
          dpiContext_getServerVersion
            conn
            releaseStringPtr
            (fromIntegral (10 :: Int))
            versionInfoPtr
        if status == 0
          then (peekCString <=< peek) releaseStringPtr
          else error $ show status <> " oh no!"

foreign import ccall "dpiContext_initConnCreateParams"
  dpiContext_initConnCreateParams ::
    DPIContext ->
    Ptr ConnectionCreateParams ->
    IO Int

withConnCreateParams ::
  (ConnectionCreateParams -> IO a) ->
  IO a
withConnCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \connCreateParamsPtr -> do
    status <- dpiContext_initConnCreateParams ctx connCreateParamsPtr
    unless (status == 0) $ do
      error $ "conn create params isn't 0" <> show status
    f =<< peek connCreateParamsPtr

data DPIBytes = DPIBytes
  { dpiBytesPtr :: CString
  , dpiBytesLength :: CUInt
  , dpiBytesEncoding :: CString
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

mkDPIBytesUTF8 :: String -> IO DPIBytes
mkDPIBytesUTF8 str = do
  (dpiBytesPtr, fromIntegral -> dpiBytesLength) <- newCStringLen str
  dpiBytesEncoding <- newCString "UTF-8"
  pure $ DPIBytes {..}

data DPIIntervalDS = DPIIntervalDS
  { days :: CInt
  , hours :: CInt
  , minutes :: CInt
  , seconds :: CInt
  , fseconds :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPIIntervalYM = DPIIntervalYM
  { years :: CInt
  , months :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPITimestamp = DPITimestamp
  { year :: Int16
  , month :: Word8
  , day :: Word8
  , hour :: Word8
  , minute :: Word8
  , second :: Word8
  , fsecond :: CUInt
  , tzHourOffset :: Int8
  , tzMinuteOffset :: Int8
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

{- | Converts a DPITimestamp into the UTCTime zone by applying the offsets
to the year, month, day, hour, minutes and seconds
-}
dpiTimeStampToUTCDPITimeStamp :: DPITimestamp -> DPITimestamp
dpiTimeStampToUTCDPITimeStamp dpi@DPITimestamp {..} =
  let offsetInMinutes, currentMinutes :: Int
      offsetInMinutes = negate $ (fromIntegral tzHourOffset * 60) + fromIntegral tzMinuteOffset
      currentMinutes = (fromIntegral hour * 60) + fromIntegral minute

      (hours, minutes) = ((currentMinutes + offsetInMinutes) `mod` 1440) `quotRem` 60
      gregorianDay = Time.fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
      updatedDay
        | currentMinutes + offsetInMinutes > 1440 = Time.addDays 1 gregorianDay
        | currentMinutes + offsetInMinutes < 0 = Time.addDays (-1) gregorianDay
        | otherwise = gregorianDay
      (year', month', day') = Time.toGregorian updatedDay
   in dpi
        { tzHourOffset = 0
        , tzMinuteOffset = 0
        , year = fromIntegral year'
        , month = fromIntegral month'
        , day = fromIntegral day'
        , hour = fromIntegral hours
        , minute = fromIntegral minutes
        }

data DPIAppContext = DPIAppContext
  { namespaceName :: CString
  , namespaceNameLength :: CUInt
  , name :: CString
  , nameLength :: CUInt
  , value :: CString
  , valueLength :: CUInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

data DPIContextCreateParams = DPIContextCreateParams
  { defaultDriverName :: CString
  , defaultEncoding :: CString
  , loadErrorUrl :: CString
  , oracleClientLibDir :: CString
  , oracleClientConfigDir :: CString
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

foreign import ccall "dpiContext_getError"
  dpiContext_getError :: DPIContext -> Ptr ErrorInfo -> IO ()

getErrorInfo :: IO ErrorInfo
getErrorInfo = do
  ctx <- readIORef globalContext
  alloca $ \errorInfoPtr -> do
    dpiContext_getError ctx errorInfoPtr
    peek errorInfoPtr

foreign import ccall "dpiConn_prepareStmt"
  dpiConn_prepareStmt ::
    Ptr DPIConn ->
    CInt ->
    CString ->
    CUInt ->
    CString ->
    CUInt ->
    Ptr DPIStmt ->
    IO CInt

prepareStmt ::
  Connection ->
  -- | sql
  String ->
  IO DPIStmt
prepareStmt (Connection fptr) sql = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \stmtPtr -> do
      withCStringLen sql $ \(sqlCStr, fromIntegral -> sqlCStrLen) -> do
        status <- dpiConn_prepareStmt conn 0 sqlCStr sqlCStrLen nullPtr 0 stmtPtr
        throwOracleError status
        peek stmtPtr

data DPIModeExec
  = DPI_MODE_EXEC_DEFAULT -- 0x00000000
  | DPI_MODE_EXEC_DESCRIBE_ONLY -- 0x00000010
  | DPI_MODE_EXEC_COMMIT_ON_SUCCESS -- 0x00000020
  | DPI_MODE_EXEC_BATCH_ERRORS -- 0x00000080
  | DPI_MODE_EXEC_PARSE_ONLY -- 0x00000100
  | DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS -- 0x00100000
  deriving (Show, Eq, Ord)

toDPIModeExec :: DPIModeExec -> CUInt
toDPIModeExec DPI_MODE_EXEC_DEFAULT = 0x00000000
toDPIModeExec DPI_MODE_EXEC_DESCRIBE_ONLY = 0x00000010
toDPIModeExec DPI_MODE_EXEC_COMMIT_ON_SUCCESS = 0x00000020
toDPIModeExec DPI_MODE_EXEC_BATCH_ERRORS = 0x00000080
toDPIModeExec DPI_MODE_EXEC_PARSE_ONLY = 0x00000100
toDPIModeExec DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS = 0x00100000

foreign import ccall "dpiStmt_execute"
  dpiStmt_execute ::
    DPIStmt ->
    CUInt ->
    Ptr CUInt ->
    IO CInt

-- | Execute a statement.
dpiExecute ::
  -- | Statement to be executed
  DPIStmt ->
  -- | Execution mode
  DPIModeExec ->
  -- | query columns
  IO CUInt
dpiExecute stmt mode =
  alloca $ \rowsPtr -> do
    throwOracleError =<< dpiStmt_execute stmt (toDPIModeExec mode) rowsPtr
    peek rowsPtr

foreign import ccall "dpiStmt_fetch"
  dpiStmt_fetch ::
    DPIStmt ->
    Ptr CInt ->
    Ptr CUInt ->
    IO CInt

-- | Fetch a single row from the buffers defined for the query.
fetch ::
  -- | Statement from which row is to be fetched
  DPIStmt ->
  IO CInt
fetch stmt =
  alloca $ \bufferRowIdxPtr ->
    alloca $ \foundPtr -> do
      throwOracleError =<< dpiStmt_fetch stmt foundPtr bufferRowIdxPtr
      peek foundPtr

foreign import ccall "dpiStmt_getQueryValue"
  dpiStmt_getQueryValue ::
    DPIStmt ->
    CUInt ->
    Ptr CUInt ->
    Ptr (Ptr (DPIData ReadBuffer)) ->
    IO CInt

-- | Return the value of the column at the given position for the currently fetched row.
getQueryValue ::
  -- | Statement from which column value is to be retrieved
  DPIStmt ->
  -- | Column position
  CUInt ->
  IO (DPINativeType, Ptr (DPIData ReadBuffer))
getQueryValue stmt pos = do
  alloca $ \(buffer :: Ptr (Ptr (DPIData ReadBuffer))) -> do
    alloca $ \(typPtr :: Ptr CUInt) -> do
      throwOracleError =<< dpiStmt_getQueryValue stmt pos typPtr buffer
      mbNativeType <- uintToDPINativeType <$> peek typPtr
      case mbNativeType of
        Nothing ->
          error "getQueryValue: Invalid type returned"
        Just nativeType -> do
          dataBuffer <- peek buffer
          pure (nativeType, dataBuffer)

data DPINativeType
  = DPI_NATIVE_TYPE_INT64
  | DPI_NATIVE_TYPE_UINT64
  | DPI_NATIVE_TYPE_FLOAT
  | DPI_NATIVE_TYPE_DOUBLE
  | DPI_NATIVE_TYPE_BYTES
  | DPI_NATIVE_TYPE_TIMESTAMP
  | DPI_NATIVE_TYPE_INTERVAL_DS
  | DPI_NATIVE_TYPE_INTERVAL_YM
  | DPI_NATIVE_TYPE_LOB
  | DPI_NATIVE_TYPE_OBJECT
  | DPI_NATIVE_TYPE_STMT
  | DPI_NATIVE_TYPE_BOOLEAN
  | DPI_NATIVE_TYPE_ROWID
  | DPI_NATIVE_TYPE_JSON
  | DPI_NATIVE_TYPE_JSON_OBJECT
  | DPI_NATIVE_TYPE_JSON_ARRAY
  | DPI_NATIVE_TYPE_NULL
  deriving (Show, Eq)

instance Storable DPINativeType where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    intVal <- peek (castPtr ptr)
    case uintToDPINativeType intVal of
      Nothing -> fail "DPINativeType.peek: Invalid value"
      Just val -> pure val
  poke ptr val = poke (castPtr ptr) (dpiNativeTypeToUInt val)

dpiNativeTypeToUInt :: DPINativeType -> CUInt
dpiNativeTypeToUInt DPI_NATIVE_TYPE_INT64 = 3000
dpiNativeTypeToUInt DPI_NATIVE_TYPE_UINT64 = 3001
dpiNativeTypeToUInt DPI_NATIVE_TYPE_FLOAT = 3002
dpiNativeTypeToUInt DPI_NATIVE_TYPE_DOUBLE = 3003
dpiNativeTypeToUInt DPI_NATIVE_TYPE_BYTES = 3004
dpiNativeTypeToUInt DPI_NATIVE_TYPE_TIMESTAMP = 3005
dpiNativeTypeToUInt DPI_NATIVE_TYPE_INTERVAL_DS = 3006
dpiNativeTypeToUInt DPI_NATIVE_TYPE_INTERVAL_YM = 3007
dpiNativeTypeToUInt DPI_NATIVE_TYPE_LOB = 3008
dpiNativeTypeToUInt DPI_NATIVE_TYPE_OBJECT = 3009
dpiNativeTypeToUInt DPI_NATIVE_TYPE_STMT = 3010
dpiNativeTypeToUInt DPI_NATIVE_TYPE_BOOLEAN = 3011
dpiNativeTypeToUInt DPI_NATIVE_TYPE_ROWID = 3012
dpiNativeTypeToUInt DPI_NATIVE_TYPE_JSON = 3013
dpiNativeTypeToUInt DPI_NATIVE_TYPE_JSON_OBJECT = 3014
dpiNativeTypeToUInt DPI_NATIVE_TYPE_JSON_ARRAY = 3015
dpiNativeTypeToUInt DPI_NATIVE_TYPE_NULL = 3016

uintToDPINativeType :: CUInt -> Maybe DPINativeType
uintToDPINativeType 3000 = Just DPI_NATIVE_TYPE_INT64
uintToDPINativeType 3001 = Just DPI_NATIVE_TYPE_UINT64
uintToDPINativeType 3002 = Just DPI_NATIVE_TYPE_FLOAT
uintToDPINativeType 3003 = Just DPI_NATIVE_TYPE_DOUBLE
uintToDPINativeType 3004 = Just DPI_NATIVE_TYPE_BYTES
uintToDPINativeType 3005 = Just DPI_NATIVE_TYPE_TIMESTAMP
uintToDPINativeType 3006 = Just DPI_NATIVE_TYPE_INTERVAL_DS
uintToDPINativeType 3007 = Just DPI_NATIVE_TYPE_INTERVAL_YM
uintToDPINativeType 3008 = Just DPI_NATIVE_TYPE_LOB
uintToDPINativeType 3009 = Just DPI_NATIVE_TYPE_OBJECT
uintToDPINativeType 3010 = Just DPI_NATIVE_TYPE_STMT
uintToDPINativeType 3011 = Just DPI_NATIVE_TYPE_BOOLEAN
uintToDPINativeType 3012 = Just DPI_NATIVE_TYPE_ROWID
uintToDPINativeType 3013 = Just DPI_NATIVE_TYPE_JSON
uintToDPINativeType 3014 = Just DPI_NATIVE_TYPE_JSON_OBJECT
uintToDPINativeType 3015 = Just DPI_NATIVE_TYPE_JSON_ARRAY
uintToDPINativeType 3016 = Just DPI_NATIVE_TYPE_NULL
uintToDPINativeType _ = Nothing

{- | Oracle data types.
Includes types used for columns in tables as well as types exclusive to PL/SQL.
Each type maps to a DPI native type to read/write values via ODPI functions.
-}
data DPIOracleType
  = DPI_ORACLE_TYPE_NONE
  | DPI_ORACLE_TYPE_VARCHAR
  | DPI_ORACLE_TYPE_NVARCHAR
  | DPI_ORACLE_TYPE_CHAR
  | DPI_ORACLE_TYPE_NCHAR
  | DPI_ORACLE_TYPE_ROWID
  | DPI_ORACLE_TYPE_RAW
  | DPI_ORACLE_TYPE_NATIVE_FLOAT
  | DPI_ORACLE_TYPE_NATIVE_DOUBLE
  | DPI_ORACLE_TYPE_NATIVE_INT
  | DPI_ORACLE_TYPE_NUMBER
  | DPI_ORACLE_TYPE_DATE
  | DPI_ORACLE_TYPE_TIMESTAMP
  | DPI_ORACLE_TYPE_TIMESTAMP_TZ
  | DPI_ORACLE_TYPE_TIMESTAMP_LTZ
  | DPI_ORACLE_TYPE_INTERVAL_DS
  | DPI_ORACLE_TYPE_INTERVAL_YM
  | DPI_ORACLE_TYPE_CLOB
  | DPI_ORACLE_TYPE_NCLOB
  | DPI_ORACLE_TYPE_BLOB
  | DPI_ORACLE_TYPE_BFILE
  | DPI_ORACLE_TYPE_STMT
  | DPI_ORACLE_TYPE_BOOLEAN
  | DPI_ORACLE_TYPE_OBJECT
  | DPI_ORACLE_TYPE_LONG_VARCHAR
  | DPI_ORACLE_TYPE_LONG_RAW
  | DPI_ORACLE_TYPE_NATIVE_UINT
  | DPI_ORACLE_TYPE_JSON
  | DPI_ORACLE_TYPE_JSON_OBJECT
  | DPI_ORACLE_TYPE_JSON_ARRAY
  | DPI_ORACLE_TYPE_UROWID
  | DPI_ORACLE_TYPE_LONG_NVARCHAR
  | DPI_ORACLE_TYPE_MAX

instance Storable DPIOracleType where
  sizeOf _ = sizeOf (undefined :: CUInt)
  alignment _ = alignment (undefined :: CUInt)
  peek ptr = do
    intVal <- peek (castPtr ptr)
    case uintToDPIOracleType intVal of
      Nothing -> fail "DPIOracleType.peek: Invalid value"
      Just val -> pure val
  poke ptr val = poke (castPtr ptr) (dpiOracleTypeToUInt val)

dpiOracleTypeToUInt :: DPIOracleType -> CUInt
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NONE = 2000
dpiOracleTypeToUInt DPI_ORACLE_TYPE_VARCHAR = 2001
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NVARCHAR = 2002
dpiOracleTypeToUInt DPI_ORACLE_TYPE_CHAR = 2003
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NCHAR = 2004
dpiOracleTypeToUInt DPI_ORACLE_TYPE_ROWID = 2005
dpiOracleTypeToUInt DPI_ORACLE_TYPE_RAW = 2006
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NATIVE_FLOAT = 2007
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NATIVE_DOUBLE = 2008
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NATIVE_INT = 2009
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NUMBER = 2010
dpiOracleTypeToUInt DPI_ORACLE_TYPE_DATE = 2011
dpiOracleTypeToUInt DPI_ORACLE_TYPE_TIMESTAMP = 2012
dpiOracleTypeToUInt DPI_ORACLE_TYPE_TIMESTAMP_TZ = 2013
dpiOracleTypeToUInt DPI_ORACLE_TYPE_TIMESTAMP_LTZ = 2014
dpiOracleTypeToUInt DPI_ORACLE_TYPE_INTERVAL_DS = 2015
dpiOracleTypeToUInt DPI_ORACLE_TYPE_INTERVAL_YM = 2016
dpiOracleTypeToUInt DPI_ORACLE_TYPE_CLOB = 2017
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NCLOB = 2018
dpiOracleTypeToUInt DPI_ORACLE_TYPE_BLOB = 2019
dpiOracleTypeToUInt DPI_ORACLE_TYPE_BFILE = 2020
dpiOracleTypeToUInt DPI_ORACLE_TYPE_STMT = 2021
dpiOracleTypeToUInt DPI_ORACLE_TYPE_BOOLEAN = 2022
dpiOracleTypeToUInt DPI_ORACLE_TYPE_OBJECT = 2023
dpiOracleTypeToUInt DPI_ORACLE_TYPE_LONG_VARCHAR = 2024
dpiOracleTypeToUInt DPI_ORACLE_TYPE_LONG_RAW = 2025
dpiOracleTypeToUInt DPI_ORACLE_TYPE_NATIVE_UINT = 2026
dpiOracleTypeToUInt DPI_ORACLE_TYPE_JSON = 2027
dpiOracleTypeToUInt DPI_ORACLE_TYPE_JSON_OBJECT = 2028
dpiOracleTypeToUInt DPI_ORACLE_TYPE_JSON_ARRAY = 2029
dpiOracleTypeToUInt DPI_ORACLE_TYPE_UROWID = 2030
dpiOracleTypeToUInt DPI_ORACLE_TYPE_LONG_NVARCHAR = 2031
dpiOracleTypeToUInt DPI_ORACLE_TYPE_MAX = 2032

uintToDPIOracleType :: CUInt -> Maybe DPIOracleType
uintToDPIOracleType 2000 = Just DPI_ORACLE_TYPE_NONE
uintToDPIOracleType 2001 = Just DPI_ORACLE_TYPE_VARCHAR
uintToDPIOracleType 2002 = Just DPI_ORACLE_TYPE_NVARCHAR
uintToDPIOracleType 2003 = Just DPI_ORACLE_TYPE_CHAR
uintToDPIOracleType 2004 = Just DPI_ORACLE_TYPE_NCHAR
uintToDPIOracleType 2005 = Just DPI_ORACLE_TYPE_ROWID
uintToDPIOracleType 2006 = Just DPI_ORACLE_TYPE_RAW
uintToDPIOracleType 2007 = Just DPI_ORACLE_TYPE_NATIVE_FLOAT
uintToDPIOracleType 2008 = Just DPI_ORACLE_TYPE_NATIVE_DOUBLE
uintToDPIOracleType 2009 = Just DPI_ORACLE_TYPE_NATIVE_INT
uintToDPIOracleType 2010 = Just DPI_ORACLE_TYPE_NUMBER
uintToDPIOracleType 2011 = Just DPI_ORACLE_TYPE_DATE
uintToDPIOracleType 2012 = Just DPI_ORACLE_TYPE_TIMESTAMP
uintToDPIOracleType 2013 = Just DPI_ORACLE_TYPE_TIMESTAMP_TZ
uintToDPIOracleType 2014 = Just DPI_ORACLE_TYPE_TIMESTAMP_LTZ
uintToDPIOracleType 2015 = Just DPI_ORACLE_TYPE_INTERVAL_DS
uintToDPIOracleType 2016 = Just DPI_ORACLE_TYPE_INTERVAL_YM
uintToDPIOracleType 2017 = Just DPI_ORACLE_TYPE_CLOB
uintToDPIOracleType 2018 = Just DPI_ORACLE_TYPE_NCLOB
uintToDPIOracleType 2019 = Just DPI_ORACLE_TYPE_BLOB
uintToDPIOracleType 2020 = Just DPI_ORACLE_TYPE_BFILE
uintToDPIOracleType 2021 = Just DPI_ORACLE_TYPE_STMT
uintToDPIOracleType 2022 = Just DPI_ORACLE_TYPE_BOOLEAN
uintToDPIOracleType 2023 = Just DPI_ORACLE_TYPE_OBJECT
uintToDPIOracleType 2024 = Just DPI_ORACLE_TYPE_LONG_VARCHAR
uintToDPIOracleType 2025 = Just DPI_ORACLE_TYPE_LONG_RAW
uintToDPIOracleType 2026 = Just DPI_ORACLE_TYPE_NATIVE_UINT
uintToDPIOracleType 2027 = Just DPI_ORACLE_TYPE_JSON
uintToDPIOracleType 2028 = Just DPI_ORACLE_TYPE_JSON_OBJECT
uintToDPIOracleType 2029 = Just DPI_ORACLE_TYPE_JSON_ARRAY
uintToDPIOracleType 2030 = Just DPI_ORACLE_TYPE_UROWID
uintToDPIOracleType 2031 = Just DPI_ORACLE_TYPE_LONG_NVARCHAR
uintToDPIOracleType 2032 = Just DPI_ORACLE_TYPE_MAX
uintToDPIOracleType _ = Nothing

-- | Used to write values to or read values from a column.
data DPIData a = DPIData
  { dataIsNull :: CInt
  -- ^ If reading, a null value was read. If writing, writes a null value.
  , dataValue :: a
  -- ^ The value that was read/will be written, of type 'ReadBuffer' or 'WriteBuffer'.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

{- | An opaque pointer type for the @dpiDataBuffer@ union that we read from.
We cannot write to this in a way that ODPIC could use.
For poking purposes, use 'WriteBuffer'.
-}
newtype ReadBuffer = ReadBuffer (Ptr ReadBuffer)
  deriving (Show, Eq)
  deriving newtype (Storable)

{- | @dpiDataBuffer@ union that we can write to.
We cannot read from this without a hint as to what type of data it contains.
-}
data WriteBuffer
  = AsInt64 Int64
  | AsUInt64 Word64
  | AsDouble Double
  | AsString CString
  | AsBytes DPIBytes
  | AsTimestamp DPITimestamp
  | AsBoolean Int
  | AsNull
  deriving (Show, Eq, Generic)

instance Storable WriteBuffer where
  sizeOf _ = sizeOf (undefined :: DPITimestamp)

  alignment _ = 8

  peek = error "WriteBuffer: peek not supported!"

  poke ptr (AsInt64 intVal) = poke (castPtr ptr) intVal
  poke ptr (AsUInt64 word64Val) = poke (castPtr ptr) word64Val
  poke ptr (AsDouble doubleVal) = poke (castPtr ptr) doubleVal
  poke ptr (AsString cStringVal) = poke (castPtr ptr) cStringVal
  poke ptr (AsBytes dpiBytesVal) = poke (castPtr ptr) dpiBytesVal
  poke ptr (AsTimestamp dpiTimeStampVal) = poke (castPtr ptr) dpiTimeStampVal
  poke ptr (AsBoolean cbool) = poke (castPtr ptr) cbool
  poke ptr AsNull = poke (castPtr ptr) nullPtr

{- | Free all pointers in the WriteBuffer.
Call only after the contents of the buffer (specifically, any pointers) are no longer needed.
-}
freeWriteBuffer :: WriteBuffer -> IO ()
freeWriteBuffer (AsString cString) = free cString
freeWriteBuffer (AsBytes DPIBytes {..}) = free dpiBytesPtr >> free dpiBytesEncoding
freeWriteBuffer _ = pure ()

foreign import ccall "dpiData_getDouble"
  dpiData_getDouble :: Ptr (DPIData ReadBuffer) -> IO Double

foreign import ccall "dpiData_getFloat"
  dpiData_getFloat :: Ptr (DPIData ReadBuffer) -> IO Float

foreign import ccall "dpiData_getBytes"
  dpiData_getBytes :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPIBytes)

foreign import ccall "dpiData_getTimestamp"
  dpiData_getTimestamp :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPITimestamp)

foreign import ccall "dpiData_getInt64"
  dpiData_getInt64 :: Ptr (DPIData ReadBuffer) -> IO Int64

foreign import ccall "dpiData_getUint64"
  dpiData_getUint64 :: Ptr (DPIData ReadBuffer) -> IO Word64

foreign import ccall "dpiData_getBool"
  dpiData_getBool :: Ptr (DPIData ReadBuffer) -> IO Int

foreign import ccall "dpiData_getIsNull"
  dpiData_getIsNull :: Ptr (DPIData ReadBuffer) -> IO Int

foreign import ccall "dpiStmt_bindValueByPos"
  dpiStmt_bindValueByPos ::
    -- | dpiStmt *stmt
    DPIStmt ->
    -- | uint32_t pos
    CUInt ->
    -- | dpiNativeTypeNum nativeTypeNum
    CUInt ->
    -- | dpiData *data
    Ptr (DPIData WriteBuffer) ->
    -- | int
    IO CInt

bindValueByPos :: DPIStmt -> Column -> DPINativeType -> DPIData WriteBuffer -> IO ()
bindValueByPos stmt col nativeType val = do
  alloca $ \dpiData' -> do
    poke dpiData' val
    throwOracleError
      =<< dpiStmt_bindValueByPos stmt (fromIntegral $ getColumn col) (dpiNativeTypeToUInt nativeType) dpiData'
    pure ()

foreign import ccall "dpiStmt_getRowCount"
  dpiStmt_getRowCount ::
    DPIStmt ->
    Ptr Word64 ->
    IO CInt

getRowCount :: DPIStmt -> IO Word64
getRowCount stmt = do
  alloca $ \rowCount -> do
    throwOracleError =<< dpiStmt_getRowCount stmt rowCount
    peek rowCount

-- | Column position, starting with 1 for the first column.
newtype Column = Column {getColumn :: Word32}
  deriving newtype (Num, Show)

foreign import ccall "dpiConn_ping"
  dpiConn_ping ::
    Ptr DPIConn ->
    IO CInt

-- | Ping the connection to see if it is still alive
ping :: Connection -> IO Bool
ping (Connection fptr) =
  withForeignPtr fptr $ fmap (== 0) . dpiConn_ping

-- | DPI_EXPORT int dpiConn_getIsHealthy(dpiConn *conn, int *isHealthy);
foreign import ccall unsafe "dpiConn_getIsHealthy"
  dpiConn_getIsHealthy ::
    Ptr DPIConn ->
    Ptr CInt ->
    IO CInt

-- | A pointer to an integer defining whether the connection is healthy (1) or not (0), which will be populated upon successful completion of this function.
isHealthy :: Connection -> IO Bool
isHealthy (Connection fptr) =
  withForeignPtr fptr $ \conn -> do
    alloca $ \healthPtr -> do
      throwOracleError =<< dpiConn_getIsHealthy conn healthPtr
      (== 1) <$> peek healthPtr

{- | The 1-tuple type or single-value "collection".
Structurally equivalent to 'Data.Functor.Identity.Identity'.
-}
newtype Only a = Only {fromOnly :: a}
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving newtype (Enum)
