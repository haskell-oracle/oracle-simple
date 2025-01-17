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
    DPILob (..),
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
    mkStringFromDPIBytesUTF8,
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
    dpiNativeTypeToUInt,
    dpiOracleTypeToUInt,
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
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable.Generic (GStorable, Storable (..))
import GHC.Generics (Generic)
import GHC.TypeLits (Natural)
import System.IO.Unsafe (unsafePerformIO)

-- | A newtype wrapper for a pointer to a DPI statement.
-- This type is used to manage the lifecycle and operations on database statements.
newtype DPIStmt = DPIStmt (Ptr DPIStmt)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | A newtype wrapper for a pointer to a DPI connection.
-- This type ensures type safety when working with DPI connection pointers.
newtype DPIConn = DPIConn (Ptr DPIConn)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | Represents a foreign connection using a 'DPIConn' pointer wrapped in a 'ForeignPtr'.
-- This type manages memory automatically, ensuring proper cleanup.
newtype Connection = Connection (ForeignPtr DPIConn)
  deriving (Show, Eq)

-- | A newtype wrapper for a pointer to a DPI pool.
-- This type is used for managing database connection pools within the application.
newtype DPIPool = DPIPool (Ptr DPIPool)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | A newtype wrapper for a pointer to a DPI context.
-- The DPI context manages the overall state and configuration for DPI operations.
newtype DPIContext = DPIContext (Ptr DPIContext)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | A newtype wrapper for a pointer to a DPI sharding key column.
-- This type is used when working with sharded databases and their associated keys.
newtype DPIShardingKeyColumn = DPIShardingKeyColumn (Ptr DPIShardingKeyColumn)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPILob = DPILob (Ptr DPILob)
  deriving (Show, Eq)
  deriving newtype (Storable)

-- | Represents additional parameters for creating a connection pool.
-- The 'AdditionalConnectionParams' data type includes settings such as session counts,
-- timeouts, and modes.
data AdditionalConnectionParams = AdditionalConnectionParams
  { minSessions :: Natural            -- ^ Minimum number of sessions in the pool.
  , maxSessions :: Natural            -- ^ Maximum number of sessions in the pool.
  , sessionIncrement :: Natural       -- ^ Number of sessions to add when expanding the pool.
  , pingInterval :: Natural           -- ^ Interval at which to ping sessions to keep them alive.
  , pingTimeout :: Natural            -- ^ Timeout for pinging sessions.
  , homogeneous :: Natural            -- ^ Indicates whether the pool is homogeneous.
  , getMode :: DPIPoolGetMode         -- ^ Mode for obtaining sessions from the pool.
  , timeout :: Natural                -- ^ Timeout duration for sessions.
  , waitTimeout :: Natural            -- ^ Timeout for waiting to obtain a session.
  , maxLifetimeSession :: Natural     -- ^ Maximum lifetime of a session in the pool.
  , maxSessionsPerShard :: Natural    -- ^ Maximum number of sessions per shard.
  , authMode :: DPIAuthMode
  }
  deriving (Eq, Ord, Show)

-- | Default parameters for additional connection settings.
-- This provides a default configuration for connections that can be modified as needed.
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
    , authMode = DPI_MODE_AUTH_DEFAULT 
    }

-- | Connection parameters type
data ConnectionParams = ConnectionParams
  { user :: String -- ^ name of the user used for authenticating the user
  , pass :: String -- ^ password to use for authenticating the user
  , connString :: String -- ^ connect string identifying the database to which a connection is to be established,
  , additionalParams :: Maybe AdditionalConnectionParams -- ^ optional additional parameters
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
          let connCreate paramsPtr = 
                        dpiConn_create 
                            ctx 
                            userCString userLen passCString passLen connCString connLen nullPtr paramsPtr connPtr
          status <-
            case additionalParams params of
              Nothing -> connCreate nullPtr
              Just addParams ->
                withConnCreateParams $ \defaultConnParams -> do
                    let newConnParams = defaultConnParams { 
                        dpi_authMode = authMode addParams
                        -- New fields can be added here
                      }
                    alloca $ \newConnParamsPtr -> do 
                      poke newConnParamsPtr newConnParams
                      connCreate newConnParamsPtr

          throwOracleError status
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
    -- | dpiConn ** conn
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
  deriving (Show, Eq, Ord)

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
-- | Indicates the purity level of an Oracle session.
-- 'DPIPurity' is used to specify whether a session should be default or new.
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

-- | A finalizer for closing a DPI connection.
-- This function is called to safely close and clean up resources associated
-- with a DPI connection.
foreign import ccall "&finalize_connection_default"
  dpiConn_close_finalizer :: FunPtr (Ptr DPIConn -> IO ())

-- | A finalizer for releasing a DPI connection.
-- Ensures that the allocated resources for a connection are properly
-- released when it is no longer needed.
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

-- | Utilizes default pool creation parameters to execute an action.
-- This is useful for working with connection pools using predefined settings.
withDefaultPoolCreateParams :: (Ptr DPIPoolCreateParams -> IO a) -> IO a
withDefaultPoolCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \poolCreateParamsPtr -> do
    status <- dpiContext_initPoolCreateParams ctx poolCreateParamsPtr
    unless (status == 0) $ do
      error $ "pool create params status wasn't 0" <> show status
    f poolCreateParamsPtr

-- | Parameters for creating a DPI pool.
-- The 'DPIPoolCreateParams' data type includes various settings for pool creation,
-- such as session limits, timeouts, and encoding details.
data DPIPoolCreateParams = DPIPoolCreateParams
  { dpi_minSessions :: CUInt               -- ^ Minimum number of sessions in the pool.
  , dpi_maxSessions :: CUInt               -- ^ Maximum number of sessions in the pool.
  , dpi_sessionIncrement :: CUInt          -- ^ Increment for expanding the pool.
  , dpi_pingInterval :: CInt               -- ^ Interval for pinging sessions.
  , dpi_pingTimeout :: CInt                -- ^ Timeout for session pings.
  , dpi_homogeneous :: CInt                -- ^ Homogeneity of the pool.
  , dpi_externalAuth :: CInt               -- ^ Use of external authentication.
  , dpi_getMode :: DPIPoolGetMode          -- ^ Session retrieval mode.
  , dpi_outPoolName :: CString             -- ^ Name of the pool for output.
  , dpi_outPoolNameLength :: CUInt         -- ^ Length of the pool name.
  , dpi_timeout :: CUInt                   -- ^ Session timeout duration.
  , dpi_waitTimeout :: CUInt               -- ^ Timeout for waiting for a session.
  , dpi_maxLifetimeSession :: CUInt        -- ^ Maximum session lifetime.
  , dpi_plsqlFixupCallback :: CString      -- ^ PL/SQL fixup callback.
  , dpi_plsqlFixupCallbackLength :: CUInt  -- ^ Length of the PL/SQL fixup callback.
  , dpi_maxSessionsPerShard :: CUInt       -- ^ Maximum sessions per shard.
  , dpi_accessTokenCallback :: FunPtr ()   -- ^ Access token callback function.
  , dpi_accessTokenCallbackContext :: Ptr () -- ^ Context for the access token callback.
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

-- | Modes for obtaining connections from a connection pool.
-- 'DPIPoolGetMode' specifies different strategies for retrieving a connection.
data DPIPoolGetMode
  = DPI_MODE_POOL_GET_FORCEGET  -- ^ Forces obtaining a connection, regardless of wait time.
  | DPI_MODE_POOL_GET_NOWAIT    -- ^ Attempts to get a connection without waiting.
  | DPI_MODE_POOL_GET_TIMEDWAIT -- ^ Waits for a connection with a specified timeout.
  | DPI_MODE_POOL_GET_WAIT      -- ^ Waits indefinitely for a connection.
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

-- | Parameters used for creating a database connection.
-- 'ConnectionCreateParams' contains various fields to configure the connection,
-- such as authentication mode, connection class, session purity, and sharding keys.
data ConnectionCreateParams = ConnectionCreateParams
  { dpi_authMode :: DPIAuthMode         -- ^ Authentication mode to use for the connection.
  , connectionClass :: CString          -- ^ Class of the connection as a C string.
  , connectionClassLength :: CUInt      -- ^ Length of the connection class string.
  , purity :: DPIPurity                 -- ^ Purity level for the session.
  , newPassword :: CString              -- ^ New password for changing the existing one.
  , newPasswordLength :: CUInt          -- ^ Length of the new password string.
  , appContenxt :: Ptr DPIAppContext    -- ^ Application context to attach to the session.
  , numAppContext :: CUInt              -- ^ Number of items in the application context.
  , externalAuth :: CInt                -- ^ Flag to indicate external authentication.
  , externalHandle :: Ptr ()            -- ^ Pointer to an external authentication handle.
  , pool :: DPIPool                     -- ^ Connection pool for obtaining the connection.
  , tag :: CString                      -- ^ Tag to use when retrieving a connection.
  , tagLength :: CUInt                  -- ^ Length of the tag string.
  , matchAnyTag :: CInt                 -- ^ Flag to indicate if any tag should match.
  , outTag :: CString                   -- ^ Output tag after obtaining a connection.
  , outTagLength :: CUInt               -- ^ Length of the output tag string.
  , outTagFound :: CInt                 -- ^ Flag to indicate if the output tag was found.
  , shardingKeyColumn :: DPIShardingKeyColumn -- ^ Sharding key column for the connection.
  , numShardingKeyColumns :: Word8      -- ^ Number of sharding key columns.
  , superShardingKeyColumns :: Ptr DPIShardingKeyColumn -- ^ Super sharding key column.
  , numSuperShardingKeyColumns :: Word8 -- ^ Number of super sharding key columns.
  , outNewSession :: CInt               -- ^ Flag to indicate if a new session was created.
  }
  deriving (Show, Eq)

instance Storable ConnectionCreateParams where

    sizeOf _ =  sizeOf (undefined :: DPIAuthMode)
              + (4 * sizeOf (undefined :: CString))
              + (5 * sizeOf (undefined :: CUInt))
              + sizeOf (undefined :: DPIPurity)
              + sizeOf (undefined :: DPIAppContext)
              + (4 * sizeOf (undefined :: CInt))
              + sizeOf (undefined :: Ptr ())
              + sizeOf (undefined :: DPIPool)
              + (2 * sizeOf (undefined :: DPIShardingKeyColumn))
              + (2 * sizeOf (undefined :: Word8))
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
      let base = castPtr ptr
      ConnectionCreateParams
        <$> peek (base `plusPtr` 0)                                 -- dpi_authMode
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode)) -- connectionClass
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` sizeOf (undefined :: CString))     -- connectionClassLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` sizeOf (undefined :: CString)
                       `plusPtr` sizeOf (undefined :: CUInt))     -- purity
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` sizeOf (undefined :: CString)
                       `plusPtr` sizeOf (undefined :: CUInt)
                       `plusPtr` sizeOf (undefined :: DPIPurity)) -- newPassword
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` sizeOf (undefined :: CUInt)
                       `plusPtr` sizeOf (undefined :: DPIPurity)) -- newPasswordLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)) -- appContenxt
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)) -- numAppContext
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)) -- externalAuth
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: CInt)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)) -- externalHandle
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: CInt)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())) -- pool
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))
                       `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: CInt)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- tag
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))
                       `plusPtr` (3 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: CInt)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- tagLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))
                       `plusPtr` (4 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` sizeOf (undefined :: CInt)
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- matchAnyTag
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))
                       `plusPtr` (4 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (2 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- outTag
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (4 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (2 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- outTagLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (2 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- outTagFound
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (3 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)) -- shardingKeyColumn
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (3 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)
                       `plusPtr` sizeOf (undefined :: DPIShardingKeyColumn)) -- numShardingKeyColumns
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (3 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)
                       `plusPtr` sizeOf (undefined :: DPIShardingKeyColumn)
                       `plusPtr` sizeOf (undefined :: Word8)) -- superShardingKeyColumns
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (3 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)
                       `plusPtr` (2 * sizeOf (undefined :: DPIShardingKeyColumn))
                       `plusPtr` sizeOf (undefined :: Word8)) -- numSuperShardingKeyColumns
        <*> peek (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (5 * sizeOf (undefined :: CUInt))
                       `plusPtr` sizeOf (undefined :: DPIPurity)
                       `plusPtr` (3 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: DPIAppContext)
                       `plusPtr` sizeOf (undefined :: Ptr ())
                       `plusPtr` sizeOf (undefined :: DPIPool)
                       `plusPtr` (2 * sizeOf (undefined :: DPIShardingKeyColumn))
                       `plusPtr` (2 * sizeOf (undefined :: Word8))) -- outNewSession

    poke ptr ConnectionCreateParams{..} = do
      let base = castPtr ptr
      poke (base `plusPtr` 0) dpi_authMode
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode)) connectionClass
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` sizeOf (undefined :: CString)) connectionClassLength
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` sizeOf (undefined :: CString)
              `plusPtr` sizeOf (undefined :: CUInt))  purity
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` sizeOf (undefined :: CString)
              `plusPtr` sizeOf (undefined :: CUInt)
              `plusPtr` sizeOf (undefined :: DPIPurity)) newPassword
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` sizeOf (undefined :: CUInt)
              `plusPtr` sizeOf (undefined :: DPIPurity)) newPasswordLength
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (2 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)) appContenxt
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (2 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: DPIAppContext)) numAppContext
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (3 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: DPIAppContext)) externalAuth
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (3 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: CInt)
              `plusPtr` sizeOf (undefined :: DPIAppContext)) externalHandle
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (3 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: CInt)
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())) pool
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (2 * sizeOf (undefined :: CString))
              `plusPtr` (3 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: CInt)
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) tag
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (3 * sizeOf (undefined :: CString))
              `plusPtr` (3 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: CInt)
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) tagLength
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (3 * sizeOf (undefined :: CString))
              `plusPtr` (4 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` sizeOf (undefined :: CInt)
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) matchAnyTag
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (3 * sizeOf (undefined :: CString))
              `plusPtr` (4 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (2 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) outTag
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (4 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (2 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) outTagLength
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (2 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) outTagFound
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (3 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)) shardingKeyColumn
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (3 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)
              `plusPtr` sizeOf (undefined :: DPIShardingKeyColumn)) numShardingKeyColumns
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (3 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)
              `plusPtr` sizeOf (undefined :: DPIShardingKeyColumn)
              `plusPtr` sizeOf (undefined :: Word8)) superShardingKeyColumns
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (3 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)
              `plusPtr` (2 * sizeOf (undefined :: DPIShardingKeyColumn))
              `plusPtr` sizeOf (undefined :: Word8)) numSuperShardingKeyColumns
      poke (base `plusPtr` sizeOf (undefined :: DPIAuthMode) 
              `plusPtr` (4 * sizeOf (undefined :: CString))
              `plusPtr` (5 * sizeOf (undefined :: CUInt))
              `plusPtr` sizeOf (undefined :: DPIPurity)
              `plusPtr` (3 * sizeOf (undefined :: CInt))
              `plusPtr` sizeOf (undefined :: DPIAppContext)
              `plusPtr` sizeOf (undefined :: Ptr ())
              `plusPtr` sizeOf (undefined :: DPIPool)
              `plusPtr` (2 * sizeOf (undefined :: DPIShardingKeyColumn))
              `plusPtr` (2 * sizeOf (undefined :: Word8))) outNewSession
              
-- | Common parameters for creating DPI resources.
-- The 'DPICommonCreateParams' data type includes settings like encoding, edition,
-- and driver details.
data DPICommonCreateParams = DPICommonCreateParams
  { createMode :: DPICreateMode   -- ^ The mode for creating resources.
  , encoding :: CString           -- ^ The character encoding to use.
  , nencoding :: CString          -- ^ The national character encoding to use.
  , edition :: CString            -- ^ The edition of the database.
  , editionLength :: CInt         -- ^ The length of the edition string.
  , driverName :: CString         -- ^ The name of the driver.
  , driverNameLength :: CInt      -- ^ The length of the driver name.
  , sodaMetadataCache :: Int      -- ^ Setting for SODA metadata caching.
  , stmtCacheSize :: CInt         -- ^ Size of the statement cache.
  }
  deriving (Show, Eq)

instance Storable DPICommonCreateParams where
    sizeOf _ =  sizeOf (undefined :: DPICreateMode)
              + (4 * sizeOf (undefined :: CString))
              + (3 * sizeOf (undefined :: CInt))
              + sizeOf (undefined :: Int)

    alignment _ = alignment (undefined :: Int)

    peek ptr = do
      let base = castPtr ptr
      DPICommonCreateParams
        <$> peek (base `plusPtr` 0)                                   -- createMode
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode)) -- encoding
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` sizeOf (undefined :: CString))       -- nencoding
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))) -- edition
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))) -- editionLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))
                       `plusPtr` sizeOf (undefined :: CInt)) -- driverName
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` sizeOf (undefined :: CInt)) -- driverNameLength
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CInt))) -- sodaMetadataCache
        <*> peek (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: Int)) -- stmtCacheSize

    poke ptr DPICommonCreateParams{..} = do
        let base = castPtr ptr
        poke (base `plusPtr` 0) createMode
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode)) encoding
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` sizeOf (undefined :: CString)) nencoding
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (2 * sizeOf (undefined :: CString))) edition
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))) editionLength
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (3 * sizeOf (undefined :: CString))
                       `plusPtr` sizeOf (undefined :: CInt)) driverName
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` sizeOf (undefined :: CInt)) driverNameLength
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CInt))) sodaMetadataCache
        poke (base `plusPtr` sizeOf (undefined :: DPICreateMode) 
                       `plusPtr` (4 * sizeOf (undefined :: CString))
                       `plusPtr` (2 * sizeOf (undefined :: CInt))
                       `plusPtr` sizeOf (undefined :: Int)) stmtCacheSize

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

-- | A global context reference for DPI operations.
-- This reference is used to manage the global state shared across DPI functions.
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

-- | Renders error information to the standard output or a designated error log.
-- This function takes an 'ErrorInfo' structure and outputs its contents in a human-readable form.
--
-- /Arguments:/
--
-- * `ErrorInfo`: The error information to be rendered.
--
-- /Returns:/
--
-- An IO action to render the error information.
renderErrorInfo :: ErrorInfo -> IO ()
renderErrorInfo ErrorInfo {errorInfoCode, errorInfoMessage} = do
  putStrLn $ "Error code: " <> show errorInfoCode
  unless (errorInfoMessage == nullPtr) $ do
    str <- peekCString errorInfoMessage
    putStrLn $ "Error msg: " <> str

-- | Contains detailed information about errors encountered during database operations.
-- The 'ErrorInfo' data type provides a comprehensive set of fields to diagnose issues,
-- including error codes, messages, and whether the error is recoverable.
data ErrorInfo = ErrorInfo
  { errorInfoCode :: CInt               -- ^ The error code.
  , errorInfoOffset16 :: Word16         -- ^ The offset in bytes for the error.
  , errorInfoMessage :: CString         -- ^ The error message as a C string.
  , errorInfoMessageLength :: CUInt     -- ^ The length of the error message.
  , errorInfoEncoding :: CString        -- ^ The character encoding for the error message.
  , errorInfoFnName :: CString          -- ^ The name of the function where the error occurred.
  , errorInfoAction :: CString          -- ^ The action being performed when the error happened.
  , errorInfoSqlState :: CString        -- ^ The SQL state associated with the error.
  , errorInfoIsRecoverable :: CInt      -- ^ Indicates if the error is recoverable (non-zero if true).
  , errorInfoIsWarning :: CInt          -- ^ Indicates if the error is a warning (non-zero if true).
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (GStorable)

-- | Oracle Error type
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

-- | Throws an Oracle error based on the given error code.
-- This function is used to handle errors by converting them to Haskell exceptions.
throwOracleError :: CInt -> IO ()
throwOracleError returnCode = do
  unless (returnCode == 0) $
    (throwIO =<< toOracleError =<< getErrorInfo)

instance Exception OracleError

-- | Represents version information of the database or client.
-- The 'VersionInfo' data type includes various fields identifying the
-- specific version, release, and update numbers.
data VersionInfo = VersionInfo
  { versionNum :: CInt          -- ^ The version number.
  , releaseNum :: CInt          -- ^ The release number.
  , updateNum :: CInt           -- ^ The update number.
  , portReleaseNum :: CInt      -- ^ The port release number.
  , portUpdateNum :: CInt       -- ^ The port update number.
  , fullVersionNum :: CUInt     -- ^ The full version number as a single integer.
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

-- | Return information about the version of the Oracle Client that is being used.
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

-- | Returns the version information of the Oracle Database to which the connection has been made.
foreign import ccall "dpiConn_getServerVersion"
  dpiContext_getServerVersion ::
    Ptr DPIConn ->
    Ptr CString ->
    Ptr CInt ->
    Ptr VersionInfo ->
    IO Int

-- | Retrieves the server version information for a given database connection.
-- This function takes a 'Connection' and 'VersionInfo', returning a string
-- representation of the server version.
--
-- /Arguments:/
--
-- * `Connection`: The database connection from which to retrieve the server version.
-- * `VersionInfo`: Version information structure to be filled by the function.
--
-- /Returns:/
--
-- An IO action that produces a 'String' representing the server version.
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
            nullPtr
            versionInfoPtr
        if status == 0
          then peekCString =<< peek releaseStringPtr
          else error $ show status <> " oh no!"

foreign import ccall "dpiContext_initConnCreateParams"
  dpiContext_initConnCreateParams ::
    DPIContext ->
    Ptr ConnectionCreateParams ->
    IO Int

-- | Executes a given action with default 'ConnectionCreateParams'.
-- This function provides a convenient way to use default connection
-- creation parameters without manually specifying them.
--
-- /Arguments:/
--
-- * A function that takes 'ConnectionCreateParams' and returns an IO action.
--
-- /Returns:/
--
-- An IO action that executes with the default connection creation parameters.
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

-- | Represents a sequence of bytes, typically used for handling binary or encoded data.
-- The 'DPIBytes' structure holds a pointer to the byte data, its length, and its encoding.
data DPIBytes = DPIBytes
  { dpiBytesPtr :: CString       -- ^ Pointer to the byte data.
  , dpiBytesLength :: CUInt      -- ^ Length of the byte data.
  , dpiBytesEncoding :: CString  -- ^ Encoding of the byte data.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- | Creates a 'DPIBytes' representation of a UTF-8 encoded string.
-- Useful for passing string data to Oracle database functions that require 'DPIBytes'.
mkDPIBytesUTF8 :: String -> IO DPIBytes
mkDPIBytesUTF8 str = do
  (dpiBytesPtr, fromIntegral -> dpiBytesLength) <- newCStringLen str
  dpiBytesEncoding <- newCString "UTF-8"
  pure $ DPIBytes {..}

-- | Converts 'DPIBytes' representing a UTF-8 encoded string to a Haskell 'String'.
-- This function is useful for retrieving string data from Oracle databases.
mkStringFromDPIBytesUTF8 :: DPIBytes -> IO String
mkStringFromDPIBytesUTF8 DPIBytes{..} = peekCString dpiBytesPtr

-- | Represents an interval of time in terms of days, hours, minutes, seconds, and fractional seconds.
-- The 'DPIIntervalDS' type is typically used for handling intervals in Oracle database operations.
data DPIIntervalDS = DPIIntervalDS
  { days :: CInt      -- ^ The number of days in the interval.
  , hours :: CInt     -- ^ The number of hours in the interval.
  , minutes :: CInt   -- ^ The number of minutes in the interval.
  , seconds :: CInt   -- ^ The number of seconds in the interval.
  , fseconds :: CInt  -- ^ The fractional seconds in the interval.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- | Represents a year-month interval, useful for date calculations in Oracle databases.
-- The 'DPIIntervalYM' type captures the duration in terms of years and months.
data DPIIntervalYM = DPIIntervalYM
  { years :: CInt   -- ^ The number of years in the interval.
  , months :: CInt  -- ^ The number of months in the interval.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- | Represents a timestamp, including date, time, and timezone offset information.
-- The 'DPITimestamp' type is used for precise time representations and calculations.
data DPITimestamp = DPITimestamp
  { year :: Int16         -- ^ The year component of the timestamp.
  , month :: Word8        -- ^ The month component (1 to 12) of the timestamp.
  , day :: Word8          -- ^ The day component (1 to 31) of the timestamp.
  , hour :: Word8         -- ^ The hour component (0 to 23) of the timestamp.
  , minute :: Word8       -- ^ The minute component (0 to 59) of the timestamp.
  , second :: Word8       -- ^ The second component (0 to 59) of the timestamp.
  , fsecond :: CUInt      -- ^ The fractional seconds component of the timestamp.
  , tzHourOffset :: Int8  -- ^ The timezone hour offset from UTC.
  , tzMinuteOffset :: Int8 -- ^ The timezone minute offset from UTC.
  }
  deriving (Show, Eq)

instance Storable DPITimestamp where
    sizeOf _ = sizeOf (undefined :: Int16)
             + (5 * sizeOf (undefined :: Word8))
             + sizeOf (undefined :: CUInt)
             + (2 * sizeOf (undefined :: Int8))

    alignment _ = sizeOf (undefined :: CUInt)

    peek ptr = do
        let base = castPtr ptr
        DPITimestamp
          <$> peek (base `plusPtr` 0)                           -- year
          <*> peek (base `plusPtr`      sizeOf (undefined :: Int16)) -- month
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` sizeOf (undefined :: Word8)) -- day
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (2 * sizeOf (undefined :: Word8))) -- hour
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (3 * sizeOf (undefined :: Word8))) -- minute
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (4 * sizeOf (undefined :: Word8))) -- second
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (5 * sizeOf (undefined :: Word8))) -- fsecond
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (5 * sizeOf (undefined :: Word8))
                        `plusPtr` sizeOf (undefined :: CUInt)) -- tzHourOffset
          <*> peek (base `plusPtr` sizeOf (undefined :: Int16)
                        `plusPtr` (5 * sizeOf (undefined :: Word8))
                        `plusPtr` sizeOf (undefined :: CUInt)
                        `plusPtr` sizeOf (undefined :: Int8)) -- tzMinuteOffset
    
    poke ptr DPITimestamp{..} = do
        let base = castPtr ptr
        poke (base `plusPtr` 0) year
        poke (base `plusPtr` sizeOf (undefined :: Int16)) month
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` sizeOf (undefined :: Word8)) day
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (2 * sizeOf (undefined :: Word8))) hour
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (3 * sizeOf (undefined :: Word8))) minute
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (4 * sizeOf (undefined :: Word8))) second
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (5 * sizeOf (undefined :: Word8))) fsecond
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (5 * sizeOf (undefined :: Word8))
                       `plusPtr` sizeOf (undefined :: CUInt)) tzHourOffset
        poke (base `plusPtr` sizeOf (undefined :: Int16) 
                       `plusPtr` (5 * sizeOf (undefined :: Word8))
                       `plusPtr` sizeOf (undefined :: CUInt)
                       `plusPtr` sizeOf (undefined :: Int8)) tzMinuteOffset


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

-- | Represents an application context for Oracle sessions, including namespace, name, and value.
-- The 'DPIAppContext' type is used to store context-specific information for sessions.
data DPIAppContext = DPIAppContext
  { namespaceName :: CString       -- ^ The namespace name of the application context.
  , namespaceNameLength :: CUInt   -- ^ The length of the namespace name.
  , name :: CString                -- ^ The name of the application context.
  , nameLength :: CUInt            -- ^ The length of the name.
  , value :: CString               -- ^ The value associated with the application context.
  , valueLength :: CUInt           -- ^ The length of the value.
  }
  deriving (Show, Eq)

instance Storable DPIAppContext where
    sizeOf _ = (3 * sizeOf (undefined :: CString))
                + (3 * sizeOf (undefined :: CUInt))

    alignment _ = alignment (undefined :: CUInt)

    peek ptr = do
        let base = castPtr ptr
        DPIAppContext
          <$> peek (base `plusPtr` 0)                           -- namespaceName
          <*> peek (base `plusPtr`  sizeOf (undefined :: CString)) -- namespaceNameLength
          <*> peek (base `plusPtr`  sizeOf (undefined :: CString)
                            `plusPtr`  sizeOf (undefined :: CUInt)) -- name
          <*> peek (base `plusPtr` (2 * sizeOf (undefined :: CString))
                            `plusPtr` sizeOf (undefined :: CUInt)) -- nameLength
          <*> peek (base `plusPtr` (2 * sizeOf (undefined :: CString))
                            `plusPtr` (2 * sizeOf (undefined :: CUInt))) -- value
          <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CString))
                            `plusPtr` (2 * sizeOf (undefined :: CUInt))) -- valueLength

    poke ptr DPIAppContext{..} = do
        let base = castPtr ptr
        poke (base `plusPtr` 0) namespaceName
        poke (base `plusPtr` sizeOf (undefined :: CString)) namespaceNameLength
        poke (base `plusPtr` sizeOf (undefined :: CString)
                        `plusPtr` sizeOf (undefined :: CUInt)) name
        poke (base `plusPtr` (2 * sizeOf (undefined :: CString))
                        `plusPtr` sizeOf (undefined :: CUInt)) nameLength
        poke (base `plusPtr` (2 * sizeOf (undefined :: CString))
                        `plusPtr` (2 * sizeOf (undefined :: CUInt))) value
        poke (base `plusPtr` (3 * sizeOf (undefined :: CString))
                        `plusPtr` (2 * sizeOf (undefined :: CUInt))) valueLength

-- | Contains parameters for creating a DPI context, including driver and encoding settings.
-- The 'DPIContextCreateParams' type allows customization of the context creation process.
data DPIContextCreateParams = DPIContextCreateParams
  { defaultDriverName :: CString          -- ^ The default driver name as a C string.
  , defaultEncoding :: CString            -- ^ The default character encoding as a C string.
  , loadErrorUrl :: CString               -- ^ The URL for loading error information.
  , oracleClientLibDir :: CString         -- ^ The directory path to the Oracle client libraries.
  , oracleClientConfigDir :: CString      -- ^ The configuration directory for the Oracle client.
  }
  deriving (Show, Eq)

instance Storable DPIContextCreateParams where
    sizeOf _ = 5 * sizeOf (undefined :: CString)
    alignment _ = alignment (undefined :: CString)

    peek ptr = do
        let base = castPtr ptr
        DPIContextCreateParams
            <$> peek (base `plusPtr` 0) -- defaultDriverName
            <*> peek (base `plusPtr` sizeOf (undefined :: CString)) -- defaultEncoding
            <*> peek (base `plusPtr` (2 * sizeOf (undefined :: CString))) -- loadErrorUrl
            <*> peek (base `plusPtr` (3 * sizeOf (undefined :: CString))) -- oracleClientLibDir
            <*> peek (base `plusPtr` (4 * sizeOf (undefined :: CString))) -- oracleClientConfigDir

    poke ptr DPIContextCreateParams{..} = do
        let base = castPtr ptr
        poke (base `plusPtr` 0) defaultDriverName
        poke (base `plusPtr` sizeOf (undefined :: CString)) defaultEncoding
        poke (base `plusPtr` (2 * sizeOf (undefined :: CString))) loadErrorUrl
        poke (base `plusPtr` (3 * sizeOf (undefined :: CString))) oracleClientLibDir
        poke (base `plusPtr` (4 * sizeOf (undefined :: CString))) oracleClientConfigDir
        

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

-- | Prepares a SQL statement for execution using a connection.
--
-- /Arguments:/
--
-- * `Connection`: The database connection to use.
-- * `String`: The SQL query to prepare.
--
-- /Returns:/
--
-- An IO action that produces a 'DPIStmt' ready for execution.
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

-- | Specifies the execution mode for DPI operations.
-- Currently, only the 'DPI_MODE_EXEC_DEFAULT' mode is defined.
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

-- | Enumerates the native types used in Oracle data structures.
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

-- | Converts a 'DPINativeType' to an unsigned integer.
-- Useful for interfacing with C libraries that require numeric
-- representation of native types.
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
    deriving (Eq, Show)

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

-- | Retrieves a 'Double' value from the DPI data buffer.
foreign import ccall "dpiData_getDouble"
  dpiData_getDouble :: Ptr (DPIData ReadBuffer) -> IO Double

-- | Retrieves a 'Float' value from the DPI data buffer.
foreign import ccall "dpiData_getFloat"
  dpiData_getFloat :: Ptr (DPIData ReadBuffer) -> IO Float

-- | Retrieves a byte pointer from the DPI data buffer.
foreign import ccall "dpiData_getBytes"
  dpiData_getBytes :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPIBytes)

-- | Retrieves a timestamp pointer from the DPI data buffer.
foreign import ccall "dpiData_getTimestamp"
  dpiData_getTimestamp :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPITimestamp)

-- | Retrieves a 64-bit integer from the DPI data buffer.
foreign import ccall "dpiData_getInt64"
  dpiData_getInt64 :: Ptr (DPIData ReadBuffer) -> IO Int64

-- | Retrieves a 64-bit unsigned integer from the DPI data buffer.
foreign import ccall "dpiData_getUint64"
  dpiData_getUint64 :: Ptr (DPIData ReadBuffer) -> IO Word64

-- | Retrieves a Boolean value as an integer from the DPI data buffer.
foreign import ccall "dpiData_getBool"
  dpiData_getBool :: Ptr (DPIData ReadBuffer) -> IO Int

-- | Returns an integer where a non-zero value indicates that the data is null.
foreign import ccall "dpiData_getIsNull"
  dpiData_getIsNull :: Ptr (DPIData ReadBuffer) -> IO Int

-- | Binds a value by position in a DPI statement.
-- This function is used to set parameters in a statement before execution.
--
-- /Arguments:/
--
-- * `DPIStmt`: The statement to which the value is bound.
-- * `CUInt`: The position of the parameter to bind.
-- * `CUInt`: The native type number of the value.
-- * Pointer to 'DPIData WriteBuffer': The data to bind.
--
-- /Returns:/
--
-- An IO action resulting in an integer indicating success or failure.
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

-- | Binds a value by position in a statement.
-- This function is a higher-level wrapper around the C FFI for binding values.
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

-- | Retrieves the number of rows affected by the execution of a statement.
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


