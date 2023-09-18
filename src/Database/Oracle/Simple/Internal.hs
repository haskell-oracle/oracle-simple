{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Database.Oracle.Simple.Internal where

import Database.Oracle.Simple.TableInfo
import Data.Kind
import Data.List as L
import GHC.TypeLits
import GHC.Generics
import Control.Monad.State.Strict
import Control.Exception
import Control.Monad
import Data.Coerce
import Data.IORef
import Data.Text
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Generics
import System.IO.Unsafe

newtype DPIStmt = DPIStmt (Ptr DPIStmt)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype DPIConn = DPIConn (Ptr DPIConn)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype Conn = Conn (ForeignPtr DPIConn)
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

data ConnectionParams = ConnectionParams
  { user :: String
  , pass :: String
  , connString :: String
  }
  deriving (Eq, Ord, Show)

createConn
  :: ConnectionParams
  -> IO DPIConn
createConn params = do
  ctx <- readIORef globalContext
  alloca $ \connPtr -> do
    (userCString, fromIntegral -> userLen) <- newCStringLen (user params)
    (passCString, fromIntegral -> passLen) <- newCStringLen (pass params)
    (connCString, fromIntegral -> connLen) <- newCStringLen (connString params)
    throwOracleError
      =<< dpiConn_create
        ctx
        userCString
        userLen
        passCString
        passLen
        connCString
        connLen
        nullPtr
        nullPtr
        connPtr
    peek connPtr -- error "createConn: oh no" -- throwIO ConnectionException
    -- TODO: fetch errorInfo struct... somehow?

-- DPI_EXPORT int dpiConn_create(const dpiContext *context, const char *userName,
--         uint32_t userNameLength, const char *password, uint32_t passwordLength,
--         const char *connectString, uint32_t connectStringLength,
--         const dpiCommonCreateParams *commonParams,
--         dpiConnCreateParams *createParams, dpiConn **conn);

foreign import ccall unsafe "dpiConn_create"
  dpiConn_create
    :: DPIContext
    -- ^ const dpiContext *context
    -> CString
    -- ^ const char *userName
    -> CUInt
    -- ^ uint32_t userNameLength
    -> CString
    -- ^ const char *password
    -> CUInt
    -- ^ uint32_t passwordLength
    -> CString
    -- ^ const char *connectString
    -> CUInt
    -- ^ uint32_t conn length
    -> Ptr DPICommonCreateParams
    -- ^ const dpiCommonCreateParams *commonParams
    -> Ptr DPIConnCreateParams
    -- ^ const dpiConnCreateParams *createParams
    -> Ptr DPIConn
    -- ^ dpi * conn
    -> IO CInt

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
    fromDPIAuthMode <$> peek (castPtr ptr) >>= \case
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
    fromDPIPurity <$> peek (castPtr ptr) >>= \case
      Nothing -> fail "DPIPurity.peek: Invalid create mode"
      Just mode -> pure mode
  poke ptr mode =
    poke (castPtr ptr) (toDPIPurity mode)

toDPIPurity :: DPIPurity -> CUInt
toDPIPurity = fromIntegral . fromEnum

fromDPIPurity :: CUInt -> Maybe DPIPurity
fromDPIPurity 0 = Just DPI_PURITY_DEFAULT
fromDPIPurity 1 = Just DPI_PURITY_NEW
fromDPIPurity 2 = Just DPI_PURITY_SELF
fromDPIPurity _ = Nothing

-- struct dpiConnCreateParams {
--     dpiAuthMode authMode;
--     const char *connectionClass;
--     uint32_t connectionClassLength;
--     dpiPurity purity;
--     const char *newPassword;
--     uint32_t newPasswordLength;
--     dpiAppContext *appContext;
--     uint32_t numAppContext;
--     int externalAuth;
--     void *externalHandle;
--     dpiPool *pool;
--     const char *tag;
--     uint32_t tagLength;
--     int matchAnyTag;
--     const char *outTag;
--     uint32_t outTagLength;
--     int outTagFound;
--     dpiShardingKeyColumn *shardingKeyColumns;
--     uint8_t numShardingKeyColumns;
--     dpiShardingKeyColumn *superShardingKeyColumns;
--     uint8_t numSuperShardingKeyColumns;
--     int outNewSession;
-- };

data DPIConnCreateParams = DPIConnCreateParams
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

-- struct dpiCommonCreateParams {
--     dpiCreateMode createMode;
--     const char *encoding;
--     const char *nencoding;
--     const char *edition;
--     ui nt32_t editionLength;
--     const char *driverName;
--     uint32_t driverNameLength;
--     int sodaMetadataCache;
--     uint32_t stmtCacheSize;
-- };

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
    fromDPICreateMode <$> peek (castPtr ptr) >>= \case
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
  dpiContext_create
    :: CInt
    -- ^ major version
    -> CInt
    -- ^ minor version
    -> Ptr DPIContext
    -- ^ context return
    -> Ptr ErrorInfo
    -- ^ error info struct
    -> IO Int

globalContext :: IORef DPIContext
{-# NOINLINE globalContext #-}
globalContext = unsafePerformIO (newIORef =<< createContext)


foreign import ccall "getMajorVersion" getMajorVersion :: IO CInt
foreign import ccall "getMinorVersion" getMinorVersion :: IO CInt

-- dpiContext_create(majorVersion, minorVersion, context, errorInfo) \
--     dpiContext_createWithParams(majorVersion, minorVersion, NULL, context, \
--             errorInfo)

createContext
  :: IO DPIContext
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
renderErrorInfo ErrorInfo{errorInfoCode, errorInfoMessage} = do
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
toOracleError ErrorInfo{..} = do
  oracleErrorFnName <- peekCString errorInfoFnName
  oracleErrorAction <- peekCString errorInfoAction
  oracleErrorMessage <- peekCStringLen (errorInfoMessage, fromIntegral errorInfoMessageLength)
  oracleErrorSqlState <- peekCString errorInfoSqlState
  let oracleErrorCode = fromIntegral errorInfoCode
  let oracleErrorIsRecoverable = intToBool $ fromIntegral errorInfoIsRecoverable
  let oracleErrorIsWarning = intToBool $ fromIntegral errorInfoIsWarning
  pure OracleError{..}
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

-- struct dpiVersionInfo {
--     int versionNum;
--     int releaseNum;
--     int updateNum;
--     int portReleaseNum;
--     int portUpdateNum;
--     uint32_t fullVersionNum;
-- };

data VersionInfo = VersionInfo
  { versionNum :: CInt
  , releaseNum :: CInt
  , updateNum :: CInt
  , portReleaseNum :: CInt
  , portUpdateNum :: CInt
  , fullVersionNum :: CUInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- DPI_EXPORT int dpiContext_getClientVersion(const dpiContext *context,
--         dpiVersionInfo *versionInfo);

getClientVersion
  :: IO VersionInfo
getClientVersion = do
  ctx <- readIORef globalContext
  alloca $ \versionPtr -> do
    statusCode <- dpiContext_getClientVersion ctx versionPtr
    if statusCode == 0
      then peek versionPtr
      else error ("getClientVersion: " <> show statusCode)

foreign import ccall "dpiContext_getClientVersion"
  dpiContext_getClientVersion
    :: DPIContext
    -> Ptr VersionInfo
    -> IO Int

-- DPI_EXPORT int dpiConn_getServerVersion(dpiConn *conn,
--         const char **releaseString, uint32_t *releaseStringLength,
--         dpiVersionInfo *versionInfo);

foreign import ccall "dpiConn_getServerVersion"
  dpiContext_getServerVersion
    :: DPIConn
    -> Ptr CString
    -> CInt
    -> Ptr VersionInfo
    -> IO Int

getServerVersion
  :: DPIConn
  -> VersionInfo
  -> IO String
getServerVersion con versionInfo = do
  alloca $ \releaseStringPtr -> do
    alloca $ \versionInfoPtr -> do
      poke versionInfoPtr versionInfo
      status <- dpiContext_getServerVersion con releaseStringPtr (fromIntegral (10 :: Int)) versionInfoPtr
      if status == 0
        then (peekCString <=< peek) releaseStringPtr
        else error $ show status <> " oh no!"

-- DPI_EXPORT int dpiContext_initCommonCreateParams(const dpiContext *context,
--        dpiCommonCreateParams *params);

foreign import ccall "dpiContext_initCommonCreateParams"
  dpiContext_initCommonCreateParams
    :: DPIContext
    -> Ptr DPICommonCreateParams
    -> IO Int

withCommonCreateParams
  :: (DPICommonCreateParams -> IO a)
  -> IO a
withCommonCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \commonCreateParamsPtr -> do
    status <- dpiContext_initCommonCreateParams ctx commonCreateParamsPtr
    unless (status == 0) $ do
      error $ "common create params isn't 0" <> show status
    f =<< peek commonCreateParamsPtr

-- DPI_EXPORT int dpiContext_initConnCreateParams(const dpiContext *context,
--        dpiConnCreateParams *params);

foreign import ccall "dpiContext_initConnCreateParams"
  dpiContext_initConnCreateParams
    :: DPIContext
    -> Ptr DPIConnCreateParams
    -> IO Int

withConnCreateParams
  :: (DPIConnCreateParams -> IO a)
  -> IO a
withConnCreateParams f = do
  ctx <- readIORef globalContext
  alloca $ \connCreateParamsPtr -> do
    status <- dpiContext_initConnCreateParams ctx connCreateParamsPtr
    unless (status == 0) $ do
      error $ "conn create params isn't 0" <> show status
    f =<< peek connCreateParamsPtr

-- typedef struct {
--     char *ptr;
--     uint32_t length;
--     const char *encoding;
-- } dpiBytes;

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

-- typedef struct {
--     int32_t days;
--     int32_t hours;
--     int32_t minutes;
--     int32_t seconds;
--     int32_t fseconds;
-- } dpiIntervalDS;

data DPIIntervalDS = DPIIntervalDS
  { days :: CInt
  , hours :: CInt
  , minutes :: CInt
  , seconds :: CInt
  , fseconds :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- typedef struct {
--     int32_t years;
--     int32_t months;
-- } dpiIntervalYM;

data DPIIntervalYM = DPIIntervalYM
  { years :: CInt
  , months :: CInt
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (GStorable)

-- typedef struct {
--     int16_t year;
--     uint8_t month;
--     uint8_t day;
--     uint8_t hour;
--     uint8_t minute;
--     uint8_t second;
--     uint32_t fsecond;
--     int8_t tzHourOffset;
--     int8_t tzMinuteOffset;
-- } dpiTimestamp;

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

instance HasDPINativeType DPITimestamp where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_TIMESTAMP

-- struct dpiAppContext {
--     const char *namespaceName;
--     uint32_t namespaceNameLength;
--     const char *name;
--     uint32_t nameLength;
--     const char *value;
--     uint32_t valueLength;
-- };

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

-- struct dpiContextCreateParams {
--     const char *defaultDriverName;
--     const char *defaultEncoding;
--     const char *loadErrorUrl;
--     const char *oracleClientLibDir;
--     const char *oracleClientConfigDir;
-- };

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
  dpiConn_prepareStmt
    :: DPIConn
    -> CInt
    -> CString
    -> CUInt
    -> CString
    -> CUInt
    -> Ptr DPIStmt
    -> IO CInt

prepareStmt
  :: DPIConn
  -> String
  -- ^ sql
  -> IO DPIStmt
prepareStmt conn sql = do
  alloca $ \stmtPtr -> do
    (sqlCStr, fromIntegral -> sqlCStrLen) <- newCStringLen sql
    status <- dpiConn_prepareStmt conn 0 sqlCStr sqlCStrLen nullPtr 0 stmtPtr
    throwOracleError status
    peek stmtPtr

-- typedef uint32_t dpiExecMode;

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

fromDPIModeExec :: CUInt -> Maybe DPIModeExec
fromDPIModeExec 0x00000000 = Just DPI_MODE_EXEC_DEFAULT
fromDPIModeExec 0x00000010 = Just DPI_MODE_EXEC_DESCRIBE_ONLY
fromDPIModeExec 0x00000020 = Just DPI_MODE_EXEC_COMMIT_ON_SUCCESS
fromDPIModeExec 0x00000080 = Just DPI_MODE_EXEC_BATCH_ERRORS
fromDPIModeExec 0x00000100 = Just DPI_MODE_EXEC_PARSE_ONLY
fromDPIModeExec 0x00100000 = Just DPI_MODE_EXEC_ARRAY_DML_ROWCOUNTS
fromDPIModeExec _ = Nothing

-- DPI_EXPORT int dpiStmt_execute(dpiStmt *stmt, dpiExecMode mode,
--        uint32_t *numQueryColumns);

foreign import ccall "dpiStmt_execute"
  dpiStmt_execute
    :: DPIStmt
    -> CUInt
    -> Ptr CUInt
    -> IO CInt

-- DPI_EXPORT int dpiStmt_execute(dpiStmt *stmt, dpiExecMode mode,
--         uint32_t *numQueryColumns);

-- | Execute a statement.
execute
  :: DPIStmt
  -- ^ Statement to be executed
  -> DPIModeExec
  -- ^ Execution mode
  -> IO CUInt
  -- ^ query columns
execute stmt mode =
  alloca $ \rowsPtr -> do
    throwOracleError =<< dpiStmt_execute stmt (toDPIModeExec mode) rowsPtr
    peek rowsPtr

-- DPI_EXPORT int dpiStmt_fetch(dpiStmt *stmt, int *found,
--      uint32_t *bufferRowIndex);

foreign import ccall "dpiStmt_fetch"
  dpiStmt_fetch
    :: DPIStmt
    -> Ptr CInt
    -> Ptr CUInt
    -> IO CInt

-- | Fetch a single row from the buffers defined for the query.
fetch
  :: DPIStmt
  -- ^ Statement from which row is to be fetched
  -> IO CInt
fetch stmt =
  alloca $ \bufferRowIdxPtr ->
    alloca $ \foundPtr -> do
      throwOracleError =<< dpiStmt_fetch stmt foundPtr bufferRowIdxPtr
      peek foundPtr

-- DPI_EXPORT int dpiStmt_getQueryValue(dpiStmt *stmt, uint32_t pos,
--        dpiNativeTypeNum *nativeTypeNum, dpiData **data);

foreign import ccall "dpiStmt_getQueryValue"
  dpiStmt_getQueryValue
    :: DPIStmt
    -> CUInt
    -> Ptr CUInt
    -> Ptr (Ptr (DPIData ReadBuffer))
    -> IO CInt

-- | Return the value of the column at the given position for the currently fetched row.
getQueryValue
  :: DPIStmt
  -- ^ Statement from which column value is to be retrieved
  -> CUInt
  -- ^ Column position
  -> IO (DPINativeType, Ptr (DPIData ReadBuffer))
getQueryValue stmt pos = do
  alloca $ \(buffer :: Ptr (Ptr (DPIData ReadBuffer))) -> do
    alloca $ \(typ :: Ptr CUInt) -> do
      throwOracleError =<< dpiStmt_getQueryValue stmt pos typ buffer
      (uintToDPINativeType <$> peek typ) >>= \case
        Nothing ->
          error "getQueryValue: Invalid type returned"
        Just typ -> do
          dataBuffer <- peek buffer
          pure (typ, dataBuffer)

-- | Class of all Haskell types that have an equivalent DPI native type.
class HasDPINativeType a where
  dpiNativeType :: Proxy a -> DPINativeType
  -- ^ DPI native type for the Haskell type
  --
instance HasDPINativeType Double where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_DOUBLE

instance HasDPINativeType Float where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_FLOAT

instance HasDPINativeType Text where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_BYTES

instance HasDPINativeType String where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_BYTES

instance HasDPINativeType Int64 where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_INT64

instance HasDPINativeType Word64 where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_UINT64

instance HasDPINativeType Bool where
  dpiNativeType Proxy = DPI_NATIVE_TYPE_BOOLEAN

instance HasDPINativeType a => HasDPINativeType (Maybe a) where
  dpiNativeType Proxy = dpiNativeType (Proxy @a)

instance HasDPINativeType Int where
  dpiNativeType Proxy = dpiNativeType (Proxy @Int64)

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

foreign import ccall "dpiConn_release" dpiConn_release :: DPIConn -> IO CInt
foreign import ccall "dpiStmt_release" dpiStmt_release :: DPIStmt -> IO CInt

connRelease
  :: DPIConn
  -> IO ()
connRelease = throwOracleError <=< dpiConn_release

stmtRelease
  :: DPIStmt
  -> IO ()
stmtRelease = throwOracleError <=< dpiStmt_release

-- | Used to write values to or read values from a column.
data DPIData a = DPIData
  { dataIsNull :: CInt
  -- ^ If reading, a null value was read. If writing, writes a null value.
  , dataValue :: a
  -- ^ The value that was read/will be written, of type 'ReadBuffer' or 'WriteBuffer'.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GStorable)

-- | An opaque pointer type for the @dpiDataBuffer@ union that we read from.
-- We cannot write to this in a way that ODPIC could use.
-- For poking purposes, use 'WriteBuffer'.
newtype ReadBuffer = ReadBuffer (Ptr ReadBuffer)
  deriving (Show, Eq)
  deriving newtype Storable

-- | @dpiDataBuffer@ union that we can write to.
-- We cannot read from this without a hint as to what type of data it contains.
data WriteBuffer =
  AsBoolean CBool
  | AsInt64 Int64
  | AsUInt64 Word64
  | AsFloat Float
  | AsDouble Double
  | AsString CString
  | AsBytes DPIBytes
  | AsTimestamp DPITimestamp
  | AsNull (Ptr ())
  deriving (Show, Eq, Generic)

instance Storable WriteBuffer where
  sizeOf _ = sizeOf (undefined :: DPITimestamp) -- size of largest element
  alignment _ = 8
  peek = error "WriteBuffer says: don't peek in here!" -- handle this better!

  poke ptr (AsInt64 intVal) = poke (castPtr ptr) intVal
  poke ptr (AsUInt64 word64Val) = poke (castPtr ptr) word64Val
  poke ptr (AsFloat floatVal) = poke (castPtr ptr) floatVal
  poke ptr (AsDouble doubleVal) = poke (castPtr ptr) doubleVal
  poke ptr (AsString cStringVal) = poke (castPtr ptr) cStringVal
  poke ptr (AsBytes dpiBytesVal) = poke (castPtr ptr) dpiBytesVal
  poke ptr (AsTimestamp dpiTimeStampVal) = poke (castPtr ptr) dpiTimeStampVal
  poke ptr (AsNull nullVal) = poke (castPtr ptr) nullVal


--instance GStorable DPIDataBuffer

-- DPI_EXPORT double dpiData_getDouble(dpiData *data);

foreign import ccall "dpiData_getDouble"
  dpiData_getDouble :: Ptr (DPIData ReadBuffer) -> IO Double

foreign import ccall "dpiData_getFloat"
  dpiData_getFloat :: Ptr (DPIData ReadBuffer) -> IO Float

-- DPI_EXPORT dpiBytes *dpiData_getBytes(dpiData *data);

foreign import ccall "dpiData_getBytes"
  dpiData_getBytes :: Ptr (DPIData ReadBuffer) -> IO (Ptr DPIBytes)

-- DPI_EXPORT dpiTimestamp *dpiData_getTimestamp(dpiData *data);

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

-- DPI_EXPORT int dpiStmt_bindValueByPos(dpiStmt *stmt, uint32_t pos,
--         dpiNativeTypeNum nativeTypeNum, dpiData *data);

foreign import ccall "dpiStmt_bindValueByPos"
  dpiStmt_bindValueByPos
    :: DPIStmt
    -- ^ dpiStmt *stmt
    -> CUInt
    -- ^ uint32_t pos
    -> CUInt
    -- ^ dpiNativeTypeNum nativeTypeNum
    -> Ptr (DPIData WriteBuffer)
    -- ^ dpiData *data
    -> IO CInt
    -- ^ int

bindValueByPos :: DPIStmt -> Column -> DPINativeType -> (DPIData WriteBuffer) -> IO ()
bindValueByPos stmt col nativeType val = do
  alloca $ \dpiData' -> do
    poke dpiData' val
    throwOracleError
      =<< dpiStmt_bindValueByPos stmt (fromIntegral $ getColumn col) (dpiNativeTypeToUInt nativeType) dpiData'
    pure ()

-- | Column position, starting with 1 for the first column.
newtype Column = Column {getColumn :: Word32}
  deriving newtype (Num, Show)

