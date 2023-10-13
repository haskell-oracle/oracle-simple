{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Pool (Pool, createPool, acquireConnection, withPool, withPoolConnection, closePool) where

import Control.Exception (bracket)
import Data.IORef (readIORef)
import Foreign
  ( ForeignPtr
  , FunPtr
  , Ptr
  , addForeignPtrFinalizer
  , alloca
  , finalizeForeignPtr
  , newForeignPtr_
  , nullPtr
  , peek
  , withForeignPtr
  )
import Foreign.C (CInt (CInt), CString, CUInt (CUInt), newCStringLen)

import Database.Oracle.Simple.Internal
  ( Connection (Connection)
  , ConnectionCreateParams
  , ConnectionParams (connString, pass, user)
  , DPICommonCreateParams
  , DPIConn (DPIConn)
  , DPIContext (DPIContext)
  , DPIPool (DPIPool)
  , close
  , dpiConn_close_finalizer
  , dpiConn_release_finalizer
  , globalContext
  , throwOracleError
  )

-- | A session pool; a group of stateless connections ("sessions") to the database.
newtype Pool = Pool (ForeignPtr DPIPool)
  deriving (Show, Eq)

-- | Creates and maintains a group of stateless connections to the database.
createPool
  :: ConnectionParams
  -> IO Pool
createPool params = do
  ctx <- readIORef globalContext
  DPIPool poolPtr <- alloca $ \connPtr -> do
    (userCString, fromIntegral -> userLen) <- newCStringLen (user params)
    (passCString, fromIntegral -> passLen) <- newCStringLen (pass params)
    (connCString, fromIntegral -> connLen) <- newCStringLen (connString params)
    throwOracleError
      =<< dpiPool_create
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
    peek connPtr
  fptr <- newForeignPtr_ poolPtr
  addForeignPtrFinalizer dpiPool_release_finalizer fptr
  addForeignPtrFinalizer dpiPool_close_finalizer fptr
  pure (Pool fptr)

foreign import ccall unsafe "dpiPool_create"
  dpiPool_create
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
    -- ^ uint32_t connLength
    -> Ptr DPICommonCreateParams
    -- ^ const dpiCommonCreateParams *commonParams
    -> Ptr ConnectionCreateParams
    -- ^ const dpiPoolCreateParams *createParams
    -> Ptr DPIPool
    -- ^ dpiPool **pool
    -> IO CInt

foreign import ccall "&close_pool_default"
  dpiPool_close_finalizer :: FunPtr (Ptr DPIPool -> IO ())

foreign import ccall "&dpiPool_release"
  dpiPool_release_finalizer :: FunPtr (Ptr DPIPool -> IO ())

-- | Close a session pool.
closePool :: Pool -> IO ()
closePool (Pool pool) = finalizeForeignPtr pool

-- | Bracket a computation between creating and closing a session pool.
withPool :: ConnectionParams -> (Pool -> IO c) -> IO c
withPool params = bracket (createPool params) closePool

-- | Acquire a connection from a session pool.
acquireConnection :: Pool -> IO Connection
acquireConnection (Pool poolFptr) = do
  (DPIConn connPtr) <- withForeignPtr poolFptr $ \pool -> do
    alloca $ \conn -> do
      throwOracleError =<< acquire_connection pool conn
      peek conn
  fptr <- newForeignPtr_ connPtr
  addForeignPtrFinalizer dpiConn_release_finalizer fptr
  addForeignPtrFinalizer dpiConn_close_finalizer fptr
  pure (Connection fptr)

foreign import ccall unsafe "acquire_connection"
  acquire_connection
    :: Ptr DPIPool
    -- ^ dpiPool *pool
    -> Ptr DPIConn
    -- ^ dpiConn **conn
    -> IO CInt

-- | Bracket a computation between acquiring a connection from a session pool and releasing the connection.
withPoolConnection :: Pool -> (Connection -> IO c) -> IO c
withPoolConnection pool = bracket (acquireConnection pool) close
