{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Database.Oracle.Simple.LOB
--
-- Implementation of handler functions for oracle's large objects (CLOB, BLOB, NCLOB, BFILE)
module Database.Oracle.Simple.LOB
  ( LOB
  , LOBType (..)
  , genLOB
  , readLOB
  , writeLOB
  , closeLOB
  , openLOBResource
  , closeLOBResource
  , readLOBBytes
  , getLOBType
  , setLOBVal
  , copyLOB
  , getLOBBufferSize
  , getLOBChunkSize
  , getLOBDirectoryAndFile
  , setLOBDirectoryAndFile
  , isLOBResoureOpen
  , doesLOBFileExists
  , getLOBSize
  , releaseLOB
  , trimLOBVal
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import Database.Oracle.Simple.Internal
import Foreign
import Foreign.C.String
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))

newtype DPILob = DPILob (Ptr DPILob)
  deriving (Show, Eq)
  deriving newtype (Storable)

newtype LOB = LOB (ForeignPtr DPILob)
  deriving (Show, Eq)

data LOBType = CLOB | NCLOB | BLOB
  deriving (Show, Eq)

lobTypeToCUInt :: LOBType -> CUInt
lobTypeToCUInt CLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_CLOB
lobTypeToCUInt NCLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_NCLOB
lobTypeToCUInt BLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_BLOB

cuintToLobType :: CUInt -> Maybe LOBType
cuintToLobType 2018 = Just NCLOB -- DPI_ORACLE_TYPE_NCLOB
cuintToLobType 2017 = Just CLOB -- DPI_ORACLE_TYPE_CLOB
cuintToLobType 2019 = Just BLOB -- DPI_ORACLE_TYPE_BLOB
cuintToLobType _ = Nothing

genLOB :: Connection -> LOBType -> IO LOB
genLOB (Connection fptr) lobType = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \dpiLobPtr -> do
      throwOracleError =<< dpiConn_newTempLob conn (lobTypeToCUInt lobType) dpiLobPtr
      f <- newForeignPtr_ dpiLobPtr
      return (LOB f)

foreign import ccall unsafe "dpiConn_newTempLob"
  dpiConn_newTempLob
    :: Ptr DPIConn
    -- ^ dpiConn *
    -> CUInt
    -- ^ dpiOracleTypeNum
    -> Ptr DPILob
    -- ^ dpiLob **
    -> IO CInt

closeLOB :: LOB -> IO ()
closeLOB (LOB lob) = finalizeForeignPtr lob

closeLOBResource :: LOB -> IO ()
closeLOBResource (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    throwOracleError =<< dpiLob_closeResource lob

foreign import ccall unsafe "dpiLob_closeResource"
  dpiLob_closeResource
    :: Ptr DPILob
    -- ^ dpiLob *
    -> IO CInt

copyLOB :: LOB -> IO LOB
copyLOB (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \dpiLobPtr -> do
      throwOracleError =<< dpiLob_copy lob dpiLobPtr
      f <- newForeignPtr_ dpiLobPtr
      return (LOB f)

foreign import ccall unsafe "dpiLob_copy"
  dpiLob_copy
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr DPILob
    -- ^ dpiLob **
    -> IO CInt

getLOBBufferSize :: LOB -> Int64 -> IO Int64
getLOBBufferSize (LOB fptr) sizeInChars = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \sizeInBytesPtr -> do
      throwOracleError
        =<< dpiLob_getBufferSize lob (CULong $ fromIntegral sizeInChars) sizeInBytesPtr
      fromIntegral <$> peek sizeInBytesPtr

foreign import ccall unsafe "dpiLob_getBufferSize"
  dpiLob_getBufferSize
    :: Ptr DPILob
    -- ^ dpiLob *
    -> CULong
    -- ^ sizeInChars
    -> Ptr CULong
    -- ^ sizeInBytes
    -> IO CInt

getLOBChunkSize :: LOB -> IO Int
getLOBChunkSize (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \sizePtr -> do
      throwOracleError
        =<< dpiLob_getChunkSize lob sizePtr
      fromIntegral <$> peek sizePtr

foreign import ccall unsafe "dpiLob_getChunkSize"
  dpiLob_getChunkSize
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CUInt
    -- ^ uint32_t size
    -> IO CInt

getLOBDirectoryAndFile :: LOB -> IO (String, String)
getLOBDirectoryAndFile (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \directoryAliasPtr -> do
      alloca $ \directoryAliasLenPtr -> do
        alloca $ \fileNamePtr -> do
          alloca $ \fileNameLenPtr -> do
            throwOracleError
              =<< dpiLob_getDirectoryAndFileName
                lob
                directoryAliasPtr
                directoryAliasLenPtr
                fileNamePtr
                fileNameLenPtr
            r1 <- peekCString =<< peek fileNamePtr
            r2 <- peekCString =<< peek directoryAliasPtr
            return (r1, r2)

foreign import ccall unsafe "dpiLob_getDirectoryAndFileName"
  dpiLob_getDirectoryAndFileName
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CString
    -- ^ const char **directoryAlias
    -> Ptr CUInt
    -- ^ uint32_t *directoryAliasLength
    -> Ptr CString
    -- ^ const char **fileName
    -> Ptr CUInt
    -- ^ uint32_t *fileNameLength
    -> IO CInt

setLOBDirectoryAndFile :: LOB -> String -> String -> IO ()
setLOBDirectoryAndFile (LOB fptr) directoryName fileName = do
  withForeignPtr fptr $ \lob -> do
    withCStringLen directoryName $ \(dirNamePtr, dirNameLen) -> do
      withCStringLen fileName $ \(fNamePtr, fNameLen) -> do
        throwOracleError
          =<< dpiLob_setDirectoryAndFileName
            lob
            dirNamePtr
            (fromIntegral dirNameLen)
            fNamePtr
            (fromIntegral fNameLen)

foreign import ccall unsafe "dpiLob_setDirectoryAndFileName"
  dpiLob_setDirectoryAndFileName
    :: Ptr DPILob
    -- ^ dpiLob *
    -> CString
    -- ^ const char *directoryAlias
    -> CUInt
    -- ^ uint32_t directoryAliasLength
    -> CString
    -- ^ const char *fileName
    -> CUInt
    -- ^ uint32_t fileNameLength
    -> IO CInt

doesLOBFileExists :: LOB -> IO Bool
doesLOBFileExists (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \isOpenPtr -> do
      throwOracleError
        =<< dpiLob_getFileExists lob isOpenPtr
      r <- peek isOpenPtr
      if r == 0 then return False else return True

foreign import ccall unsafe "dpiLob_getFileExists"
  dpiLob_getFileExists
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CInt
    -- ^ int *isOpen
    -> IO CInt

isLOBResoureOpen :: LOB -> IO Bool
isLOBResoureOpen (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \isOpenPtr -> do
      throwOracleError
        =<< dpiLob_getIsResourceOpen lob isOpenPtr
      r <- peek isOpenPtr
      if r == 0 then return False else return True

foreign import ccall unsafe "dpiLob_getIsResourceOpen"
  dpiLob_getIsResourceOpen
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CInt
    -- ^ int *isOpen
    -> IO CInt

{-
WARNING: for historical reasons, Oracle stores CLOBs and NCLOBs using the UTF-16 encoding, regardless of what encoding is otherwise in use by the database. The number of characters, however, is defined by the number of UCS-2 codepoints. For this reason, if a character requires more than one UCS-2 codepoint, the size returned will be inaccurate and care must be taken to account for the difference.
-}
getLOBSize :: LOB -> IO Int64
getLOBSize (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \sizePtr -> do
      throwOracleError
        =<< dpiLob_getSize lob sizePtr
      fromIntegral <$> peek sizePtr

foreign import ccall unsafe "dpiLob_getSize"
  dpiLob_getSize
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CULong
    -- ^ uint64_t *size
    -> IO CInt

getLOBType :: LOB -> IO LOBType
getLOBType (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \numTypePtr -> do
      throwOracleError
        =<< dpiLob_getType lob numTypePtr
      mType <- cuintToLobType <$> peek numTypePtr
      case mType of
        Nothing -> return BLOB -- kind of impossible case
        Just r -> return r

foreign import ccall unsafe "dpiLob_getType"
  dpiLob_getType
    :: Ptr DPILob
    -- ^ dpiLob *
    -> Ptr CUInt
    -- ^ dpiOracleTypeNum numType
    -> IO CInt

openLOBResource :: LOB -> IO ()
openLOBResource (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    throwOracleError
      =<< dpiLob_openResource lob

foreign import ccall unsafe "dpiLob_openResource"
  dpiLob_openResource
    :: Ptr DPILob
    -- ^ dpiLob *
    -> IO CInt

releaseLOB :: LOB -> IO ()
releaseLOB (LOB fptr) = do
  withForeignPtr fptr $ \lob -> do
    throwOracleError
      =<< dpiLob_release lob

foreign import ccall unsafe "dpiLob_release"
  dpiLob_release
    :: Ptr DPILob
    -- ^ dpiLob *
    -> IO CInt

-- offset starts at 1
readLOBBytes :: LOB -> Int64 -> Int64 -> IO BSLC.ByteString
readLOBBytes (LOB fptr) offset maxAmount = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \valPtr -> do
      throwOracleError
        =<< dpiLob_readBytes
          lob
          (fromIntegral offset)
          (fromIntegral maxAmount)
          valPtr
          nullPtr -- here string length will come but we don't need it
      BSLC.pack <$> peekCString valPtr

-- offset starts at 1
readLOB :: LOB -> Int64 -> Int64 -> IO String
readLOB (LOB fptr) offset maxAmount = do
  withForeignPtr fptr $ \lob -> do
    alloca $ \valPtr -> do
      throwOracleError
        =<< dpiLob_readBytes
          lob
          (fromIntegral offset)
          (fromIntegral maxAmount)
          valPtr
          nullPtr -- here string length will come but we don't need it
      peekCString valPtr

foreign import ccall unsafe "dpiLob_readBytes"
  dpiLob_readBytes
    :: Ptr DPILob
    -- ^ dpiLob *
    -> CULong
    -- ^  uint64_t offset
    -> CULong
    -- ^  uint64_t amount
    -> CString
    -- ^  char *value
    -> Ptr CULong
    -- ^  uint64_t *valueLength
    -> IO CInt

-- offset starts at 1
{-
WARNING: for historical reasons, Oracle stores CLOBs and NCLOBs using the UTF-16 encoding, regardless of what encoding is otherwise in use by the database. The number of characters, however, is defined by the number of UCS-2 codepoints. For this reason, if a character requires more than one UCS-2 codepoint, care must be taken to account for them in the offset parameter.
-}
writeLOB :: LOB -> Int64 -> BSLC.ByteString -> IO ()
writeLOB (LOB fptr) offset val = do
  withForeignPtr fptr $ \lob -> do
    valPtr <- newCString (BSLC.unpack val)
    let valPtrLen = BSLC.length val
    throwOracleError
      =<< dpiLob_writeBytes
        lob
        (fromIntegral offset)
        valPtr
        (fromIntegral valPtrLen)

foreign import ccall unsafe "dpiLob_writeBytes"
  dpiLob_writeBytes
    :: Ptr DPILob
    -- ^ dpiLob *
    -> CULong
    -- ^  uint64_t offset
    -> CString
    -- ^  char *value
    -> CULong
    -- ^  uint64_t valueLength
    -> IO CInt

setLOBVal :: LOB -> String -> IO ()
setLOBVal (LOB fptr) val = do
  withForeignPtr fptr $ \lob -> do
    withCStringLen val $ \(valPtr, valPtrLen) -> do
      throwOracleError
        =<< dpiLob_setFromBytes lob valPtr (fromIntegral valPtrLen)

foreign import ccall unsafe "dpiLob_setFromBytes"
  dpiLob_setFromBytes
    :: Ptr DPILob
    -- ^ dpiLob *
    -> CString
    -- ^ const char *value
    -> CULong
    -- ^ uint64_t value
    -> IO CInt

trimLOBVal :: LOB -> Int64 -> IO ()
trimLOBVal (LOB fptr) newSize = do
  withForeignPtr fptr $ \lob -> do
    throwOracleError
      =<< dpiLob_trim lob (fromIntegral newSize)

foreign import ccall unsafe "dpiLob_trim"
  dpiLob_trim
    :: Ptr DPILob
    -> CULong
    -- ^ uint64_t newSize
    -> IO CInt
