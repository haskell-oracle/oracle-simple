{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Database.Oracle.Simple.LOB
--
-- Implementation of handler functions for oracle's large objects (CLOB, BLOB, NCLOB, BFILE)
module Database.Oracle.Simple.LOB
    ( LOBType (..)
    , LOBField (..)
    , withLOB
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
    , dpiLobToByteString
    ) where

import Control.Monad (when, (<=<))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Coerce (coerce)
import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal
import Foreign
import Foreign.C.String
import Foreign.C.Types (CInt (..), CUInt (..), CULong (..))

data LOBType = CLOB | NCLOB | BLOB
    deriving (Show, Eq)

newtype LOBField = LOBField {unLOBField :: BSLC.ByteString}
    deriving (Show, Eq)

instance FromField LOBField where
    fromDPINativeType _ = DPI_NATIVE_TYPE_LOB
    fromField = coerce $ FieldParser (dpiLobToByteString <=< getLOB_)

dpiLobToByteString :: DPILob -> IO BSLC.ByteString
dpiLobToByteString lob = do
    lobSize <- getLOBSize lob
    chunkSize <- getLOBChunkSize lob
    readInChunks lob (fromIntegral chunkSize) 1 lobSize

readInChunks :: DPILob -> Int64 -> Int64 -> Int64 -> IO BSLC.ByteString
readInChunks lob chunkSize offset remaining
    | remaining <= 0 = return BSLC.empty
    | otherwise = do
        res <- readLOBBytes lob offset chunkSize
        let sizeRead = BSLC.length res
        when (sizeRead == 0) $ closeLOB lob
        rest <- readInChunks lob chunkSize (offset + sizeRead) (remaining - sizeRead)
        return $ res `BSLC.append` rest

lobTypeToCUInt :: LOBType -> CUInt
lobTypeToCUInt CLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_CLOB
lobTypeToCUInt NCLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_NCLOB
lobTypeToCUInt BLOB = dpiOracleTypeToUInt DPI_ORACLE_TYPE_BLOB

cuintToLobType :: CUInt -> Maybe LOBType
cuintToLobType 2018 = Just NCLOB -- DPI_ORACLE_TYPE_NCLOB
cuintToLobType 2017 = Just CLOB -- DPI_ORACLE_TYPE_CLOB
cuintToLobType 2019 = Just BLOB -- DPI_ORACLE_TYPE_BLOB
cuintToLobType _ = Nothing

--x Higher level convenience functions x--
withLOB :: Connection -> LOBType -> (DPILob -> IO ()) -> IO ()
withLOB conn lobType func = do
  lob <- genLOB conn lobType
  func lob
  closeLOB lob

genLOB :: Connection -> LOBType -> IO DPILob
genLOB (Connection fptr) lobType = do
    withForeignPtr fptr $ \conn -> do
        alloca $ \dpiLobPtr -> do
            throwOracleError =<< dpiConn_newTempLob conn (lobTypeToCUInt lobType) dpiLobPtr
            peek dpiLobPtr

foreign import ccall unsafe "dpiConn_newTempLob"
    dpiConn_newTempLob
        :: Ptr DPIConn
        -- ^ dpiConn *
        -> CUInt
        -- ^ dpiOracleTypeNum
        -> Ptr DPILob
        -- ^ dpiLob **
        -> IO CInt

closeLOB :: DPILob -> IO ()
closeLOB lob = throwOracleError =<< dpiLob_close lob

closeLOBResource :: DPILob -> IO ()
closeLOBResource lob = throwOracleError =<< dpiLob_closeResource lob

foreign import ccall unsafe "dpiLob_close"
    dpiLob_close
        :: DPILob
        -- ^ dpiLob *
        -> IO CInt

foreign import ccall unsafe "dpiLob_closeResource"
    dpiLob_closeResource
        :: DPILob
        -- ^ dpiLob *
        -> IO CInt

copyLOB :: DPILob -> IO DPILob
copyLOB lob = do
    alloca $ \dpiLobPtr -> do
        throwOracleError =<< dpiLob_copy lob dpiLobPtr
        peek dpiLobPtr

foreign import ccall unsafe "dpiLob_copy"
    dpiLob_copy
        :: DPILob
        -- ^ dpiLob *
        -> Ptr DPILob
        -- ^ dpiLob **
        -> IO CInt

getLOBBufferSize :: DPILob -> Int64 -> IO Int64
getLOBBufferSize lob sizeInChars = do
    alloca $ \sizeInBytesPtr -> do
        throwOracleError
            =<< dpiLob_getBufferSize lob (CULong $ fromIntegral sizeInChars) sizeInBytesPtr
        fromIntegral <$> peek sizeInBytesPtr

foreign import ccall unsafe "dpiLob_getBufferSize"
    dpiLob_getBufferSize
        :: DPILob
        -- ^ dpiLob *
        -> CULong
        -- ^ sizeInChars
        -> Ptr CULong
        -- ^ sizeInBytes
        -> IO CInt

getLOBChunkSize :: DPILob -> IO Int
getLOBChunkSize lob = do
    alloca $ \sizePtr -> do
        throwOracleError
            =<< dpiLob_getChunkSize lob sizePtr
        fromIntegral <$> peek sizePtr

foreign import ccall unsafe "dpiLob_getChunkSize"
    dpiLob_getChunkSize
        :: DPILob
        -- ^ dpiLob *
        -> Ptr CUInt
        -- ^ uint32_t size
        -> IO CInt

getLOBDirectoryAndFile :: DPILob -> IO (String, String)
getLOBDirectoryAndFile lob = do
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
        :: DPILob
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

setLOBDirectoryAndFile :: DPILob -> String -> String -> IO ()
setLOBDirectoryAndFile lob directoryName fileName = do
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
        :: DPILob
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

doesLOBFileExists :: DPILob -> IO Bool
doesLOBFileExists lob = do
    alloca $ \isOpenPtr -> do
        throwOracleError
            =<< dpiLob_getFileExists lob isOpenPtr
        r <- peek isOpenPtr
        if r == 0 then return False else return True

foreign import ccall unsafe "dpiLob_getFileExists"
    dpiLob_getFileExists
        :: DPILob
        -- ^ dpiLob *
        -> Ptr CInt
        -- ^ int *isOpen
        -> IO CInt

isLOBResoureOpen :: DPILob -> IO Bool
isLOBResoureOpen lob = do
    alloca $ \isOpenPtr -> do
        throwOracleError
            =<< dpiLob_getIsResourceOpen lob isOpenPtr
        r <- peek isOpenPtr
        if r == 0 then return False else return True

foreign import ccall unsafe "dpiLob_getIsResourceOpen"
    dpiLob_getIsResourceOpen
        :: DPILob
        -- ^ dpiLob *
        -> Ptr CInt
        -- ^ int *isOpen
        -> IO CInt

{-
WARNING: for historical reasons, Oracle stores CLOBs and NCLOBs using the UTF-16 encoding, regardless of what encoding is otherwise in use by the database. The number of characters, however, is defined by the number of UCS-2 codepoints. For this reason, if a character requires more than one UCS-2 codepoint, the size returned will be inaccurate and care must be taken to account for the difference.
-}
getLOBSize :: DPILob -> IO Int64
getLOBSize lob = do
    alloca $ \sizePtr -> do
        throwOracleError
            =<< dpiLob_getSize lob sizePtr
        fromIntegral <$> peek sizePtr

foreign import ccall unsafe "dpiLob_getSize"
    dpiLob_getSize
        :: DPILob
        -- ^ dpiLob *
        -> Ptr CULong
        -- ^ uint64_t *size
        -> IO CInt

getLOBType :: DPILob -> IO LOBType
getLOBType lob = do
    alloca $ \numTypePtr -> do
        throwOracleError
            =<< dpiLob_getType lob numTypePtr
        mType <- cuintToLobType <$> peek numTypePtr
        case mType of
            Nothing -> return BLOB -- kind of impossible case
            Just r -> return r

foreign import ccall unsafe "dpiLob_getType"
    dpiLob_getType
        :: DPILob
        -- ^ dpiLob *
        -> Ptr CUInt
        -- ^ dpiOracleTypeNum numType
        -> IO CInt

openLOBResource :: DPILob -> IO ()
openLOBResource lob = do
    throwOracleError
        =<< dpiLob_openResource lob

foreign import ccall unsafe "dpiLob_openResource"
    dpiLob_openResource
        :: DPILob
        -- ^ dpiLob *
        -> IO CInt

releaseLOB :: DPILob -> IO ()
releaseLOB lob = do
    throwOracleError
        =<< dpiLob_release lob

foreign import ccall unsafe "dpiLob_release"
    dpiLob_release
        :: DPILob
        -- ^ dpiLob *
        -> IO CInt

-- offset starts at 1
readLOBBytes :: DPILob -> Int64 -> Int64 -> IO BSLC.ByteString
readLOBBytes lob offset maxAmount = do
    valPtr <- callocBytes (fromIntegral maxAmount)
    alloca $ \valLenPtr -> do
        poke valLenPtr (fromIntegral maxAmount)
        throwOracleError
            =<< dpiLob_readBytes
                lob
                (fromIntegral offset)
                (fromIntegral maxAmount)
                valPtr
                valLenPtr
        res <- BSLC.pack <$> peekCString valPtr
        free valPtr
        return res

-- offset starts at 1
readLOB :: DPILob -> Int64 -> Int64 -> IO String
readLOB lob offset maxAmount = do
    valPtr <- callocBytes (fromIntegral maxAmount)
    alloca $ \valLenPtr -> do
        poke valLenPtr (fromIntegral maxAmount)
        throwOracleError
            =<< dpiLob_readBytes
                lob
                (fromIntegral offset)
                (fromIntegral maxAmount)
                valPtr
                valLenPtr
    res <- peekCString valPtr
    free valPtr
    return res

foreign import ccall unsafe "dpiLob_readBytes"
    dpiLob_readBytes
        :: DPILob
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
{- | 
 e.g
ghci> :set -XOverloadedStrings
ghci> writeLOB lob 1 "Hello"
ghci> dpiLobToByteString lob
"Hello"
ghci> writeLOB lob 6 " World!"
ghci> dpiLobToByteString lob
"Hello World!"
-}
writeLOB :: DPILob -> Int64 -> BSLC.ByteString -> IO ()
writeLOB lob offset val = do
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
        :: DPILob
        -- ^ dpiLob *
        -> CULong
        -- ^  uint64_t offset
        -> CString
        -- ^  char *value
        -> CULong
        -- ^  uint64_t valueLength
        -> IO CInt

setLOBVal :: DPILob -> String -> IO ()
setLOBVal lob val = do
    withCStringLen val $ \(valPtr, valPtrLen) -> do
        throwOracleError
            =<< dpiLob_setFromBytes lob valPtr (fromIntegral valPtrLen)

foreign import ccall unsafe "dpiLob_setFromBytes"
    dpiLob_setFromBytes
        :: DPILob
        -- ^ dpiLob *
        -> CString
        -- ^ const char *value
        -> CULong
        -- ^ uint64_t value
        -> IO CInt

trimLOBVal :: DPILob -> Int64 -> IO ()
trimLOBVal lob newSize = do
    throwOracleError
        =<< dpiLob_trim lob (fromIntegral newSize)

foreign import ccall unsafe "dpiLob_trim"
    dpiLob_trim
        :: DPILob
        -> CULong
        -- ^ uint64_t newSize
        -> IO CInt
