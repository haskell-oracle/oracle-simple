{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Oracle.Simple.Transaction
  ( beginTransaction
  , commitTransaction
  , prepareCommit
  , withTransaction
  , commitIfNeeded
  , withSavepoint
  ) where

import Control.Exception (catch, throw)
import Control.Monad ((<=<), replicateM, void, when)
import Data.List (unfoldr)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)
import Foreign (alloca, peek, poke, withForeignPtr)
import Foreign.C.String (CString, withCStringLen)
import Foreign.C.Types (CInt (CInt), CLong, CUInt (CUInt))
import Foreign.Ptr (Ptr)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import System.Random (getStdRandom, uniformR)

import Database.Oracle.Simple.Execute (execute_)
import Database.Oracle.Simple.Internal
  ( Connection (Connection)
  , DPIConn
  , OracleError
  , throwOracleError
  )

-- * Transactions

-- | Execute an action in an SQL transaction.
--
-- If the action succeeds, the transaction will be completed with commit before this function returns.
-- If the action throws any kind of exception, the transaction is rolled back and the exception will be rethrown.
--
-- Nesting transactions may result in undefined behavior. For /nesting-like/ functionality, use 'withSavepoint'.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action = do
  txHandle <- beginTransaction conn
  result <-
    action
      `catch` (\(e :: OracleError) -> rollbackTransaction conn txHandle >> throw e)
  commitIfNeeded conn txHandle
  pure result

-- ** Low-level transaction interface

data Transaction = Transaction
  { transactionId :: UUID
  , branchQualifier :: UUID
  }

-- | Begin a new transaction.
beginTransaction :: Connection -> IO Transaction
beginTransaction (Connection fptr) =
  withForeignPtr fptr $ \conn -> do
    transactionId <- nextRandom
    branchQualifier <- nextRandom
    let dpiTransaction = Transaction{..}
    withDPIXid dpiTransaction $ \dpiXid ->
      throwOracleError =<< dpiConn_tpcBegin conn dpiXid 0 0x00000001
    pure dpiTransaction

foreign import ccall unsafe "dpiConn_tpcBegin"
  dpiConn_tpcBegin
    :: Ptr DPIConn
    -> Ptr DPIXid
    -> CUInt
    -> CUInt
    -> IO CInt

-- | Prepare transaction for commit. Returns whether the transaction needs to be committed.
-- Attempting a commit if this function returns False may cause an exception.
--
-- Use 'commitIfNeeded' to safely commit a transaction.
prepareCommit :: Connection -> Transaction -> IO Bool
prepareCommit (Connection fptr) dpiTransaction =
  withForeignPtr fptr $ \conn ->
    withDPIXid dpiTransaction $ \dpiXid ->
      alloca $ \commitNeededPtr -> do
        throwOracleError =<< dpiConn_tpcPrepare conn dpiXid commitNeededPtr
        (== 1) <$> peek commitNeededPtr

foreign import ccall unsafe "dpiConn_tpcPrepare"
  dpiConn_tpcPrepare
    :: Ptr DPIConn
    -> Ptr DPIXid
    -> Ptr CInt
    -> IO CInt

-- | Commit a transaction.
-- Throws an exception if a commit was not necessary.
-- Whether a commit is necessary can be checked by 'prepareCommit'.
commitTransaction :: Connection -> Transaction -> IO ()
commitTransaction (Connection fptr) dpiTransaction =
  withForeignPtr fptr $ \conn ->
    withDPIXid dpiTransaction $ \dpiXid ->
      throwOracleError =<< dpiConn_tpcCommit conn dpiXid 0

foreign import ccall unsafe "dpiConn_tpcCommit"
  dpiConn_tpcCommit
    :: Ptr DPIConn
    -> Ptr DPIXid
    -> CInt
    -> IO CInt

-- | Roll back a transaction.
rollbackTransaction :: Connection -> Transaction -> IO ()
rollbackTransaction (Connection fptr) dpiTransaction =
  withForeignPtr fptr $ \conn ->
    withDPIXid dpiTransaction $ throwOracleError <=< dpiConn_tpcRollback conn

foreign import ccall unsafe "dpiConn_tpcRollback"
  dpiConn_tpcRollback
    :: Ptr DPIConn
    -> Ptr DPIXid
    -> IO CInt

-- | Commit a transaction, if needed.
commitIfNeeded :: Connection -> Transaction -> IO ()
commitIfNeeded conn dpiTransaction = do
  commitNeeded <- prepareCommit conn dpiTransaction
  when commitNeeded $ commitTransaction conn dpiTransaction

data DPIXid = DPIXid
  { dpixFormatId :: CLong
  , dpixGlobalTransactionId :: CString
  , dpixGlobalTransactionIdLength :: CUInt
  , dpixBranchQualifier :: CString
  , dpixBranchQualifierLength :: CUInt
  }
  deriving (Generic, Show)
  deriving anyclass (GStorable)

withDPIXid :: Transaction -> (Ptr DPIXid -> IO a) -> IO a
withDPIXid Transaction{..} action =
  withCStringLen (toString transactionId) $ \(dpixGlobalTransactionId, fromIntegral -> dpixGlobalTransactionIdLength) ->
    withCStringLen (toString branchQualifier) $ \(dpixBranchQualifier, fromIntegral -> dpixBranchQualifierLength) ->
      let dpixFormatId = 115 -- chosen at our discretion, can be anything but 0
       in alloca $ \dpiPtr -> poke dpiPtr DPIXid{..} >> action dpiPtr

-- * Savepoints

-- | Create a savepoint, and roll back to it if an error occurs. This should only be used within a transaction.
--
-- Savepoints may be nested.
withSavepoint :: Connection -> IO a -> IO a
withSavepoint conn action = do
  savepoint <- newSavepoint conn
  action
    `catch` (\(e :: OracleError) -> rollbackToSavepoint conn savepoint >> throw e)

-- ** Low-level savepoint interface

newtype Savepoint = Savepoint String
  deriving newtype (Show)

-- | Create a new savepoint. This should only be used within a transaction.
newSavepoint :: Connection -> IO Savepoint
newSavepoint conn = do
  name <- genSavepointName
  _ <- execute_ conn ("savepoint " <> name)
  pure $ Savepoint name
 where
  genSavepointName = replicateM 8 (getStdRandom $ uniformR ('a', 'z'))

-- | Roll back to a savepoint.
rollbackToSavepoint :: Connection -> Savepoint -> IO ()
rollbackToSavepoint conn (Savepoint name) = void $ execute_ conn ("rollback to savepoint " <> name)
