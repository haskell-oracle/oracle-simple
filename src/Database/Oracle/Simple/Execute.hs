{-# LANGUAGE BangPatterns #-}
module Database.Oracle.Simple.Execute where

import Control.Monad (foldM)
import Control.Monad.State.Strict (evalStateT)
import Data.Word (Word64)
import Database.Oracle.Simple.ToRow (ToRow, RowWriter(runRowWriter), toRow)
import Database.Oracle.Simple.Internal (DPIConn, prepareStmt, Column(Column), dpiExecute, DPIModeExec(DPI_MODE_EXEC_COMMIT_ON_SUCCESS), getRowCount)

-- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results.
-- Returns the number of rows affected.
execute :: (ToRow a) => DPIConn -> String -> a -> IO Word64
execute conn sql param = do
  stmt <- prepareStmt conn sql
  _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
  dpiExecute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS
  rowsAffected <- getRowCount stmt
  pure rowsAffected

-- | A version of 'execute' that does not perform query substitution.
execute_ :: DPIConn -> String -> IO Word64
execute_ conn sql = do
  stmt <- prepareStmt conn sql
  dpiExecute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS
  rowsAffected <- getRowCount stmt
  pure rowsAffected

-- | Execute a multi-row INSERT, UPDATE or other SQL query that is not expected to return results.
-- Returns the number of rows affected. If the list of parameters is empty, the function will simply
-- return 0 without issuing the query to the backend.
executeMany :: (ToRow a) => DPIConn -> String -> [a] -> IO Word64
executeMany coon sql [] = pure 0
executeMany conn sql params = do
  stmt <- prepareStmt conn sql
  foldM (go stmt) 0 params
    where
      go stmt !totalRowsAffected param = do
        _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
        dpiExecute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS
        rowsAffected <- getRowCount stmt
        pure (totalRowsAffected + rowsAffected)
