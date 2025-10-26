{-# LANGUAGE BangPatterns #-}

module Database.Oracle.Simple.Execute
  ( execute,
    execute_,
    executeMany,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (evalStateT)
import Data.Word (Word64)

import Database.Oracle.Simple.Internal
  ( Column (Column),
    Connection,
    DPIModeExec (DPI_MODE_EXEC_DEFAULT),
    dpiExecute,
    getRowCount,
    prepareStmt,
  )
import Database.Oracle.Simple.ToRow (RowWriter (runRowWriter), ToRow, toRow)

{- | Execute an INSERT, UPDATE, or other SQL query that is not expected to return results.
Returns the number of rows affected.
-}
execute :: (ToRow a) => Connection -> String -> a -> IO Word64
execute conn sql param = do
  stmt <- prepareStmt conn sql
  _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
  _ <- dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  getRowCount stmt

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> String -> IO Word64
execute_ conn sql = do
  stmt <- prepareStmt conn sql
  _ <- dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  getRowCount stmt

{- | Execute a multi-row INSERT, UPDATE or other SQL query that is not expected to return results.
Returns the number of rows affected. If the list of parameters is empty, the function will simply
return 0 without issuing the query to the backend.
-}
executeMany :: (ToRow a) => Connection -> String -> [a] -> IO Word64
executeMany _ _ [] = pure 0
executeMany conn sql params = do
  stmt <- prepareStmt conn sql
  foldM (go stmt) 0 params
  where
    go stmt !totalRowsAffected param = do
      _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
      _ <- dpiExecute stmt DPI_MODE_EXEC_DEFAULT
      rowsAffected <- getRowCount stmt
      pure (totalRowsAffected + rowsAffected)
