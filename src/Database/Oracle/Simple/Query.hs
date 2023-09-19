module Database.Oracle.Simple.Query where

import GHC.Generics (Generic)
import Control.Monad.State.Strict (evalStateT)
import Database.Oracle.Simple.ToField (ToField)
import Database.Oracle.Simple.FromField (FromField)
import Database.Oracle.Simple.ToRow (ToRow, RowWriter(..), toRow)
import Database.Oracle.Simple.FromRow (FromRow, getRow)
import Database.Oracle.Simple.Internal (Connection, prepareStmt, dpiExecute, DPIModeExec(..), fetch, Column(..))

-- | Perform a SELECT or other SQL query that is expected to return results.
-- All results are retrieved and converted before this function ends.
query :: (FromRow a, ToRow b) => Connection -> String -> b -> IO [a]
query conn sql param = do
  stmt <- prepareStmt conn sql
  _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
  dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt [] found
 where
  loop stmt xs n | n < 1 = pure xs
  loop stmt xs _ = do
    tsVal <- getRow stmt
    found <- fetch stmt
    loop stmt (xs ++ [tsVal]) found

-- | A version of 'query' that does not perform query substitution.
query_ :: FromRow a => Connection -> String -> IO [a]
query_ conn sql = do
  stmt <- prepareStmt conn sql
  dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt [] found
 where
  loop stmt xs n | n < 1 = pure xs
  loop stmt xs _ = do
    tsVal <- getRow stmt
    found <- fetch stmt
    loop stmt (xs ++ [tsVal]) found
