module Database.Oracle.Simple.Query where

import Control.Monad.State.Strict (evalStateT)
import GHC.Generics (Generic)

import Database.Oracle.Simple.FromField (FromField)
import Database.Oracle.Simple.FromRow (FromRow, getRow)
import Database.Oracle.Simple.Internal
  ( Column (Column)
  , Connection
  , DPIModeExec (DPI_MODE_EXEC_DEFAULT)
  , dpiExecute
  , fetch
  , prepareStmt
  )
import Database.Oracle.Simple.ToField (ToField)
import Database.Oracle.Simple.ToRow (RowWriter (runRowWriter), ToRow, toRow)

-- | Perform a SELECT or other SQL query that is expected to return results.
-- All results are retrieved and converted before this function ends.
query :: (FromRow a, ToRow b) => Connection -> String -> b -> IO [a]
query conn sql param = do
  stmt <- prepareStmt conn sql
  _ <- evalStateT (runRowWriter (toRow param) stmt) (Column 0)
  dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt found
 where
  loop stmt n | n < 1 = pure []
  loop stmt _ = do
    tsVal <- getRow stmt
    found <- fetch stmt
    (tsVal :) <$> loop stmt found

-- | A version of 'query' that does not perform query substitution.
query_ :: FromRow a => Connection -> String -> IO [a]
query_ conn sql = do
  stmt <- prepareStmt conn sql
  dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt found
 where
  loop stmt n | n < 1 = pure []
  loop stmt _ = do
    tsVal <- getRow stmt
    found <- fetch stmt
    (tsVal :) <$> loop stmt found

-- | Incrementally process a query
forEach_ :: FromRow row => Connection -> String -> (row -> IO ()) -> IO ()
forEach_ conn sql cont = do
  stmt <- prepareStmt conn sql
  dpiExecute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  loop stmt found
    where
      loop stmt n | n < 1 = pure ()
      loop stmt _ = do
        tsVal <- getRow stmt
        cont tsVal
        found <- fetch stmt
        loop stmt found
