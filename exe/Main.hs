{-# LANGUAGE CPP  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Database.Oracle.Simple
import Foreign.Storable
import Foreign.C.Types
import Data.Text
import Data.Int

main :: IO ()
main = foo

foreign import ccall safe "example" example :: IO ()

newtype RowCount = RowCount { getRowCount :: Int64 }
  deriving Show

instance FromField RowCount where
  fromField = RowCount <$> fromField

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  , message :: Text
  } deriving Show

instance FromRow ReturnedRow where
  fromRow = do
    count <- field
    sysdate <- field
    message <- field
    pure ReturnedRow {..}
  -- or:
  -- fromRow = ReturnedRow <$> field <*> field

foo :: IO ()
foo = do
  let stmt = "select count(*), sysdate, 'hello world' from dual"
  putStrLn stmt
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmt
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    tsVal <- getRow @ReturnedRow stmt
    print tsVal
