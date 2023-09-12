{-# LANGUAGE CPP  #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Database.Oracle.Simple
import Foreign.Storable
import Foreign.C.Types

main :: IO ()
main = foo

foreign import ccall safe "example" example :: IO ()

newtype RowCount = RowCount { getRowCount :: CDouble }
  deriving Show

instance FromField RowCount where
  fromField = fmap RowCount . fromField

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  } deriving Show

instance FromRow ReturnedRow where
  fromRow =
    ReturnedRow
      <$> (fromField $ Position 1)
      <*> (fromField $ Position 2)

foo :: IO ()
foo = do
  let stmt = "select count(*), sysdate from dual"
  putStrLn stmt
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmt
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    tsVal <- getRow @ReturnedRow stmt
    print tsVal
