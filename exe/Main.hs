{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.Int
import Data.Text
import Database.Oracle.Simple
import Foreign.C.Types
import Foreign.Storable

main :: IO ()
main = foo

foreign import ccall safe "example" example :: IO ()

newtype RowCount = RowCount {getRowCount :: Int64}
  deriving (Show)

instance FromField RowCount where
  fromField = RowCount <$> fromField

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  , amount :: Double
  }
  deriving (Show)

instance FromRow ReturnedRow where
  fromRow = do
    count <- field (Column 1)
    sysdate <- field (Column 2)
    hint <- field @Text (Column 3)
    amount <-
      if hint == "ignore next column"
        then field (Column 5)
        else field (Column 4)
    pure $ ReturnedRow{..}

foo :: IO ()
foo = do
  let stmtStr = "select count(*), sysdate, 'ignore next column', 125.24, 3.14 from dual"
  putStrLn stmtStr
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmtStr
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    tsVal <- getRow @ReturnedRow stmt
    print tsVal
