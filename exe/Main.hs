{-# LANGUAGE CPP  #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Database.Oracle.Simple
import Foreign.Storable

main :: IO ()
main = do
  -- example
  -- putStrLn "haskell time"
  -- print $ sizeOf (undefined :: DPITimeStamp)
  testTimestamp
  testDouble

foreign import ccall safe "example" example :: IO ()

testTimestamp :: IO ()
testTimestamp = do
  let stmt = "select sysdate from dual"
  putStrLn stmt
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmt
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    (typ, dataPtr) <- getQueryValue stmt 1
    print typ
    print =<< peek =<< dpiData_getTimestamp dataPtr

testDouble :: IO ()
testDouble = do
  let stmt = "select count(*) from dual"
  putStrLn stmt
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmt
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    (typ, dataPtr) <- getQueryValue stmt 1
    print typ
    print =<< dpiData_getDouble dataPtr
