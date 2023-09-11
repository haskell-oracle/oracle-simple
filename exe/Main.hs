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
  foo

foreign import ccall safe "example" example :: IO ()

foo :: IO ()
foo = do
  major <- getMajorVersion
  minor <- getMinorVersion
  print ("version -> " :: String, major, minor)
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn "select sysdate from dual"
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    (typ, dataPtr) <- getQueryValue stmt 1
    print typ
    print =<< peek =<< dpiData_getTimestamp dataPtr
