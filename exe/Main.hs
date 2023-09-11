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
  let stmt = "select sysdate, count(*) from dual"
  putStrLn stmt
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn stmt
  execute stmt DPI_MODE_EXEC_DEFAULT
  found <- fetch stmt
  unless (found == 0) $ do
    -- tsVal <- getQueryValue' stmt DPI_NATIVE_TYPE_TIMESTAMP 1
    tsVal <- runGetter getTimeAndCount stmt 1
    print tsVal
    -- countVal <- runGetter getDouble stmt 2
    -- countVal <- getQueryValue' stmt DPI_NATIVE_TYPE_DOUBLE 2
    -- print countVal
