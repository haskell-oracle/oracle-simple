{-# LANGUAGE CPP  #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Database.Oracle.Simple
import Foreign.Storable

main :: IO ()
main = do
  example
  -- putStrLn "haskell time"
  -- print $ sizeOf (undefined :: DPITimeStamp)
  -- foo

foreign import ccall safe "example" example :: IO ()

foo :: IO ()
foo = do
  putStrLn "1"
  conn <- createConn (ConnectionParams "j222933" "OoEHstuj" "wpx4-scan.heb.com:1521/pdb2om1_rw")
  putStrLn "2"
  stmt <- prepareStmt conn "select sysdate from dual"
  putStrLn "3"
  execute stmt DPI_MODE_EXEC_DEFAULT
  putStrLn "4"
  found <- fetch stmt
  putStrLn "5"
  unless (found == 0) $ do
    putStrLn "6"
    (typ, dataPtr) <- getQueryValue stmt 1
    putStrLn "7"
    print typ
    putStrLn "8"
    print =<< peek =<< dpiData_getTimestamp dataPtr
    putStrLn "9"
