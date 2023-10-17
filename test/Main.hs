{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foreign hiding (withPool, Pool)
import Foreign.C.Types
import Data.Fixed
import Control.Monad.IO.Class (liftIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Time
import Test.Hspec
import Foreign.C

import Database.Oracle.Simple

foreign import ccall "run_json_demo"
  run_json_demo :: Ptr DPIConn -> Ptr CString -> IO CInt

runJsonDemo :: Connection -> IO ()
runJsonDemo (Connection fptr) = do
  withForeignPtr fptr $ \conn -> do
    alloca $ \sPtrPtr -> do
      run_json_demo conn sPtrPtr
      sPtr <- peek sPtrPtr
      str <- peekCString sPtr
      free sPtr
      putStrLn str
      pure ()

main :: IO ()
main = withPool params $ \pool -> withPoolConnection pool $ \conn -> do 
  runJsonDemo conn

params :: ConnectionParams
params = ConnectionParams "username" "password" "localhost/devdb"

spec :: Pool -> Spec
spec pool = do
  around (withPoolConnection pool) $ do
    describe "SELECT tests" $ do
      it "Should select timestamp from Oracle" $ \conn -> do
        currentDay <- utctDay <$> getCurrentTime
        [Only DPITimestamp {..}] <- query_ conn "select sysdate from dual"
        currentDay `shouldBe`
          fromGregorian
            (fromIntegral year)
            (fromIntegral month)
            (fromIntegral day)

    describe "Connection tests" $ do
      it "Should check connetion health" $ \conn ->
        (`shouldBe` True) =<< isHealthy conn

    describe "DPITimeStamp tests" $ do
      it "Should roundtrip DPITimestamp through UTCTime" $ \_ -> do
        property $ \dpiTimestamp -> do
          utcTimeToDPITimestamp (dpiTimeStampToUTCTime dpiTimestamp)
            `shouldBe` dpiTimeStampToUTCDPITimeStamp dpiTimestamp

      it "Idempotency of dpiTimeStampToUTCDPITimeStamp " $ \_ -> do
        property $ \dpi -> do
          dpiTimeStampToUTCDPITimeStamp (dpiTimeStampToUTCDPITimeStamp dpi)
            `shouldBe` dpiTimeStampToUTCDPITimeStamp dpi

      it "YYYY/MM/DD should be affected by UTC offset changes" $ \_ -> do
        let dpi =
              DPITimestamp { year = 1000
                           , month = 1
                           , day = 1
                           , hour = 0
                           , minute = 0
                           , second = 0
                           , fsecond = 0
                           , tzHourOffset = 0
                           , tzMinuteOffset = 1
                           }
        let expected =
              DPITimestamp { year = 999
                           , month = 12
                           , day = 31
                           , hour = 23
                           , minute = 59
                           , second = 0
                           , fsecond = 0
                           , tzHourOffset = 0
                           , tzMinuteOffset = 0
                           }
        dpiTimeStampToUTCDPITimeStamp dpi `shouldBe` expected

      it "Should roundtrip UTCTime through DPITimestamp (w/ nanos -- not picos) " $ \_ -> do
        property $ \tod day (nanos :: Nano) -> do
          let utc = UTCTime day $ timeOfDayToTime tod { todSec = realToFrac nanos }
          utc `shouldBe` dpiTimeStampToUTCTime (utcTimeToDPITimestamp utc)
