{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Fixed
import Data.Time
import Data.Word
import Foreign.C.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Database.Oracle.Simple

foo :: IO [Only (Maybe Word64)]
foo = withConnection params $ \conn ->
  query_ conn "select sample_integer from sample_table"

main :: IO ()
main = hspec spec

params :: ConnectionParams
params = ConnectionParams "username" "password" "localhost/XEPDB1"

spec :: Spec
spec = do
  around (withConnection params) $ do
    describe "SELECT tests" $ do
      it "Should select timestamp from Oracle" $ \conn -> do
        currentDay <- utctDay <$> getCurrentTime
        [Only DPITimestamp{..}] <- query_ conn "select sysdate from dual"
        currentDay
          `shouldBe` fromGregorian
            (fromIntegral year)
            (fromIntegral month)
            (fromIntegral day)

    describe "Connection tests" $ do
      it "Should check connetion health" $ \conn ->
        (`shouldBe` True) =<< isHealthy conn

    describe "DPITimeStamp tests" $ do
      it "Should roundtrip DPITimestamp through UTCTime" $ \conn -> do
        property $ \dpiTimestamp -> do
          utcTimeToDPITimestamp (dpiTimeStampToUTCTime dpiTimestamp)
            `shouldBe` dpiTimeStampToUTCDPITimeStamp dpiTimestamp

      it "Idempotency of dpiTimeStampToUTCDPITimeStamp " $ \conn -> do
        property $ \dpi -> do
          dpiTimeStampToUTCDPITimeStamp (dpiTimeStampToUTCDPITimeStamp dpi)
            `shouldBe` dpiTimeStampToUTCDPITimeStamp dpi

      it "YYYY/MM/DD should be affected by UTC offset changes" $ \_ -> do
        let dpi =
              DPITimestamp
                { year = 1000
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
              DPITimestamp
                { year = 999
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
          let utc = UTCTime day $ timeOfDayToTime tod{todSec = realToFrac nanos}
          utc `shouldBe` dpiTimeStampToUTCTime (utcTimeToDPITimestamp utc)
