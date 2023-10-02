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
import Data.Int

import Database.Oracle.Simple

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

    describe "Arbitrary-precision numeric types" $ do
      it "should roundtrip maximum and minimum integral values that the database can hold" $ \conn -> do
        let maxVal :: Integer
            maxVal = 10^38 - 1 -- maxinum bound of NUMBER(38,0)
        let minVal :: Integer
            minVal = -(10^38 - 1) -- minimum bound of NUMBER(38,0)
        _ <- execute_ conn "create table integer_table(integer_value number(38,0))"
        _ <- executeMany conn "insert into integer_table values (:1)" [Only maxVal, Only minVal]
        [gotMaxVal, gotMinVal] <- fmap fromOnly <$> query_ conn "select * from integer_table"
        _ <- execute_ conn "drop table integer_table"
        [gotMaxVal, gotMinVal] `shouldBe` [maxVal, minVal]
