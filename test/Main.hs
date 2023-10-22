{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Data.Aeson
import Foreign.C.Types
import Data.Fixed
import Control.Monad.IO.Class (liftIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Time
import Test.Hspec
import GHC.Generics
import Data.Aeson

import Database.Oracle.Simple

data SumType = This | That
  deriving (Generic, Eq, Show)

instance ToJSON SumType
instance FromJSON SumType

data JsonData =
  JsonData
  { string :: String
  , number :: Int
  , bool :: Bool
  , maybeBool :: Maybe Bool 
  , stringList :: [String]
  , sumType :: SumType
  , double :: Double
  } deriving (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON) 

main :: IO ()
main = withPool params $ hspec . spec

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

    describe "JSON tests" $ do
      it "should roundtrip JSON data" $ \conn -> do
        _ <- execute_ conn "create table json_test(test_column json)"
        let jsonData = JsonData "str" 123 True Nothing ["hello", "world"] That 3.14
        _ <- execute conn "insert into json_test values (:1)" (Only jsonData)
        [Only gotData] <- query_ conn "select * from json_test"
        _ <- execute_ conn "drop table json_test"
        gotData `shouldBe` jsonData
