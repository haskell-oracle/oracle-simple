{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.IO.Class    (liftIO)
import Data.AEq
import Data.Fixed
import Data.Function
import Data.Int
import Data.Text                 (Text)
import Data.Time
import Foreign.C.Types
import GHC.Generics
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck           hiding ((===))
import Test.QuickCheck.Instances ()

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
        property $ \(UTCTimeNanos utc) -> do
          utc `shouldBe` dpiTimeStampToUTCTime (utcTimeToDPITimestamp utc)

    describe "Roundtrip tests" $ do
      it "Should round trip random values from a table" $ \conn -> do
        property $ \expected@TestTable{..} -> do
          execute_ conn "create table test (a varchar(300), b number (12,0), c number (12,0), d number (12,0) null, e timestamp (9), f number (38,28))"
          execute conn "insert into test values (:1,:2,:3,:4,:5,:6)" expected
          actual <- query_ conn "select * from test"
          execute_ conn "drop table test"
          actual `shouldBe` [expected]

data TestTable
  = TestTable
  { fieldText :: Text
  , fieldInt :: Int
  , fieldInt64 :: Int64
  , fieldMaybeInt :: Maybe Int
  , fieldUTCTime :: UTCTimeNanos
  , fieldDouble :: Double
  } deriving stock (Generic, Show)
    deriving anyclass (ToRow, FromRow)

instance Eq TestTable where
  x == y =
    and
    [ ((==) `on` fieldText) x y
    , ((==) `on` fieldInt) x y
    , ((==) `on` fieldInt64) x y
    , ((==) `on` fieldMaybeInt) x y
    , ((==) `on` fieldUTCTime) x y
    , ((~==) `on` fieldDouble) x y
    ]

instance Arbitrary TestTable where
  arbitrary =
    TestTable
      <$> arbitrary
      <*> arbitrary -- choose (- 2^12, 2 ^ 12)
      <*> arbitrary -- choose (- 2^12, 2 ^ 12)
      <*> arbitrary -- oneof [ Just <$> choose (- 2^12, 2 ^ 12), pure Nothing ]
      <*> arbitrary
      <*> arbitrary

newtype UTCTimeNanos = UTCTimeNanos UTCTime
  deriving stock (Eq)
  deriving newtype (Show, FromField, ToField, HasDPINativeType)

instance Arbitrary UTCTimeNanos where
  arbitrary = do
    tod <- arbitrary
    day <- arbitrary
    nanos :: Nano <- arbitrary
    let utcTime = UTCTime day $ timeOfDayToTime tod { todSec = realToFrac nanos }
    pure (UTCTimeNanos utcTime)
