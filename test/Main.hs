{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Exception

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

    describe "transaction tests" $ do
      it "should commit transaction successfully" $ \conn -> do
        execute_ conn "create table transaction_test(text_column number(10,0) primary key)"
        withTransaction conn $ do
          execute conn "insert into transaction_test values(:1)" (Only @Int 1)
          execute conn "insert into transaction_test values(:1)" (Only @Int 2)
          execute conn "insert into transaction_test values(:1)" (Only @Int 3)
          execute conn "insert into transaction_test values(:1)" (Only @Int 4)
        results <- query_ @(Only Int) conn "select * from transaction_test"
        execute_ conn "drop table transaction_test"
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4]

      it "should roll back transaction in case of failure" $ \conn -> do
        execute_ conn "create table rollback_test(text_column number(10,0) primary key)"
        handleOracleError $ withTransaction conn $ do
          execute conn "insert into rollback_test values(:1)" (Only @Int 1)
          execute conn "insert into rollback_test values(:1)" (Only @Int 2)
          execute conn "insert into rollback_test values(:1)" (Only @Int 3)
          execute conn "insert into rollback_test values(:1)" (Only @Int 3) -- should fail
        results <- query_ @(Only Int) conn "select * from rollback_test"
        execute_ conn "drop table rollback_test"
        results `shouldBe` [] -- should roll back transaction

      it "should roll back to savepoint" $ \conn -> do
        execute_ conn "create table savepoint_test(text_column number(10,0) primary key)"
        withTransaction conn $ do
          execute conn "insert into savepoint_test values(:1)" (Only @Int 1)
          execute conn "insert into savepoint_test values(:1)" (Only @Int 2)
          handleOracleError $ withSavepoint conn $ do
            execute conn "insert into savepoint_test values(:1)" (Only @Int 3)
            execute conn "insert into savepoint_test values(:1)" (Only @Int 4)
            execute conn "insert into savepoint_test values(:1)" (Only @Int 4) -- should fail
        results <- query_ @(Only Int) conn "select * from savepoint_test"
        execute_ conn "drop table savepoint_test"
        results `shouldBe` [Only 1, Only 2] -- should roll back to before savepoint

      it "allows for nesting savepoints" $ \conn -> do
        execute_ conn "create table savepoint_nesting_test(text_column number(10,0) primary key)"
        withTransaction conn $ do
          execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 1)
          execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 2)
          handleOracleError $ withSavepoint conn $ do
            execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 3)
            execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 4)
            handleOracleError $ withSavepoint conn $ do
              execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 5)
              execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 5) -- should fail
            execute conn "insert into savepoint_nesting_test values(:1)" (Only @Int 6)
        results <- query_ @(Only Int) conn "select * from savepoint_nesting_test"
        execute_ conn "drop table savepoint_nesting_test"
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4, Only 6] -- should roll back to inner savepoint
 
      it "handles consecutive transactions" $ \conn -> do
        execute_ conn "create table transactions_test(text_column number(10,0) primary key)"
        -- transaction that inserts rows
        withTransaction conn $ do
          execute conn "insert into transactions_test values(:1)" (Only @Int 1)
          execute conn "insert into transactions_test values(:1)" (Only @Int 2)
          execute conn "insert into transactions_test values(:1)" (Only @Int 3)
          execute conn "insert into transactions_test values(:1)" (Only @Int 4)
        -- transaction that makes no changes that require commit
        withTransaction conn $ do
          _ <- query_ @(Only Int) conn "select * from transactions_test"
          pure ()
        -- transaction that inserts rows with savepoint
        withTransaction conn $ do
          execute conn "insert into transactions_test values(:1)" (Only @Int 5)
          withSavepoint conn $ do
            execute conn "insert into transactions_test values(:1)" (Only @Int 6)
          execute conn "insert into transactions_test values(:1)" (Only @Int 7)
        -- transaction that is rolled back
        handleOracleError $ withTransaction conn $ do
          execute conn "insert into transactions_test values(:1)" (Only @Int 6) -- should fail
        -- transaction that inserts rows with savepoint that is rolled back to
        withTransaction conn $ do
          execute conn "insert into transactions_test values(:1)" (Only @Int 8)
          handleOracleError $ withSavepoint conn $ do
            execute conn "insert into transactions_test values(:1)" (Only @Int 9)
            execute conn "insert into transactions_test values(:1)" (Only @Int 9) -- should fail
          execute conn "insert into transactions_test values(:1)" (Only @Int 10)
        results <- query_ @(Only Int) conn "select * from transactions_test"
        execute_ conn "drop table transactions_test"
        results `shouldBe` [Only 1 .. Only 8] <> [Only 10]

 where handleOracleError action = try @OracleError action >>= either (\_ -> pure ()) (\_ -> pure ())
