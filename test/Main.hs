{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Foreign.C.Types
import Data.Fixed
import Control.Monad.IO.Class (liftIO)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Time
import Test.Hspec
import Control.Exception

import Database.Oracle.Simple

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
        results <- query_ @(Only Int) conn "select * from savepoint_nesting_test"
        execute_ conn "drop table savepoint_nesting_test"
        results `shouldBe` [Only 1, Only 2, Only 3, Only 4] -- should roll back to outer savepoint
 
 where handleOracleError action = try @OracleError action >>= either (\_ -> pure ()) (\_ -> pure ())
