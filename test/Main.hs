{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Time
import Test.Hspec

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
