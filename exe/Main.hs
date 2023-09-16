{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Text (Text)
import Database.Oracle.Simple
import GHC.Generics (Generic)

main :: IO ()
main = do
  insertTest
  {-
  let stmt = "select count(*), sysdate, 'text goes here', 125.24, TO_BINARY_FLOAT ('3.14'), CAST(null AS NUMBER(10,2)) from dual"
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  rows <- query @ReturnedRow conn (stmt <> " UNION ALL " <> stmt)
  mapM_ print rows -}

-- CREATE TABLE bank_user (
--	user_id VARCHAR(20) PRIMARY KEY,
--	user_name VARCHAR(50) NOT NULL,
--	user_balance NUMBER(10,5) NOT NULL,
--	user_email VARCHAR(50)
-- );

insertTest :: IO ()
insertTest = do
  let sql = "insert into bank_user values (:1, :2, :3, :4)"
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  stmt <- prepareStmt conn sql
  idVal' <- mkDPIBytesUTF8 "d001"
  let idVal = DPIData 0 (AsBytes idVal')
  nameVal' <- mkDPIBytesUTF8 "Jane Doe"
  let nameVal = DPIData 0 (AsBytes nameVal')
  let balanceVal = DPIData 0 (AsDouble 9920.5)
  emailVal' <- mkDPIBytesUTF8 "peter@gmail.com"
  let emailVal = DPIData 0 (AsBytes emailVal')
  bindValueByPos stmt (Column 1) DPI_NATIVE_TYPE_BYTES idVal
  bindValueByPos stmt (Column 2) DPI_NATIVE_TYPE_BYTES nameVal
  bindValueByPos stmt (Column 3) DPI_NATIVE_TYPE_DOUBLE balanceVal
  bindValueByPos stmt (Column 4) DPI_NATIVE_TYPE_BYTES emailVal
  execute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS -- TODO use default and then commit explicitly
  pure ()


newtype RowCount = RowCount { getRowCount :: Double }
  deriving stock (Show)
  deriving newtype (FromField)

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  , message :: Maybe String
  , amount :: Double
  , piValue :: Float
  , nullValue :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass FromRow

-- instance FromRow ReturnedRow where
--   fromRow = do
--     count <- field
--     sysdate <- field
--     amount <- field
--     pure $ ReturnedRow{..}
