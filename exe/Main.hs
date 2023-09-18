{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Text (Text)
import Database.Oracle.Simple
import GHC.Generics 
import Data.Proxy

main :: IO ()
main = do
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")

  -- selecting
  let stmt = "select count(*), sysdate, 'text goes here', 125.24, TO_BINARY_FLOAT ('3.14'), CAST(null AS NUMBER(10,2)) from dual"
  rows <- query @ReturnedRow conn (stmt <> " UNION ALL " <> stmt)
  mapM_ print rows

  -- inserting
  rowsAffected <- autoInsert
    conn
    [ (SampleTable "d001" "Some text!" (Just 9.99) (Just 64))
    , (SampleTable "d002" "Some more text" Nothing (Just 10))
    , (SampleTable "d003" "Hello world" (Just 3.14) Nothing)
    , (SampleTable "d004" "Goodbye!"  Nothing Nothing)
    ]
  putStrLn $ "Rows affected: " <> show rowsAffected

newtype RowCount = RowCount { getRowCount :: Double }
  deriving stock (Show)
  deriving newtype (HasDPINativeType, FromField)

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimestamp
  , message :: Maybe String
  , amount :: Double
  , piValue :: Float
  , nullValue :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass FromRow

-- CREATE TABLE sample_table (
--   sample_string VARCHAR(20) PRIMARY KEY,
--	 sample_text VARCHAR(50) NOT NULL,
--	 sample_double NUMBER(10,5),
--	 sample_integer NUMBER(10,0)
--);

data SampleTable =
  SampleTable
  { sampleString :: String
  , sampleText :: Text
  , sampleDouble :: Maybe Double
  , sampleInteger :: Maybe Int
  } deriving (Show, Generic)

instance HasTableInfo SampleTable

instance ToRow SampleTable where
  toRow SampleTable{..} =
    SampleTable
    <$> (row sampleString)
    <*> (row sampleText)
    <*> (row sampleDouble)
    <*> (row sampleInteger)
