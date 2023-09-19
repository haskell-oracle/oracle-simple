{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Functor.Identity
import Data.Text (Text)
import Database.Oracle.Simple
import GHC.Generics
import Data.Proxy

main :: IO ()
main = do
  conn <- connect (ConnectionParams "username" "password" "localhost/XEPDB1")

  -- querying
  let selectStmt = "select count(*), sysdate, 'text goes here', 125.24, TO_BINARY_FLOAT ('3.14'), CAST(null AS NUMBER(10,2)) from dual"
  rows <- query_ @ReturnedRow conn (selectStmt <> " UNION ALL " <> selectStmt)
  mapM_ print rows
  print =<< ping conn
  close conn
  print =<< ping conn
  release conn

  putStrLn ""

  -- inserting
  let insertStmt = "insert into sample_table values (:1, :2, :3, :4)"
  rowsAffected <- executeMany
   conn
   insertStmt
   [ (SampleTable "d001" "Some text!" (Just 9.99) (Just 64))
   , (SampleTable "d002" "Some more text" Nothing (Just 10))
   , (SampleTable "d003" "Hello world" (Just 3.14) Nothing)
   , (SampleTable "d004" "Goodbye!" Nothing Nothing)
   ]
  putStrLn $ "Rows affected: " <> show rowsAffected

  -- querying with where clause
  let selectWhereStmt = "select * from sample_table where sample_string=:1 and sample_integer=:2"
  rows <- query @SampleTable conn selectWhereStmt ("d001" :: String, 64 :: Int)
  mapM_ print rows

  -- updating
  let updateStmt = "update sample_table st set st.sample_double=:1 where st.sample_string=:2"
  rowsAffected <- execute conn updateStmt (Just @Double 123.45, "d002" :: String)
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
  } deriving stock (Show, Generic)
    deriving anyclass ToRow
    deriving anyclass FromRow

-- instance ToRow SampleTable where
--   toRow SampleTable{..} = do
--     rowField sampleString
--     rowField sampleText
--     rowField sampleDouble
--     rowField sampleInteger
