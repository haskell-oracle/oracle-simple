oracle-simple
=====================================

Modern bindings to Oracle [odpic](https://oracle.github.io/odpi/) C library.

## Example

```haskell
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
  let stmt = "select count(*), sysdate, 'ignore next column', 125.24, 3.14 from dual"
  conn <- createConn (ConnectionParams "username" "password" "localhost/XEPDB1")
  rows <- query @ReturnedRow conn stmt
  print rows

-- [ ReturnedRow { count = RowCount {getRowCount = 1.0}
--               , sysdate = DPITimeStamp {year = 2023, month = 9, day = 15, hour = 2, minute = 10, second = 50, fsecond = 0, tzHourOffset = 0, tzMinuteOffset = 0}
--               , hint = "ignore next column"
--               , amount = 125.24000000000001
--               , piValue = 3.14
--               }
-- ]

newtype RowCount = RowCount { getRowCount :: Double }
  deriving (Show)

instance FromField RowCount where
  fromField = RowCount <$> fromField

data ReturnedRow = ReturnedRow
  { count :: RowCount
  , sysdate :: DPITimeStamp
  , hint :: Text
  , amount :: Double
  , piValue :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass FromRow

-- instance FromRow ReturnedRow where
--   fromRow = do
--     count <- field
--     sysdate <- field
--     amount <- field
--     pure ReturnedRow{..}

```

## Build

```bash
$ nix-build
```

## Test

```bash
$ docker-compose up
$ nix-build && ./result/bin/example
```

