oracle-simple
=====================================
![](https://github.com/haskell-oracle/oracle-simple/actions/workflows/main.yml/badge.svg)

Modern bindings to Oracle [odpic](https://oracle.github.io/odpi/) C library.
 - See [here](https://github.com/oracle/odpi/blob/main/include/dpi.h) for a list of all structs and functions used in this library.

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
  conn <- connect (ConnectionParams "username" "password" "localhost/XEPDB1" Nothing)
  rows <- query_ conn stmt :: IO [ReturnedRow]
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

## Developing locally

### Using nix

#### Building

```bash
$ nix-build
```

```bash
$ nix-shell --run 'cabal build'
```

#### Running tests

```bash
$ docker-compose up -d
$ nix-build && ./result/bin/example
```

### Using stack

#### Building

First install `odpi` (e.g. on MacOS):
``` bash
brew install odpi
```

This should suffice to permit you to build:
```bash
$ stack build
```

#### Running tests

You'll need a runtime dependency: goto https://www.oracle.com/database/technologies/instant-client/macos-intel-x86-downloads.html#ic_osx_inst and follow the instant client installation instructions.

Then link a dynamic lib from the instant client to a location on your host where it can be found:
```
ln -s ~/Downloads/instantclient_19_8/libclntsh.dylib /usr/local/lib/
```

Run docker-compose up and tests as so:
``` bash
docker-compose up -d
stack run tests
```
