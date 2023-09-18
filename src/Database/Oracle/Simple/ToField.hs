{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Oracle.Simple.ToField where

import Data.Int
import Data.Text
import Database.Oracle.Simple.Internal

class (HasDPINativeType a) => ToField a where
  toField :: a -> IO WriteBuffer

instance ToField Double where
  toField = pure . AsDouble

instance ToField Text where
  toField = fmap AsBytes . mkDPIBytesUTF8 . unpack

instance ToField String where
  toField = fmap AsBytes . mkDPIBytesUTF8

instance ToField Int64 where
  toField = pure . AsInt64

instance ToField Int where
  toField = pure . AsInt64 . fromIntegral

instance ToField DPITimestamp where
  toField = pure . AsTimestamp
