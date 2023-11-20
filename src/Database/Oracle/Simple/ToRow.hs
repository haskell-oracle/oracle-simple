{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.ToRow where

import Control.Monad
import Control.Monad.State.Strict
import Data.Functor.Identity
import qualified Data.List as L
import Data.Proxy
import Data.Traversable
import Data.Word
import Database.Oracle.Simple.Internal
import Database.Oracle.Simple.ToField
import Foreign.Ptr
import GHC.Generics
import GHC.TypeLits

newtype RowWriter a = RowWriter {runRowWriter :: DPIStmt -> StateT Column IO a}

instance Functor RowWriter where
  fmap f g = RowWriter $ \dpiStmt -> f <$> runRowWriter g dpiStmt

instance Applicative RowWriter where
  pure a = RowWriter $ \_ -> pure a
  fn <*> g = RowWriter $ \dpiStmt -> do
    f <- runRowWriter fn dpiStmt
    f <$> runRowWriter g dpiStmt

instance Monad RowWriter where
  return = pure
  f >>= g = RowWriter $ \dpiStmt -> do
    f' <- runRowWriter f dpiStmt
    runRowWriter (g f') dpiStmt

class ToRow a where
  toRow :: a -> RowWriter ()
  default toRow :: (GToRow (Rep a), Generic a) => a -> RowWriter ()
  toRow = gToRow . from

instance ToField a => ToRow (Only a)

instance ToField a => ToRow (Identity a)

instance (ToField a, ToField b) => ToRow (a, b)

instance (ToField a, ToField b, ToField c) => ToRow (a, b, c)

instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a, b, c, d)

instance (ToField a, ToField b, ToField c, ToField d, ToField e) => ToRow (a, b, c, d, e)

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f) => ToRow (a, b, c, d, e, f)

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g) => ToRow (a, b, c, d, e, f, g)

instance
  (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h)
  => ToRow (a, b, c, d, e, f, g, h)

instance
  (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h, ToField i)
  => ToRow (a, b, c, d, e, f, g, h, i)

instance
  (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g, ToField h, ToField i, ToField j)
  => ToRow (a, b, c, d, e, f, g, h, i, j)

class GToRow f where
  gToRow :: f a -> RowWriter ()

instance (GToRow m) => GToRow (D1 i m) where
  gToRow (M1 x) = gToRow x

instance (GToRow m) => GToRow (C1 i m) where
  gToRow (M1 x) = gToRow x

instance (GToRow m) => GToRow (S1 i m) where
  gToRow (M1 x) = gToRow x

instance (GToRow l, GToRow r) => GToRow (l :*: r) where
  gToRow (l :*: r) = gToRow l >> gToRow r

instance (TypeError ('Text "Sum types not supported")) => GToRow (l :+: r) where
  gToRow = error "Sum types not supported"

instance (ToField a) => GToRow (K1 i a) where
  gToRow (K1 x) = void (writeField x)

writeField :: forall a. (ToField a) => a -> RowWriter ()
writeField field = RowWriter $ \stmt -> do
  col <- modify (+ 1) >> get
  liftIO $ do
    dataValue <- toField field
    let dataIsNull = case dataValue of
          AsNull -> 1
          _ -> 0
    bindValueByPos stmt col (toDPINativeType (Proxy @a)) (DPIData{..})
    freeWriteBuffer dataValue -- no longer needed as dpiStmt_bindValueByPos creates a memory-managed dpiVar
