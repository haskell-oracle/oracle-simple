{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Oracle.Simple.ToRow where

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.List                       as L
import           Data.Proxy
import           Data.Traversable
import           Data.Word
import           Database.Oracle.Simple.Internal
import           Database.Oracle.Simple.ToField
import           Foreign.Ptr
import           GHC.Generics
import           GHC.TypeLits

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
  gToRow (K1 x)= void (writeField x)

instance (ToField a) => ToField (Maybe a) where
  toField (Just val) = toField val
  toField Nothing = pure AsNull

writeField :: forall a. (ToField a) => a -> RowWriter ()
writeField field = RowWriter $ \stmt -> do
  col <- modify (+ 1) >> get
  liftIO $ do
    dataValue <- toField field
    let dataIsNull = case dataValue of
          AsNull -> 1
          _ -> 0
    bindValueByPos stmt col (dpiNativeType (Proxy @a)) (DPIData{..})

insert :: (ToRow a) => DPIConn -> String -> [a] -> IO Word64
insert conn sql rows = do
  stmt <- prepareStmt conn sql
  fmap fst <$> forAccumM 0 rows $ \totalRowsAffected row -> do
    _ <- evalStateT (runRowWriter (toRow row) stmt) (Column 0)
    execute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS
    rowsAffected <- getRowCount stmt
    pure (totalRowsAffected + rowsAffected, ())
