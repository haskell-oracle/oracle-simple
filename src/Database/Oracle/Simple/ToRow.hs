{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Oracle.Simple.ToRow where

import qualified Data.List as L
import Data.Proxy
import Control.Monad.State.Strict
import Database.Oracle.Simple.ToField
import Database.Oracle.Simple.Internal
import Database.Oracle.Simple.TableInfo
import Foreign.Ptr
import Control.Monad

newtype RowWriter a = RowWriter { runRowWriter :: DPIStmt -> StateT Column IO a }

newtype RowWriter' a = RowWriter' { runRowWriter' :: DPIStmt -> a -> StateT Column IO () }

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
  toRow :: a -> RowWriter a

instance ToField a => ToField (Maybe a) where
  toField (Just val) = toField val
  toField Nothing = pure $ AsNull nullPtr

row :: forall a. ToField a => a -> RowWriter a
row field = RowWriter $ \stmt -> do
    col <- modify (+1) >> get
    liftIO $ do
      dataValue <- toField field
      let dataIsNull = case dataValue of
            (AsNull _) -> 1
            _ -> 0
      bindValueByPos stmt col (dpiNativeType (Proxy @a)) (DPIData {..})
    pure field

insert :: ToRow a => DPIConn -> String -> [a] -> IO ()
insert conn sql rows = do
  stmt <- prepareStmt conn sql
  forM_ rows $ \row -> do
    _ <- evalStateT (runRowWriter (toRow row) stmt) (Column 0)
    execute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS

autoInsert :: forall a. (HasTableInfo a, ToRow a) => DPIConn -> [a] -> IO ()
autoInsert conn rows = do
  let sql = buildInsertStmt (Proxy :: Proxy a)
  stmt <- prepareStmt conn sql
  forM_ rows $ \row -> do
    _ <- evalStateT (runRowWriter (toRow row) stmt) (Column 0)
    execute stmt DPI_MODE_EXEC_COMMIT_ON_SUCCESS

buildInsertStmt :: HasTableInfo a => Proxy a -> String
buildInsertStmt proxy =
  let tname = tableName proxy
      colCount = columnCount proxy
  in "insert into " <> tname <> " values " <> "(" <> (L.intercalate "," $ fmap (":" <>) $ show <$> [1..colCount]) <> ")"
