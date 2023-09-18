{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.TableInfo where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Text.Casing

-- | Information about the database table that a type corresponds to.
-- Optional. Used to auto-derive INSERT queries for the type.
class HasTableInfo a where
  -- | Name of the database table.
  -- By default, this is a snake-cased version of the data constructor name.
  tableName :: Proxy a -> String
  default tableName :: (GHasTableName (Rep a), Generic a) => Proxy a -> String
  tableName _ = gTableName (Proxy :: Proxy (Rep a))

  -- | Number of columns in the table.
  -- By default, the arity of the data constructor.
  columnCount :: Proxy a -> Int
  default columnCount :: (GHasColumnCount (Rep a), Generic a) => Proxy a -> Int
  columnCount _ = gColumnCount (Proxy :: Proxy (Rep a))

class GHasTableName (f :: Type -> Type) where
  gTableName :: Proxy f -> String

instance (TypeError ('Text "Void types not supported")) => GHasTableName V1 where
  gTableName _ = error "Void types not supported"

instance (GHasTableName f) => GHasTableName (M1 D c f) where
  gTableName _ = gTableName (Proxy :: Proxy f)

instance (Constructor c) => GHasTableName (M1 C c f) where
  gTableName _ = quietSnake $ conName (undefined :: t c f a)

instance (TypeError ('Text "Sum types not supported")) => GHasTableName (l :+: r) where
  gTableName = error "Sum types not supported"

class GHasColumnCount (f :: Type -> Type) where
  gColumnCount :: Proxy f -> Int

instance GHasColumnCount V1 where
  gColumnCount _ = 0

instance GHasColumnCount U1 where
  gColumnCount _ = 0

instance GHasColumnCount (K1 i c) where
  gColumnCount _ = 1

instance (GHasColumnCount f) => GHasColumnCount (M1 i c f) where
  gColumnCount _ = gColumnCount (Proxy :: Proxy f)

instance (TypeError ('Text "Sum types not supported")) => GHasColumnCount (l :+: r) where
  gColumnCount = error "Sum types not supported"

instance (GHasColumnCount a, GHasColumnCount b) => GHasColumnCount (a :*: b) where
  gColumnCount _ = gColumnCount (Proxy :: Proxy a) + gColumnCount (Proxy :: Proxy b)
