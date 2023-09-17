{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.TableInfo where

import GHC.TypeLits
import Data.Proxy
import GHC.Generics
import Data.Kind


class HasTableInfo a where
  tableName :: Proxy a -> String
  default tableName :: (GHasTableName (Rep a), Generic a) => Proxy a -> String
  tableName _ = gTableName (Proxy :: Proxy (Rep a))
  columnCount :: Proxy a -> Int
  default columnCount :: (GHasColumnCount (Rep a), Generic a) => Proxy a -> Int
  columnCount _ = gColumnCount (Proxy :: Proxy (Rep a))

class GHasTableName (f :: Type -> Type) where
  gTableName :: Proxy f -> String

instance GHasTableName V1 where
  gTableName _ = mempty

instance GHasTableName f => GHasTableName (M1 D c f) where
  gTableName _ = gTableName (Proxy :: Proxy f)

instance (Constructor c) => GHasTableName (M1 C c f) where
  gTableName _ = conName (undefined :: t c f a)

instance TypeError ('Text "Sum types not supported") => GHasTableName (l :+: r) where
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

instance TypeError ('Text "Sum types not supported") => GHasColumnCount (l :+: r) where
  gColumnCount = error "Sum types not supported"

instance (GHasColumnCount a, GHasColumnCount b) => GHasColumnCount (a :*: b) where
  gColumnCount _ = gColumnCount (Proxy :: Proxy a) + gColumnCount (Proxy :: Proxy b)
