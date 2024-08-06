{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.Storage.Table.HashMap
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
-- Description:
--
-- A thread-safe in-memory 'Table' implementation based on 'HM.HashMap'.
--
module Chainweb.Storage.Table.HashMap
( HashMapTable
, emptyTable
, toList
, size
) where

import Control.Concurrent.STM.TVar
import Control.Monad ((<$!>))
import Control.Monad.STM

import Data.Hashable
import qualified Data.HashMap.Strict as HM

-- internal modules

import Chainweb.Storage.Table

-- | An 'IsTable' implementation that is base on 'HM.HashMap'.
--
newtype HashMapTable k v = HashMapTable (TVar (HM.HashMap k v))

instance (Hashable k, Eq k) => ReadableTable (HashMapTable k v) k v where
    tableLookup (HashMapTable var) k  = HM.lookup k <$!> readTVarIO var
    tableMember (HashMapTable var) k  = 
        HM.member k <$> readTVarIO var

instance (Hashable k, Eq k) => Table (HashMapTable k v) k v where
    tableInsert (HashMapTable var) k v =
        atomically $ modifyTVar' var (HM.insert k v)
    tableDelete (HashMapTable var) k =
        atomically $ modifyTVar' var (HM.delete k)

-- | Create new empty CAS
--
emptyTable :: (Hashable k, Eq k) => IO (HashMapTable k v)
emptyTable = HashMapTable <$> newTVarIO mempty

-- | Return all entries of CAS as List
--
toList :: HashMapTable k v -> IO [v]
toList (HashMapTable var) = HM.elems <$!> readTVarIO var

-- | The number of items in the CAS
--
size :: HashMapTable k v -> IO Int
size (HashMapTable var) = HM.size <$!> readTVarIO var

