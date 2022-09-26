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
data HashMapTable k v = HashMapTable !(TVar (HM.HashMap k v))

instance Hashable k => ReadableTable (HashMapTable k v) k v where
    tableLookup k (HashMapTable var) = HM.lookup k <$!> readTVarIO var
    tableMember k (HashMapTable var) = 
        HM.member k <$> readTVarIO var

instance Hashable k => Table (HashMapTable k v) k v where
    tableInsert k v (HashMapTable var) =
        atomically $ modifyTVar' var (HM.insert k v)
    tableDelete k (HashMapTable var) =
        atomically $ modifyTVar' var (HM.delete k)

-- | Create new empty CAS
--
emptyTable :: Hashable k => IO (HashMapTable k v)
emptyTable = HashMapTable <$> newTVarIO mempty

-- | Return all entries of CAS as List
--
toList :: HashMapTable k v -> IO [v]
toList (HashMapTable var) = HM.elems <$!> readTVarIO var

-- | The number of items in the CAS
--
size :: HashMapTable k v -> IO Int
size (HashMapTable var) = HM.size <$!> readTVarIO var

