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
import Data.Maybe (fromMaybe)

-- internal modules

import Chainweb.Storage.Table
import Chainweb.Storage.Table.RocksDB (Codec(..))
import Data.ByteString (ByteString)
import Control.Exception (throw)

-- | An 'IsTable' implementation that is base on 'HM.HashMap'.
--
data HashMapTable k v = HashMapTable (Codec v) (TVar (HM.HashMap k ByteString))

instance (Hashable k, Eq k) => ReadableTable (HashMapTable k v) k v where
    tableLookup (HashMapTable c var) k  = fmap (either throw id . _codecDecode c) . HM.lookup k <$!> readTVarIO var
    tableMember (HashMapTable _ var) k  =
        HM.member k <$> readTVarIO var

instance (Hashable k, Eq k) => Table (HashMapTable k v) k v where
    tableInsert (HashMapTable c var) k v =
        atomically $ modifyTVar' var (HM.insert k (_codecEncode c v))
    tableDelete (HashMapTable _ var) k =
        atomically $ modifyTVar' var (HM.delete k)

-- | Create new empty CAS
--
emptyTable :: (Hashable k, Eq k) => Codec v -> IO (HashMapTable k v)
emptyTable c = HashMapTable c <$> newTVarIO mempty

-- | Return all entries of CAS as List
--
toList :: HashMapTable k v -> IO [v]
toList (HashMapTable c var) = fmap (either throw id . _codecDecode c) . HM.elems <$!> readTVarIO var

-- | The number of items in the CAS
--
size :: HashMapTable k v -> IO Int
size (HashMapTable _ var) = HM.size <$!> readTVarIO var
