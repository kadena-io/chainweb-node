{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.Storage.Table.HashMap
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
-- Description:
--
-- A thread-safe in-memory 'Table' implementation based on 'M.HashMap'.
--
module Chainweb.Storage.Table.Map
( MapTable
, emptyTable
, toList
, size
, deleteLt
, deleteLe
) where

import Chainweb.Storage.Table
import Control.Concurrent.STM.TVar
import Control.Monad ((<$!>))
import Control.Monad.STM
import Data.Map.Strict qualified as M

-- | An 'IsTable' implementation that is base on 'M.HashMap'.
--
newtype MapTable k v = MapTable (TVar (M.Map k v))

instance (Ord k, Eq k) => ReadableTable (MapTable k v) k v where
    tableLookup (MapTable var) k  = M.lookup k <$!> readTVarIO var
    tableMember (MapTable var) k  =
        M.member k <$> readTVarIO var

instance (Ord k, Eq k) => Table (MapTable k v) k v where
    tableInsert (MapTable var) k v =
        atomically $ modifyTVar' var (M.insert k v)
    tableDelete (MapTable var) k =
        atomically $ modifyTVar' var (M.delete k)

-- | Create new empty CAS
--
emptyTable :: (Ord k, Eq k) => IO (MapTable k v)
emptyTable = MapTable <$> newTVarIO mempty

-- | Return all entries of CAS as List
--
toList :: MapTable k v -> IO [v]
toList (MapTable var) = M.elems <$!> readTVarIO var

-- | The number of items in the CAS
--
size :: MapTable k v -> IO Int
size (MapTable var) = M.size <$!> readTVarIO var

-- | Delete all keys that are strictly smaller than the given key
--
deleteLt :: Ord k => MapTable k v -> k -> IO ()
deleteLt (MapTable var) k =
    atomically $ modifyTVar' var $ M.dropWhileAntitone (< k)

-- | Delete all keys that are smaller or equal than the given key
--
deleteLe :: Ord k => MapTable k v -> k -> IO ()
deleteLe (MapTable var) k =
    atomically $ modifyTVar' var $ M.dropWhileAntitone (<= k)
