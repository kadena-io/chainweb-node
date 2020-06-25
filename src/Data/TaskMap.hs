{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.TaskMap
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A map for concurrent asynchronous task that can be awaited. Inserting a task
-- for an existing key doesn't create a new task but instead returns an
-- awaitable 'Async' handle to the existing task.
--
-- Task may depend on each other. It is up to the user to ensure to not create
-- infinite cylces that may not terminate.
--
-- Task are not memoized beyond the completion of the task. Once a task finishes
-- it is removed from the map. Computations that await the task still have
-- access to the result, but new requests for the task will trigger a new
-- exectuion. Therefore the methods from this module are best suited for long
-- running operations that may be requested by serveral concurrent proceses
-- roughly at the same time while the operation is running.
--
module Data.TaskMap
( TaskMap
, new
, insert
, delete
, lookup
, await
, size
, null
, memo
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (evaluate, finally)
import Control.Monad

import Data.Hashable
import qualified Data.HashMap.Strict as HM

import GHC.Generics

import Prelude hiding (lookup, null)

-- -------------------------------------------------------------------------- --
-- Task Map

newtype TaskMap k v = TaskMap (MVar (HM.HashMap k (Async v)))
    deriving (Generic)

new :: Eq k => Hashable k => IO (TaskMap k v)
new = TaskMap <$> newMVar mempty

insert :: Eq k => Hashable k => TaskMap k v -> k -> IO v -> IO (Async v)
insert tm@(TaskMap var) k t = modifyMVarMasked var $ \m -> do
    !a <- asyncWithUnmask $ \umask -> umask t `finally` delete tm k
    m' <- evaluate $ HM.insert k a m
    return (m', a)

delete :: Eq k => Hashable k => TaskMap k v -> k -> IO ()
delete (TaskMap var) k = modifyMVar_ var $ evaluate . HM.delete k

lookup :: Eq k => Hashable k => TaskMap k v -> k -> IO (Maybe (Async v))
lookup (TaskMap var) k = HM.lookup k <$!> readMVar var

-- | If the key is in the map this blocks and awaits the results, otherwise
-- it returns 'Nothing' immediately.
--
await :: Eq k => Hashable k => TaskMap k v -> k -> IO (Maybe v)
await t = traverse wait <=< lookup t

size :: TaskMap k v -> IO Int
size (TaskMap var) = HM.size <$!> readMVar var

null :: TaskMap k v -> IO Bool
null (TaskMap var) = HM.null <$!> readMVar var

memo
    :: Eq k
    => Hashable k
    => TaskMap k v
    -> k
    -> (k -> IO v)
        -- ^ an action that is used to produce the value if the key isn't in the map.
    -> IO v
memo tm@(TaskMap var) k task = do
    -- TODO: should we insert another lookup here? It depends on how optimistic
    -- we are that the lookup is successful.

    a <- modifyMVarMasked var $ \m -> case HM.lookup k m of
        Nothing -> do
            !a <- asyncWithUnmask $ \umask -> umask (task k) `finally` delete tm k
            m' <- evaluate $ HM.insert k a m
            return (m', a)
        (Just !a) -> return (m, a)
    wait a
