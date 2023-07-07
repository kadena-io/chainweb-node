{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
, uninterruptibleCancelTask
, size
, null
, clear
, memo
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad

import Data.Hashable
import qualified Data.HashMap.Strict as HM

import GHC.Generics

import Prelude hiding (lookup, null)

-- -------------------------------------------------------------------------- --
-- Exceptions

-- | Exception that is raised when tasks in the map a cancelled. Using a
-- dedicated exception type allows users to distinguish between the case when a
-- task is cancelled explicitely or due to an external exception.
--
data TaskCancelled = TaskCancelled
    deriving (Eq, Show, Generic)

instance Exception TaskCancelled where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

-- -------------------------------------------------------------------------- --
-- Task Map

-- | A memoization table for sharing long running tasks. It guarantees that only
-- a single instance of a potentiall expensive operations is running at a time.
-- Tasks are identified by unique keys.
--
-- Unlike classic memoization tables only the task for producing a result is
-- shared but not the result itself. Once a task is complete all computations
-- that await the task are notified and receive the result. However, subsequent
-- requests for the same task cause a new task to be created.
--
-- This TaskMap is inteded to be used with another layer for caching the actual
-- results of running tasks.
--
newtype TaskMap k v = TaskMap (MVar (HM.HashMap k (Async v))) deriving (Generic)

new :: Eq k => Hashable k => IO (TaskMap k v)
new = TaskMap <$> newMVar mempty

insert :: Eq k => Hashable k => TaskMap k v -> k -> IO v -> IO (Async v)
insert tm@(TaskMap var) k t = modifyMVarMasked var $ \m -> do
    !a <- asyncWithUnmask $ \umask -> umask t `finally` delete tm k
    m' <- evaluate $ HM.insert k a m
    return (m', a)

-- | Delete a task from the map. This does not cancel the task. Any users that
-- holds a reference to the task continuous to await it.
--
delete :: Eq k => Hashable k => TaskMap k v -> k -> IO ()
delete (TaskMap var) k = modifyMVar_ var $ evaluate . HM.delete k

-- | Lookup a task in the map. Returns 'Nothing' if there is not task for the
-- given key.
--
lookup :: Eq k => Hashable k => TaskMap k v -> k -> IO (Maybe (Async v))
lookup (TaskMap var) k = HM.lookup k <$!> readMVar var

-- | If the key is in the map this blocks and awaits the results, otherwise
-- it returns 'Nothing' immediately.
--
await :: Eq k => Hashable k => TaskMap k v -> k -> IO (Maybe v)
await t = traverse wait <=< lookup t

-- | Tne number of tasks in the map.
--
-- Complexity: \(O(n)\)
--
size :: TaskMap k v -> IO Int
size (TaskMap var) = HM.size <$!> readMVar var

-- | Check whether the map is is empty.
--
null :: TaskMap k v -> IO Bool
null (TaskMap var) = HM.null <$!> readMVar var

-- | Cancel a task in the map and wait for the task to be removed from the map.
--
-- The removed task raises a 'TaskCancelled' exception on all waiting
-- computations.
--
-- Note that the finalizer of a task aquires a lock on the task map in order to
-- remove itself from the map.
--
uninterruptibleCancelTask :: Eq k => Hashable k => TaskMap k v -> k -> IO ()
uninterruptibleCancelTask t k = lookup t k >>= \case
    Nothing -> return ()
    Just a -> uninterruptibleMask_ $ cancelWith a TaskCancelled

-- | Cancel all tasks in the map at the time that this function is called.
-- Termination of each task is awaited under an uninterruptible mask.
--
-- The operation is not atomic. Tasks are cancelled on by one and the map can be
-- queried and modified concurrently. When this operation completes all tasks
-- that existed when the operation started have been canceled. But new tasks may
-- have been added in the meantime.
--
clear :: Eq k => Hashable k => TaskMap k v -> IO ()
clear (TaskMap var) = do

    -- This can be done under a lock, because the finalizers on individual tasks
    -- aquire a lock on the map and remove themself. As a consequence this
    -- operation is not transactional
    --
    m <- readMVar var
    forM_ m $ \a -> uninterruptibleMask_ $ cancelWith a TaskCancelled

-- | Wait for a task for a given key to complete. If the task for the given key
-- doesn not yet exist in the map a new task is created.
--
-- When a task completes it is removed from the map. Any caller that was waiting
-- for the task is notified and receives the result.
--
-- Subsequent requests for the same key cause a new task to be created.
--
-- If an exception occurs while a tasks is processed that exception is rethrown
-- by this function.
--
memo
    :: Eq k
    => Hashable k
    => TaskMap k v
    -> k
    -> (k -> IO v)
        -- ^ an action that is used to produce the value if the key isn't in the map.
    -> IO v
memo tm@(TaskMap var) k task = do

    -- Optimistically try without taking a lock:
    t <- (HM.lookup k <$> readMVar var) >>= \case
        Just !a -> do
            -- print $ "MEMO: hit " <> show (asyncThreadId a)
            return a

        -- Aquire lock and try again
        Nothing -> modifyMVarMasked var $ \m -> case HM.lookup k m of
            Nothing -> do
                !a <- asyncWithUnmask $ \umask -> umask (task k) `finally` delete tm k
                m' <- evaluate $ HM.insert k a m
                return (m', a)
            (Just !a) -> return (m, a)
    wait t
