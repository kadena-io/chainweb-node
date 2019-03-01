{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.PQueue
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.PQueue
( PQueue
, newEmptyPQueue
, pQueueInsert
, pQueueRemove
, pQueueIsEmpty
, pQueueSize
) where

import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (evaluate, onException, mask_)

import qualified Data.Heap as H
import Data.Maybe

import GHC.Generics

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- PQueue

-- | A thread safe priority queue.
--
-- The queue is fair for users of the queue. It does not guarantee progress for
-- items in the queue. An item of low priority my starve in the queue if higher
-- priority items are added a rate at least as high as items are removed.
--
data PQueue a = PQueue !QSem !(MVar (H.Heap a))
    deriving (Generic)

newEmptyPQueue :: IO (PQueue a)
newEmptyPQueue = PQueue <$> newQSem 0 <*> newMVar mempty

pQueueInsert :: Ord a => PQueue a -> a -> IO ()
pQueueInsert (PQueue s q) t = modifyMVarMasked_ q $ \h -> do
    h' <- evaluate $ H.insert t h
    signalQSem s
    return h'

pQueueIsEmpty :: PQueue a -> IO Bool
pQueueIsEmpty (PQueue _ q) = H.null <$> readMVar q

pQueueSize :: PQueue a -> IO Natural
pQueueSize (PQueue _ q) = fromIntegral . H.size <$> readMVar q

-- | Blocks if the queue is empty
--
pQueueRemove :: PQueue a -> IO a
pQueueRemove (PQueue s q) = mask_ $ do
    waitQSem s
        -- waitQSem this is interruptible, which is fine. We need to be in
        -- masked state only after waitQSem succeeds.

    h <- takeMVar q `onException` signalQSem s
        -- modifyMVar is interruptible and we must ensure that we return
        -- the semaphor if we receive an exception while waiting.

    (!a, !h') <- evaluate (fromJust $ H.uncons h) `onException` putMVar q h
        -- the @fromJust@ here is guaranteed to succeed

    putMVar q h'
    return a

