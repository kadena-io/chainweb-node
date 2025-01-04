{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Data.PQueue
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A thread safe priority queue.
--
module Data.PQueue
( PQueue
, newEmptyPQueue
, pQueueInsert
, pQueueInsertLimit
, pQueueRemove
, pQueueIsEmpty
, pQueueSize
) where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad

import qualified Data.Heap as H

import GHC.Generics

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- PQueue

-- | A thread safe priority queue.
--
-- The queue is fair for users of the queue. It does not guarantee progress for
-- items in the queue. An item of low priority my starve in the queue if higher
-- priority items are added at a rate at least as high as items are removed.
--
data PQueue a = PQueue !(MVar ()) !(MVar (H.Heap a))
    deriving (Generic)

newEmptyPQueue :: IO (PQueue a)
newEmptyPQueue = PQueue <$> newEmptyMVar <*> newMVar mempty

pQueueInsert :: Ord a => PQueue a -> a -> IO ()
pQueueInsert (PQueue s q) t = modifyMVarMasked_ q $ \h -> do
    h' <- evaluate $ H.insert t h
    void $ tryPutMVar s ()
    return h'

pQueueInsertLimit :: Ord a => PQueue a -> Natural -> a -> IO ()
pQueueInsertLimit (PQueue s q) l t = modifyMVarMasked_ q $ \h -> do
    h' <- evaluate $ H.insert t h
    void $ tryPutMVar s ()
    return $! if H.size h > 2 * fromIntegral l
        then H.take (fromIntegral l) h'
        else h'

pQueueIsEmpty :: PQueue a -> IO Bool
pQueueIsEmpty (PQueue _ q) = H.null <$!> readMVar q

pQueueSize :: PQueue a -> IO Natural
pQueueSize (PQueue _ q) = fromIntegral . H.size <$!> readMVar q

-- | If the queue is empty it blocks and races for new items
--
pQueueRemove :: PQueue a -> IO a
pQueueRemove (PQueue s q) = run
  where
    run = do
        r <- modifyMVarMasked q $ \h -> case H.uncons h of
            Nothing -> return (h, Nothing)
            Just (!a, !b) -> do
                when (H.null b) $ void $ tryTakeMVar s
                return (b, Just a)
        case r of
            Nothing -> takeMVar s >> run
            (Just !x) -> return x
