{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Data.HashMap.Weak
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.HashMap.Weak
( WeakHashMap
, new
, insert
, delete
, lookup
, size
, null
, memo

-- * Weak HashMap for asynchronous computations
, AsyncWeakHashMap
, await
, memoAsync
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (evaluate)
import Control.Monad.IO.Class

import Data.Hashable
import qualified Data.HashMap.Strict as HM

import GHC.Generics

import Prelude hiding (null, lookup)

import System.Mem.Weak

-- -------------------------------------------------------------------------- --
-- Weak Hash Map

-- | A map that only keeps week pointers to the values that are stored in the
-- map. The caveats associated with weak objects apply. This type is typically
-- used with values that are some sort of reference, like, for instance,
-- 'IORef', 'MVar', or 'Async'.
--
newtype WeakHashMap k v = WeakHashMap (MVar (HM.HashMap k (Weak v)))
    deriving (Generic)

new :: Eq k => Hashable k => IO (WeakHashMap k v)
new = WeakHashMap <$> newMVar mempty

insert :: Eq k => Hashable k => WeakHashMap k v -> k -> v -> IO ()
insert m@(WeakHashMap var) k v = do
    w <- mkWeakPtr v $ Just $ delete m k
    modifyMVar_ var $ evaluate . HM.insert k w

delete :: Eq k => Hashable k => WeakHashMap k v -> k -> IO ()
delete (WeakHashMap var) k = modifyMVar_ var $ evaluate . HM.delete k

lookup :: Eq k => Hashable k => WeakHashMap k v -> k -> IO (Maybe v)
lookup (WeakHashMap var) k = HM.lookup k <$> readMVar var >>= \case
    Nothing -> return Nothing
    Just x -> deRefWeak x

size :: WeakHashMap k v -> IO Int
size (WeakHashMap var) = HM.size <$> readMVar var

null :: WeakHashMap k v -> IO Bool
null (WeakHashMap var) = HM.null <$> readMVar var

memo
    :: Eq k
    => Hashable k
    => MonadIO m
    => WeakHashMap k v
    -> k
    -> (k -> m v)
        -- ^ an action that is used to produce the value if the key isn't
        -- in the map. Cf. 'testAsyncFib' for an example.
    -> m v
memo m k a = liftIO (lookup m k) >>= \case
    Nothing -> do
        !r <- a k
        r <$ liftIO (insert m k r)
    Just x -> return x

-- -------------------------------------------------------------------------- --
-- Tools for Weak Hash Maps over Async values

type AsyncWeakHashMap k v = WeakHashMap k (Async v)

memoAsync
    :: Eq k
    => Hashable k
    => AsyncWeakHashMap k v
    -> k
    -> (k -> IO v)
    -> IO v
memoAsync m k a = await $ memo m k (async . a)

await :: IO (Async b) -> IO b
await a = a >>= wait

