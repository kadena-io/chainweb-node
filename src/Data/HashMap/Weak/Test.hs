{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Data.HashMap.Weak.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.HashMap.Weak.Test
( properties

-- * Test Properties
, test
, testFib
, testAsyncFib
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Foldable (traverse_)
import Data.IORef

import Numeric.Natural

import Prelude hiding (null, lookup)

import System.Mem
import System.Random

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

-- internal modules

import Data.HashMap.Weak

-- -------------------------------------------------------------------------- --
-- Utils

gc :: MonadIO m => m ()
gc = liftIO $ performMajorGC >> threadDelay 10000

assertSize :: WeakHashMap k v -> Int -> PropertyM IO ()
assertSize m n = do
    s <- run (size m)
    assert (s == n)

delay :: MonadIO m => m ()
delay = liftIO $ threadDelay =<< randomRIO (1000, 4000)

produce :: MonadIO m => a -> m (Async a)
produce x = liftIO $ async $ delay >> return x

tick :: Enum a => IORef a -> IO b -> IO b
tick t a = atomicModifyIORef t (\x -> (succ x, ())) >> a

fibs :: [Natural]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- -------------------------------------------------------------------------- --
-- Tests

test :: [Int] -> PropertyM IO ()
test l = do
    m <- run new
    producers <- run $ traverse (\x -> (x,) <$> produce x) l

    run $ traverse_ (uncurry $ insert m) producers
    s <- run (size m)
    assert (l == [] || s > 0)
    run $ traverse_ (wait . snd) producers

    gc
    assertSize m 0

testFib :: Natural -> PropertyM IO ()
testFib n = do
    m <- run new
    t <- run $ newIORef 0

    let fib 0 = tick t $ return 1
        fib 1 = tick t $ return 1
        fib x = tick t $ memo m x $ \k ->
            (+) <$> fib (k - 1) <*> fib (k - 2)

    gc
    !r <- run $ fib n

    -- ticks <- run $ readIORef t
    -- assert (ticks < 8 * (max 1 n) - 1)
        -- This this is 4 times the expected value, to accomodate for
        -- the worst case. We should probably run an ols regression instead.

    assert (r == fibs !! fromIntegral n)

testAsyncFib :: Natural -> PropertyM IO ()
testAsyncFib n = do
    m <- run new
    t <- run $ newIORef 0

    let fib 0 = tick t $ return 1
        fib 1 = tick t $ return 1
        fib x = tick t $ memoAsync m x $ \k -> do
            threadDelay 100000
            (+) <$> fib (k - 1) <*> fib (k - 2)

    !r <- run $ fib n

    gc
    assertSize m 0
    ticks <- run $ readIORef t
    assert (ticks < 8 * (max 1 n) - 1)
        -- This this is 4 times the expected value, to accomodate for
        -- the worst case. We should probably run an ols regression instead.

    assert (r == fibs !! fromIntegral n)

properties :: [(String, Property)]
properties =
    [ ("WeakHashMap.test", property $ monadicIO . test)
    , ("WeakHashMap.testFib", property $ monadicIO . testFib)
    , ("WeakHashMap.testAsyncFib", property $ monadicIO . testAsyncFib)
    ]

