{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.WebBlockHeaderDB.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Sync.WebBlockHeaderStore.Test
( properties

-- * Utils
, withNoopQueueServer
, startNoopQueueServer
, testQueueServer
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class

import Data.Hashable
import Data.IORef

import GHC.Generics (Generic)

import Numeric.Natural

import System.Mem

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- internal modules

import Chainweb.Sync.WebBlockHeaderStore

import Data.CAS
import qualified Data.CAS.HashMap as CAS
import Data.IVar
import Data.PQueue
import Data.TaskMap

import P2P.TaskQueue

-- -------------------------------------------------------------------------- --
-- Properties

properties :: [(String, Property)]
properties =
    [ ("Chainweb.WebBlockHeaderDB.Test.testAsyncFib", property $ monadicIO . testAsyncFib)
    ]

-- -------------------------------------------------------------------------- --
-- Test

data Fib = Fib !Natural !Natural
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

getFib :: Fib -> Natural
getFib (Fib _ b) = b

instance IsCasValue Fib where
    type CasKeyType Fib = Natural
    casKey (Fib a _) = a

testAsyncFib :: Natural -> PropertyM IO ()
testAsyncFib n = do
    m <- run new
    cas <- run CAS.emptyCas
    t <- run $ newIORef @Int 0

    let fib 0 = tick t $ return $ Fib 0 1
        fib 1 = tick t $ return $ Fib 1 1
        fib x = tick t $ memoInsert cas m x $ \k -> do
            r <- (+)
                <$> (getFib <$> fib (k - 1))
                    -- FIXME: this is synchronous, so the second fetch is
                    -- triggered only after the first completes.
                    -- It is still concurrent/shared with other queries, though.
                    -- Maybe, that's the right thing to do and we should
                    -- parallelize explicitely here?
                <*> (getFib <$> fib (k - 2))
            return $ Fib x r

    !(Fib _ !r) <- run $ fib n

    gc

    -- Here we miss-use quick checks coverage mechanism for testing a
    -- a distribution of the test restults.

    ms <- run $ size m
    monitor $ cover 0.75 (ms == 0) "memo map is empty after gc"

    ticks <- run $ readIORef t

    let expectedTicks :: Double
        expectedTicks = 2 * max 1 (fromIntegral n) - 1

    monitor $ cover 0.75 (fromIntegral ticks <= 1.5 * expectedTicks) "1.5 of expected ticks"

    casSize <- run $ fromIntegral <$> CAS.size cas
    monitor $ cover 1 (casSize == max 1 n - 1) "expected cas size"

    -- assert that result is correct
    assert (r == fibs !! fromIntegral n)
    return ()

-- -------------------------------------------------------------------------- --
-- Test Task Queue Server

-- | A test task queue server for tasks that take int values as environment.
--
testQueueServer :: AttemptsCount -> PQueue (Task Int a) -> IO ()
testQueueServer limit q = forM_ [0..] $ session_ limit q (\_ _ -> return ())

-- TODO provide test with actual block header db

withNoopQueueServer :: (PQueue (Task env a) -> IO b) -> IO b
withNoopQueueServer a = do
    q <- newEmptyPQueue
    let failTask = do
            task <- pQueueRemove q
            putIVar (_taskResult task) $ Left $ []
    withAsync (forever failTask) $ const $ a q

startNoopQueueServer :: IO (Async (), PQueue (Task env a))
startNoopQueueServer = do
    q <- newEmptyPQueue
    let failTask = do
            task <- pQueueRemove q
            putIVar (_taskResult task) $ Left $ []
    a <- async $ forever failTask
    return (a, q)

-- -------------------------------------------------------------------------- --
-- Utils

tick :: Enum a => IORef a -> IO b -> IO b
tick t a = atomicModifyIORef t (\x -> (succ x, ())) >> a

fibs :: [Natural]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

gc :: MonadIO m => m ()
gc = liftIO $ performMajorGC >> threadDelay 1000
