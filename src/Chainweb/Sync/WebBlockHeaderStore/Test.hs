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
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad.IO.Class

import Data.Foldable
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
import Data.HashMap.Weak
import Data.PQueue

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
    cas <- run $ emptyCas @(CAS.HashMapCas Fib)
    t <- run $ newIORef 0

    let fib 0 = tick t $ return $ Fib 0 1
        fib 1 = tick t $ return $ Fib 1 1
        fib x = tick t $ memoCache cas m x $ \k -> do
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
    assertSize m 0
    ticks <- run $ readIORef t
    assert (ticks == 4 * (max 1 n) - 1)
    assert (r == fibs !! fromIntegral n)

    casSize <- run $ fromIntegral <$> CAS.size cas
    assert (casSize == max 1 n - 1)

-- -------------------------------------------------------------------------- --
-- Test Task Queue Server

-- | A test task queue server for tasks that take int values as environment.
--
testQueueServer :: AttemptsCount -> PQueue (Task Int a) -> IO ()
testQueueServer limit q = forM_ [0..] $ session limit q (\_ _ -> return ())
    -- session limit q (\_ m -> T.putStrLn $ logText m)

-- TODO provide test with actual block header db

-- -------------------------------------------------------------------------- --
-- Utils

tick :: Enum a => IORef a -> IO b -> IO b
tick t a = atomicModifyIORef t (\x -> (succ x, ())) >> a

fibs :: [Natural]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

gc :: MonadIO m => m ()
gc = liftIO $ performMajorGC >> threadDelay 1000

assertSize :: WeakHashMap k v -> Int -> PropertyM IO ()
assertSize m i = do
    s <- run (size m)
    assert (s == i)

