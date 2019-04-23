{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.TaskQueue.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.TaskQueue.Test
(
-- * Test Task Runner
  TestRunnerException(..)
, testRunner

-- * Test Properties
, properties
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch

import Data.Foldable
import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- internal modules

import Chainweb.Utils

import Data.PQueue

import P2P.TaskQueue

-- -------------------------------------------------------------------------- --
-- Test Task Runner

newtype TestRunnerException = TestRunnerException Int
    deriving (Show)

instance Exception TestRunnerException

testRunner :: AttemptsCount -> PQueue (Task Int a) -> IO ()
testRunner limit q = forM_ [0..] $ session_ limit q (\_ _ -> yield)
    -- session limit q (\_ m -> T.putStrLn $ logText m)

-- -------------------------------------------------------------------------- --
-- Tests

test1 :: Int -> IO Bool
test1 n = do
    tasks <- forM [0..n] $ \i ->
        newTask (TaskId $ sshow i) (Priority i) $ \_ -> return @_ @Int
    q <- newEmptyPQueue
    withAsync (testRunner 3 q) $ \_ -> do
        traverse_ (pQueueInsert q) tasks
        results <- traverse awaitTask tasks
        return $ results == [0..n]

test2a :: (Positive Int) -> IO Bool
test2a (Positive n_) = do
    tasks <- forM [0..n] $ \i ->
        newTask (TaskId $ sshow i) (Priority (n - i)) $ \_ -> return @_ @Int
    q <- newEmptyPQueue
    withAsync (testRunner 3 q) $ \_ -> do
        traverse_ (pQueueInsert q) tasks
        results <- traverse awaitTask tasks
        return $ results /= [0..n]
  where
    n = n_ + 10

test2b :: (Positive Int) -> IO Bool
test2b (Positive n_) = do
    tasks <- forM [0..n] $ \i ->
        newTask (TaskId $ sshow i) (Priority (n - i)) $ \_ -> return @_ @Int
    q <- newEmptyPQueue
    withAsync (testRunner 3 q) $ \_ -> do
        traverse_ (pQueueInsert q) tasks
        results <- traverse awaitTask tasks
        return $ L.sort results == [0..n]
  where
    n = n_ + 10

test3 :: Int -> IO Bool
test3 n = do
    tasks <- forM [0..n] $ \i ->
        newTask (TaskId $ sshow i) (Priority i) $ \_ -> return @_ @Int
    q <- newEmptyPQueue
    traverse_ (pQueueInsert q) tasks
    withAsync (testRunner 3 q) $ \_ -> do
        results <- traverse awaitTask tasks
        return $ results == [0..n]

test4 :: Int -> IO Bool
test4 n = do
    tasks <- forM [0..n] $ \i ->
        newTask (TaskId $ sshow i) (Priority (n - i)) $ \_ -> return @_ @Int
    q <- newEmptyPQueue
    traverse_ (pQueueInsert q) tasks
    withAsync (testRunner 3 q) $ \_ -> do
        results <- traverse awaitTask tasks
        return $ results == reverse [0..n]

test5 :: Positive Int -> Positive Int -> Positive Int -> IO Bool
test5 (Positive n) (Positive m) (Positive a)
    | a >= m = go
    | otherwise = (False <$ go)
        `catchSynchronous` \(TaskFailed l) -> return (length l == a)
  where
    go = do
        tasks <- forM [0..n] $ \i ->
            newTask (TaskId $ sshow i) (Priority i) $ \_ e -> if
                | e `mod` m == 0 -> return e
                | otherwise -> throwM $ TestRunnerException e
        q <- newEmptyPQueue
        traverse_ (pQueueInsert q) tasks
        withAsync (testRunner (int a) q) $ \_ -> do
            results <- traverse awaitTask tasks
            return $ results == [0,m..(n*m)]

properties :: [(String, Property)]
properties =
    [ ("TaskQueue.Test.test1", property $ monadicIO . run . test1)
    , ("TaskQueue.Test.test2a", property $ monadicIO . run . test2a)
    , ("TaskQueue.Test.test2b", property $ monadicIO . run . test2b)
    , ("TaskQueue.Test.test3", property $ monadicIO . run . test3)
    , ("TaskQueue.Test.test4", property $ monadicIO . run . test4)
    , ("TaskQueue.Test.test5", property $ \a b -> monadicIO . run . test5 a b)
    ]

