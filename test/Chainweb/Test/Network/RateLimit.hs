{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Network.RateLimit
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Network.RateLimit
( tests
) where

import Control.Monad

import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Network.RateLimit

-- -------------------------------------------------------------------------- --
-- Utils

getTimespan :: Timespan -> Double
getTimespan (Timespan s) = s

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Network.RateLimit"
    [ testCase "request_0_0" test_request_0_0
    , testCase "request_0_1" test_request_0_1
    , testCase "request_1_0" test_request_1_0
    , testCase "request_1_1" test_request_1_1
    , testCase "request_2_2" test_request_2_2
    , testCase "request_4_2" test_request_4_2
    , testCase "request_100_2" test_request_100_2
    , testCase "request_1000_1000" test_request_1000_1000
    , testCase "request_2_3" test_request_2_3
    , testCase "request_1_05" test_request_1_05
    , testCase "request_05_05" test_request_05_05
    , testCase "request_05_05_" test_request_05_05_
    , testCase "request_05_05__" test_request_05_05__
    ]

test_request_0_0 :: Assertion
test_request_0_0 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 0) (Disposal 0)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 0

test_request_0_1 :: Assertion
test_request_0_1 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 99] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 0) (Disposal 1)
    length (catMaybes r) @?= 0

test_request_1_0 :: Assertion
test_request_1_0 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 1) (Disposal 0)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 0

test_request_1_1 :: Assertion
test_request_1_1 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 1) (Disposal 1)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 0

test_request_2_2 :: Assertion
test_request_2_2 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 2) (Disposal 2)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 500

test_request_4_2 :: Assertion
test_request_4_2 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 4) (Disposal 2)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 499

test_request_100_2 :: Assertion
test_request_100_2 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 100) (Disposal 2)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 451

test_request_1000_1000 :: Assertion
test_request_1000_1000 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 1000) (Disposal 1000)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= sum [1 .. 999]

test_request_2_3 :: Assertion
test_request_2_3 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t ->
        request b (Timestamp $ fromIntegral t) (Size 2) (Disposal 3)
    length (catMaybes r) @?= 0

test_request_1_05 :: Assertion
test_request_1_05 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t -> do
        void $ request b (Timestamp $ fromIntegral t) (Size 1) (Disposal 0.5)
        request b (Timestamp $ fromIntegral t) (Size 1) (Disposal 0.5)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 0

test_request_05_05 :: Assertion
test_request_05_05 = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t -> do
        void $ request b (Timestamp $ fromIntegral t) (Size 0.5) (Disposal 0.5)
        request b (Timestamp $ fromIntegral t) (Size 0.5) (Disposal 0.5)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 500

test_request_05_05_ :: Assertion
test_request_05_05_ = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t -> do
        void $ request b (Timestamp $ fromIntegral t) (Size 0.5) (Disposal 0.5)
        request b (Timestamp $ 0.5 + fromIntegral t) (Size 0.5) (Disposal 0.5)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 0

test_request_05_05__ :: Assertion
test_request_05_05__ = do
    b <- createBucket (Timestamp 0) (Level 0)
    r <- forM [0::Int .. 999] $ \t -> do
        void $ request b (Timestamp $ fromIntegral t) (Size 0.5) (Disposal 0.5)
        request b (Timestamp $ 0.25 + fromIntegral t) (Size 0.5) (Disposal 0.5)
    length (catMaybes r) @?= 1000
    sum (getTimespan <$> catMaybes r) @?= 250

