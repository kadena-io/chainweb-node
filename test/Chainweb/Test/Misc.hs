{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module: Chainweb.Test.Misc
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Miscellaneous tests.
--
module Chainweb.Test.Misc
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Scheduler (Comp(..), scheduleWork, terminateWith, withScheduler)

import Test.Tasty
import Test.Tasty.HUnit

---

tests :: TestTree
tests = testGroup "Misc. Unit Tests"
    [ testGroup "scheduler"
          [ testCase "early termination result order" terminateOrder
          ]
    ]

-- | Guarantee that `terminateWith` makes the scheduler's "head" return value be
-- the default that was given.
--
terminateOrder :: Assertion
terminateOrder = do
    r <- withScheduler Par' $ \sch -> do
        scheduleWork sch (threadDelay 5_000_000 >> pure 1)
        scheduleWork sch (terminateWith sch 10)
    head r @?= (10 :: Int)
