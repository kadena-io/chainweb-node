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

import Chainweb.Payload
import Chainweb.Test.Orphans.Internal ()

import Control.Concurrent (threadDelay)
import Control.Scheduler (Comp(..), scheduleWork, terminateWith, withScheduler)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

---

tests :: TestTree
tests = testGroup "Misc. Unit Tests"
    [ testGroup "scheduler"
          [ testCase "early termination result order" terminateOrder
          ]
    , testGroup "binary encoding"
          [ testProperty "BlockPayload" propPayloadBinaryEncoding
          , testProperty "BlockTransactions" propBlockTransactionsEncoding
          , testProperty "BlockOutputs" propBlockOutputsEncoding
          , testProperty "PayloadData" propPayloadDataEncoding
          , testProperty "PayloadWithOutputs" propPayloadWithOutputsEncoding
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

propPayloadBinaryEncoding :: BlockPayload -> Bool
propPayloadBinaryEncoding bp
  | Right x <- decodeBlockPayloads (encodeBlockPayloads bp) = x == bp
  | otherwise = False

propBlockTransactionsEncoding :: BlockTransactions -> Bool
propBlockTransactionsEncoding bt
  | Right x <- decodeBlockTransactions (encodeBlockTransactions bt) = x == bt
  | otherwise = False

propBlockOutputsEncoding :: BlockOutputs -> Bool
propBlockOutputsEncoding bo
  | Right x <- decodeBlockOutputs (encodeBlockOutputs bo) = x == bo
  | otherwise = False

propPayloadDataEncoding :: PayloadData -> Bool
propPayloadDataEncoding pd
  | Right x <- decodePayloadData (encodePayloadData pd) = x == pd
  | otherwise = False

propPayloadWithOutputsEncoding :: PayloadWithOutputs -> Bool
propPayloadWithOutputsEncoding pwo
  | Right x <- decodePayloadWithOutputs (encodePayloadWithOutputs pwo) = x == pwo
  | otherwise = False
