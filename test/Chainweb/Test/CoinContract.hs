{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Test the 'Coin Contract' pact code
--
module Chainweb.Test.CoinContract ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (readMVar)

import Data.Aeson
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Text

-- internal pact modules

import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime


-- internal chainweb modules

import Chainweb.Miner.Pact
import Chainweb.Pact.TransactionExec


tests :: TestTree
tests = testGroup "Chainweb.Test.CoinContract"
  [ testGroup "Pact Command Parsing"
    [ testCase "Buy Gas" buyGas'
    , testCase "Coinbase" coinbase'
    , testCase "Build Exec with Data" buildExecWithData
    , testCase "Build Exec without Data" buildExecWithoutData
    ]
  , testGroup "Pact Code Unit Tests"
    [ testCase "Coin Contract Repl Tests" (ccReplTests "pact/coin-contract/coin.repl")
    , testCase "Ns Repl Tests" (ccReplTests "pact/namespaces/ns.repl")
    , testCase "Payer Repl Tests" (ccReplTests "pact/gas-payer/gas-payer-v1.repl")
    ]
  ]

buyGas' :: Assertion
buyGas' = void $ mkBuyGasCmd minerId0 minerKeys0 sender0 1.0

coinbase' :: Assertion
coinbase' = void $ mkCoinbaseCmd minerId0 minerKeys0 1.0

buildExecWithData :: Assertion
buildExecWithData = void $ buildExecParsedCode
  (Just $ object [ "data" .= (1 :: Int) ]) "(+ 1 1)"

buildExecWithoutData :: Assertion
buildExecWithoutData = void $ buildExecParsedCode Nothing "(+ 1 1)"


ccReplTests :: FilePath -> Assertion
ccReplTests ccFile = do
    (r, rst) <- execScript' (Script False ccFile) ccFile
    either fail (\_ -> execRepl rst) r
  where
    execRepl rst = do
      lst <- readMVar $! _eePactDbVar . _rEnv $ rst
      for_ (_rlsTests lst) $ \tr ->
        traverse_ (uncurry failCC) $ trFailure tr

    failCC i e = assertFailure $ renderInfo (_faInfo i) <> ": " <> unpack e

------------------------------------------------------------------------------
-- Test Data
------------------------------------------------------------------------------

sender0 :: Text
sender0 = "sender"

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ mkKeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  "default"

minerId0 :: MinerId
minerId0 = MinerId "default miner"
