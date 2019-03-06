{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Test the 'Coin Contract' pact code
--
module Chainweb.Test.CoinContract
( tests
) where


import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson
import Data.Default (def)
import Data.Functor (void)
import Data.Text

import Control.Lens ((^.))

-- internal pact modules

import Pact.Types.Gas (GasLimit(..))
import Pact.Types.Runtime (Capability(..), evalCapabilities, capGranted)
import Pact.Types.Term (KeySet(..), Name(..), DefName(..))

-- internal chainweb modules

import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types (MinerId, MinerKeys)


tests :: TestTree
tests = testGroup "Coin Contract Unit Tests"
  [ testGroup "Pact Command Parsing"
    [ testCase "Buy Gas" buyGas'
    , testCase "Coinbase" coinbase'
    , testCase "Build Exec with Data" buildExecWithData
    , testCase "Build Exec without Data" buildExecWithoutData
    , testCase "Initialize Eval State with Capabilities" initCapabilities'
    ]
  ]

buyGas' :: Assertion
buyGas' = void $ mkBuyGasCmd minerId0 minerKeys0 sender0 gasLimit0

coinbase' :: Assertion
coinbase' = void $ mkCoinbaseCmd minerId0 minerKeys0 1.0

buildExecWithData :: Assertion
buildExecWithData = void $ buildExecParsedCode
  (Just $ object [ "data" .= (1 :: Int) ]) "(+ 1 1)"

buildExecWithoutData :: Assertion
buildExecWithoutData = void $ buildExecParsedCode Nothing "(+ 1 1)"

initCapabilities' :: Assertion
initCapabilities' = initCaps @?= evalCaps
  where
    es = initCapabilities initCaps
    evalCaps = fmap (\(UserCapability _ (DefName t) _) -> t) $
        es ^. evalCapabilities . capGranted

------------------------------------------------------------------------------
-- Test Data
------------------------------------------------------------------------------

sender0 :: Text
sender0 = "sender"

keyset0 :: KeySet
keyset0 = KeySet [] (Name "default" def)

gasLimit0 :: GasLimit
gasLimit0 = GasLimit 1

minerId0 :: MinerId
minerId0 = "default miner"

minerKeys0 :: MinerKeys
minerKeys0 = keyset0

initCaps :: [Text]
initCaps = ["CAP1", "CAP2"]
