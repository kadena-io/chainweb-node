{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
module Chainweb.Test.CoinContract
( tests
) where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (readMVar)

import Data.Aeson
import Data.Decimal (Decimal)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Text
import Data.Word

import Control.Lens ((^.))

-- internal pact modules

import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime

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
  , testGroup "Pact Code Unit Tests"
    [ testCase "Coin Contract Repl Tests" ccReplTests
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
    -- set the capabilities to be some initial list of caps
    es = initCapabilities initCaps
    -- extract the caps from the capgranted list
    evalCaps = extractName =<< es ^. evalCapabilities . capGranted
    -- extract names from the caps
    extractName = \case
      (UserCapability _ (DefName t) _) -> pure t
      _ -> fail "the impossible has finally happened"

ccReplTests :: Assertion
ccReplTests = do
    (r, rst) <- execScript' (Script False ccFile) ccFile
    either fail (\_ -> execRepl rst) r
  where
    execRepl rst = do
      lst <- readMVar $! _eePactDbVar . _rEnv $ rst
      for_ (_rlsTests lst) $ \tr ->
        maybe (pure ()) (uncurry failCC) $ trFailure tr

    failCC i e = fail $ renderInfo (_faInfo i) <> ": " <> unpack e

------------------------------------------------------------------------------
-- Test Data
------------------------------------------------------------------------------

sender0 :: Text
sender0 = "sender"

keyset0 :: KeySet
keyset0 = KeySet
  ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
  (Name "default" def)

minerId0 :: MinerId
minerId0 = "default miner"

minerKeys0 :: MinerKeys
minerKeys0 = keyset0

gasLimit0 :: Decimal
gasLimit0 = fromIntegral @Word64 @Decimal 1

initCaps :: [Text]
initCaps = ["CAP1", "CAP2"]

ccFile :: String
ccFile = "pact/coin-contract/coin.repl"
