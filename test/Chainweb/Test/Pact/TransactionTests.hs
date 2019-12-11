{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Test func in TransactionExec
--
module Chainweb.Test.Pact.TransactionTests ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (readMVar)
import Control.Exception (SomeException, try)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Text
import Data.Default

-- internal pact modules

import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Command
import Pact.Types.Runtime
import Pact.Types.Logger
import Pact.Interpreter


-- internal chainweb modules
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Miner.Pact
import Chainweb.BlockHash
import Chainweb.Utils
import Chainweb.Time

coinRepl :: FilePath
coinRepl = "pact/coin-contract/coin.repl"

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.TransactionTests"
  [ testGroup "Pact Command Parsing"
    [ testCase "Build Exec with Data" buildExecWithData
    , testCase "Build Exec without Data" buildExecWithoutData
    ]
  , testGroup "Pact Code Unit Tests"
    [ testCase "Coin Contract Repl Tests" (ccReplTests coinRepl)
    , testCase "Ns Repl Tests" (ccReplTests "pact/namespaces/ns.repl")
    , testCase "Payer Repl Tests" (ccReplTests "pact/gas-payer/gas-payer-v1.repl")
    ]
  , testGroup "Coinbase tests"
    [ testCase "testCoinbase" testCoinbase
    --, testCase "testCoinbaseGenesis" testCoinbaseGenesis
    , testCase "testCoinbase791Fix" testCoinbase797Fix
    , testCase "testCoinbaseNewBlock" testCoinbaseNewBlock
    , testCase "testCoinbaseEnforceFailure" testCoinbaseEnforceFailure
    ]
  ]


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

loadCC :: IO (PactDbEnv LibState, ModuleCache)
loadCC = do
  (r, rst) <- execScript' (Script False coinRepl) coinRepl
  either fail (const $ return ()) r
  let pdb = PactDbEnv
            (view (rEnv . eePactDb) rst)
            (view (rEnv . eePactDbVar) rst)
      mc = view (rEvalState . evalRefs . rsLoadedModules) rst
  return (pdb,mc)


testCoinbase :: Assertion
testCoinbase = do
  (pdb,mc) <- loadCC
  void $ applyCoinbase toyVersion logger pdb miner 0.1 pubData blockHsh
       (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False)
       mc
  where
    miner = noMiner
    pubData = PublicData def blockHeight' blockTime (toText blockHash')
    blockHsh@(BlockHash blockHash') = nullBlockHash
    blockTime = toInt64 [timeMicrosQQ| 2019-12-17T01:00:00.0 |]
    toInt64 (Time (TimeSpan (Micros m))) = m
    blockHeight' = 123
    logger = newLogger neverLog ""

testCoinbase797Fix :: Assertion
testCoinbase797Fix = do
    (pdb,mc) <- loadCC
    void $ applyCoinbase toyVersion logger pdb miner 0.1 pubData blockHsh
     (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) mc
  where
    miner = noMiner
    pubData = PublicData def blockHeight' blockTime (toText blockHash')
    blockHsh@(BlockHash blockHash') = nullBlockHash
    blockTime = toInt64 [timeMicrosQQ| 2019-12-16T01:00:00.0 |]
    toInt64 (Time (TimeSpan (Micros m))) = m
    blockHeight' = 123
    logger = newLogger neverLog ""

testCoinbaseNewBlock :: Assertion
testCoinbaseNewBlock = do
    (pdb,mc) <- loadCC
    r <- try $ applyCoinbase toyVersion logger pdb miner 0.1 pubData blockHsh
      (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) mc
    case r of
      Left (e :: SomeException) -> assertFailure $ "Coinbase tx failed: " <> show e
      Right (CommandResult _ _ (PactResult (Left e)) _ _ _ _) ->
        if isInfixOf "exploitable" (sshow e) then
          return ()
        else assertFailure (show e)
      Right cr -> assertFailure $ "Code succeeded and did not fail" <> show cr
  where
    miner = Miner
      (MinerId "NoMiner\" (read-keyset \"miner-keyset\") 1000.0)(module m g (defcap g () true ) (defun k () (enforce false \"exploitable\"))) (m.k) (coin.coinbase \"NoMiner")
      (MinerKeys $ mkKeySet [] "<")
    pubData = PublicData def blockHeight' blockTime (toText blockHash')
    blockHsh@(BlockHash blockHash') = nullBlockHash
    blockTime = toInt64 [timeMicrosQQ| 2019-12-16T01:00:00.0 |]
    toInt64 (Time (TimeSpan (Micros m))) = m
    blockHeight' = 123
    logger = newLogger neverLog ""

testCoinbaseEnforceFailure :: Assertion
testCoinbaseEnforceFailure = do
    (pdb,mc) <- loadCC
    r <- try $ applyCoinbase toyVersion logger pdb miner 0.1 pubData blockHsh
      (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) mc
    case r of
      Left (e :: SomeException) ->
        if isInfixOf "Coinbase tx failure" (sshow e) then
          return ()
        else assertFailure $ "Coinbase failed for unknown reason: " <> show e
      Right _ -> assertFailure "Coinbase did not failure on bad miner id"
  where
    miner = Miner (MinerId "") (MinerKeys $ mkKeySet [] "<")
    pubData = PublicData def blockHeight' blockTime (toText blockHash')
    blockHsh@(BlockHash blockHash') = nullBlockHash
    blockTime = toInt64 [timeMicrosQQ| 2019-12-17T01:00:00.0 |]
    toInt64 (Time (TimeSpan (Micros m))) = m
    blockHeight' = 123
    logger = newLogger neverLog ""
