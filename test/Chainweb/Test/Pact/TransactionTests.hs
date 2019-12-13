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

import Pact.Gas
import Pact.Parse
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Command
import qualified Pact.Types.Hash as H
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.Interpreter


-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.Miner.Pact
import Chainweb.Pact.Templates
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version


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
  , testGroup "Precompiled Statements Tests"
    [ testCase "Basic Injection Test" baseInjTest
    , testCase "Fixed Injection Test" fixedInjTest
    ]
  , testGroup "Coinbase Vuln Fix Tests"
    [ testCase "testCoinbase" testCoinbase
    , testCoinbase797DateFix
    , testCase "testCoinbaseEnforceFailure" testCoinbaseEnforceFailure
    ]
  ]


-- ---------------------------------------------------------------------- --
-- Coin Contract repl tests

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

-- ---------------------------------------------------------------------- --
-- Template vuln tests

baseInjTest :: Assertion
baseInjTest = mkCoinbaseCmd badMinerId minerKeys0 (ParsedDecimal 1.0) >>= \case
    ExecMsg (ParsedCode pccode _pcexps) _pmdata ->
      assertEqual "Precompiled exploit yields correct code" (unpack pccode) exploit
  where
    exploit = "(coin.coinbase \"alpha\" (read-keyset \"miner-keyset\") 9999999.99)"
      <> "(coin.coinbase \"alpha\" (read-keyset \"miner-keyset\") (read-decimal \"reward\"))"

fixedInjTest :: Assertion
fixedInjTest = case exec of
    ExecMsg (ParsedCode pccode _pcexps) _pmdata
      | isInfixOf "coinbase" pccode -> assertFailure
        $ "Precompiled statement contains exploitable code: "
        <> unpack pccode
      | isInfixOf "read-keyset" pccode -> assertFailure
        $ "Precompiled statement contains exploitable code: "
        <> unpack pccode
      | otherwise -> return ()
  where
    (_, exec) = mkCoinbaseTerm badMinerId minerKeys0 (ParsedDecimal 1.0)


buildExecWithData :: Assertion
buildExecWithData = void $ buildExecParsedCode
  (Just $ object [ "data" .= (1 :: Int) ]) "(+ 1 1)"

buildExecWithoutData :: Assertion
buildExecWithoutData = void $ buildExecParsedCode Nothing "(+ 1 1)"

badMinerId :: MinerId
badMinerId = MinerId ("alpha\" (read-keyset \"miner-keyset\") 9999999.99)(coin.coinbase \"alpha")

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ mkKeySet
    ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
    "default"

-- ---------------------------------------------------------------------- --
-- Vuln 792 fork tests

testCoinbase797DateFix :: TestTree
testCoinbase797DateFix = testCaseSteps "testCoinbase791Fix" $ \step -> do
    (pdb,mc) <- loadCC

    step "pre-fork code injection succeeds, no enforced precompile"

    cmd <- buildExecParsedCode Nothing "(coin.get-balance \"tester01\")"

    doCoinbaseExploit pdb mc preForkTime cmd False $ \pr -> case pr of
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 1000.1 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "post-fork code injection fails, no enforced precompile"

    cmd' <- buildExecParsedCode Nothing
      "(coin.get-balance \"tester01\\\" (read-keyset \\\"miner-keyset\\\") 1000.0)(coin.coinbase \\\"tester01\")"

    doCoinbaseExploit pdb mc postForkTime cmd' False $ \pr -> case pr of
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.1 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "pre-fork code injection fails, enforced precompile"

    doCoinbaseExploit pdb mc preForkTime cmd' True $ \pr -> case pr of
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.2 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "post-fork code injection fails, enforced precompile"

    doCoinbaseExploit pdb mc postForkTime cmd' True $ \pr -> case pr of
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.3 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

  where
    doCoinbaseExploit pdb mc t localCmd precompile testResult = do
      let pd = PublicData def blockHeight' t (toText blockHash')

      void $ applyCoinbase Mainnet01 logger pdb miner 0.1 pd blockHsh
        (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled precompile) mc

      let h = H.toUntypedHash (H.hash "" :: H.PactHash)
          tenv = TransactionEnv Transactional pdb logger def
            noSPVSupport Nothing 0.0 (RequestKey h) 0 permissiveExecutionConfig
          txst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)

      CommandResult _ _ (PactResult pr) _ _ _ _ <- evalTransactionM tenv txst $!
        applyExec defaultInterpreter localCmd [] h permissiveNamespacePolicy

      testResult pr

    miner = Miner
      (MinerId "tester01\" (read-keyset \"miner-keyset\") 1000.0)(coin.coinbase \"tester01")
      (MinerKeys $ mkKeySet ["b67e109352e8e33c8fe427715daad57d35d25d025914dd705b97db35b1bfbaa5"] "keys-all")

    blockHsh@(BlockHash blockHash') = nullBlockHash
    preForkTime = toInt64 [timeMicrosQQ| 2019-12-09T01:00:00.0 |]
    postForkTime = toInt64 [timeMicrosQQ| 2019-12-11T01:00:00.0 |]
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
      Right _ -> assertFailure "Coinbase did not fail for bad miner id"
  where
    miner = Miner (MinerId "") (MinerKeys $ mkKeySet [] "<")
    pubData = PublicData def blockHeight' blockTime (toText blockHash')
    blockHsh@(BlockHash blockHash') = nullBlockHash
    blockTime = toInt64 [timeMicrosQQ| 2019-12-10T01:00:00.0 |]
    toInt64 (Time (TimeSpan (Micros m))) = m
    blockHeight' = 123
    logger = newLogger neverLog ""
