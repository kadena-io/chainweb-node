{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Applicative((<|>))
import Control.Concurrent (readMVar)
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (intercalate)
import Data.Text (Text,isInfixOf,unpack)
import Data.Default

-- internal pact modules

import Pact.Gas
import Pact.Interpreter
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


-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Miner.Pact
import Chainweb.Pact.Templates
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version as V
import Chainweb.Test.Pact.Utils


-- ---------------------------------------------------------------------- --
-- Global settings

v :: ChainwebVersion
v = Development

coinReplV1 :: FilePath
coinReplV1 = "pact/coin-contract/coin.repl"

coinReplV4 :: FilePath
coinReplV4 = "pact/coin-contract/v4/coin-v4.repl"

coinReplV5 :: FilePath
coinReplV5 = "pact/coin-contract/v5/coin-v5.repl"

logger :: Logger
#if DEBUG_TEST
logger = newLogger alwaysLog ""
#else
logger = newLogger neverLog ""
#endif

-- ---------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.TransactionTests"
  [ testGroup "Pact Command Parsing"
    [ testCase "Build Exec with Data" buildExecWithData
    , testCase "Build Exec without Data" buildExecWithoutData
    ]
  , testGroup "Pact Code Unit Tests"
    [ testGroup "Coin Contract repl tests"
      [ testCase "v1" (ccReplTests coinReplV1)
        -- v2 and v3 repl tests were consolidated in v4
      , testCase "v4" (ccReplTests coinReplV4)
      , testCase "v5" (ccReplTests coinReplV5)
      ]
    , testCase "Ns Repl Tests" (ccReplTests "pact/namespaces/ns.repl")
    , testCase "Payer Repl Tests" (ccReplTests "pact/gas-payer/gas-payer-v1.repl")
    ]
  , testGroup "Precompiled Statements Tests"
    [ testCase "Basic Injection Test" baseInjTest
    , testCase "Fixed Injection Test" fixedInjTest
    ]
  , testGroup "Coinbase Vuln Fix Tests"
    [ testCoinbase797DateFix
    , testCase "testCoinbaseEnforceFailure" testCoinbaseEnforceFailure
    , testCase "testCoinbaseUpgradeDevnet0" (testCoinbaseUpgradeDevnet (unsafeChainId 0) 3)
    , testCase "testCoinbaseUpgradeDevnet1" (testCoinbaseUpgradeDevnet (unsafeChainId 1) 4)
    ]
  , testGroup "20-Chain Fork Upgrade Tests"
    [ testTwentyChainDevnetUpgrades
    ]
  ]

-- ---------------------------------------------------------------------- --
-- Coin Contract repl tests

ccReplTests :: FilePath -> Assertion
ccReplTests ccFile = do
    (r, rst) <- execScript' Quiet ccFile
    either fail (\_ -> execRepl rst) r
  where
    execRepl rst = do
      lst <- readMVar $! _eePactDbVar . _rEnv $ rst
      for_ (_rlsTests lst) $ \tr ->
        traverse_ (uncurry failCC) $ trFailure tr

    failCC i e = assertFailure $ renderInfo (_faInfo i) <> ": " <> unpack e

loadCC :: FilePath -> IO (PactDbEnv LibState, ModuleCache)
loadCC = loadScript

loadScript :: FilePath -> IO (PactDbEnv LibState, ModuleCache)
loadScript fp = do
  (r, rst) <- execScript' Quiet fp
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
buildExecWithData = void $ buildExecParsedCode Nothing
  (Just $ object [ "data" .= (1 :: Int) ]) "(+ 1 1)"

buildExecWithoutData :: Assertion
buildExecWithoutData = void $ buildExecParsedCode Nothing Nothing "(+ 1 1)"

badMinerId :: MinerId
badMinerId = MinerId "alpha\" (read-keyset \"miner-keyset\") 9999999.99)(coin.coinbase \"alpha"

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ mkKeySet
    ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
    "default"

-- ---------------------------------------------------------------------- --
-- Vuln 792 fork tests

testCoinbase797DateFix :: TestTree
testCoinbase797DateFix = testCaseSteps "testCoinbase791Fix" $ \step -> do
    (pdb,mc) <- loadCC coinReplV1

    step "pre-fork code injection succeeds, no enforced precompile"

    cmd <- buildExecParsedCode Nothing Nothing "(coin.get-balance \"tester01\")"

    doCoinbaseExploit pdb mc preForkHeight cmd False $ \case
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 1000.1 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "post-fork code injection fails, no enforced precompile"

    cmd' <- buildExecParsedCode Nothing Nothing
      "(coin.get-balance \"tester01\\\" (read-keyset \\\"miner-keyset\\\") 1000.0)(coin.coinbase \\\"tester01\")"

    doCoinbaseExploit pdb mc postForkHeight cmd' False $ \case
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.1 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "pre-fork code injection fails, enforced precompile"

    doCoinbaseExploit pdb mc preForkHeight cmd' True $ \case
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.2 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

    step "post-fork code injection fails, enforced precompile"

    doCoinbaseExploit pdb mc postForkHeight cmd' True $ \case
      Left _ -> assertFailure "local call to get-balance failed"
      Right (PLiteral (LDecimal d))
        | d == 0.3 -> return ()
        | otherwise -> assertFailure $ "miner balance is incorrect: " <> show d
      Right l -> assertFailure $ "wrong return type: " <> show l

  where
    doCoinbaseExploit pdb mc height localCmd precompile testResult = do
      let ctx = TxContext (mkTestParentHeader $ height - 1) def

      void $ applyCoinbase Mainnet01 logger pdb miner 0.1 ctx
        (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled precompile) mc

      let h = H.toUntypedHash (H.hash "" :: H.PactHash)
          tenv = TransactionEnv Transactional pdb logger Nothing def
            noSPVSupport Nothing 0.0 (RequestKey h) 0 def
          txst = TransactionState mempty mempty 0 Nothing (_geGasModel freeGasEnv)

      CommandResult _ _ (PactResult pr) _ _ _ _ _ <- evalTransactionM tenv txst $!
        applyExec 0 defaultInterpreter localCmd [] h permissiveNamespacePolicy

      testResult pr

    miner = Miner
      (MinerId "tester01\" (read-keyset \"miner-keyset\") 1000.0)(coin.coinbase \"tester01")
      (MinerKeys $ mkKeySet ["b67e109352e8e33c8fe427715daad57d35d25d025914dd705b97db35b1bfbaa5"] "keys-all")

    preForkHeight = 121451
    postForkHeight = 121452

    -- | someBlockHeader is a bit slow for the vuln797Fix to trigger. So, instead
    -- of mining a full chain we fake the height.
    --
    mkTestParentHeader :: BlockHeight -> ParentHeader
    mkTestParentHeader h = ParentHeader $ (someBlockHeader (FastTimedCPM singleton) 10)
        { _blockHeight = h }


testCoinbaseEnforceFailure :: Assertion
testCoinbaseEnforceFailure = do
    (pdb,mc) <- loadCC coinReplV4
    r <- tryAllSynchronous $ applyCoinbase toyVersion logger pdb badMiner 0.1 (TxContext someParentHeader def)
      (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) mc
    case r of
      Left e ->
        if isInfixOf "CoinbaseFailure" (sshow e) then
          return ()
        else assertFailure $ "Coinbase failed for unknown reason: " <> show e
      Right _ -> assertFailure "Coinbase did not fail for bad miner id"
  where
    badMiner = Miner (MinerId "") (MinerKeys $ mkKeySet [] "<")
    blockHeight' = 123
    someParentHeader = ParentHeader $ someTestVersionHeader
      { _blockHeight = blockHeight'
      , _blockCreationTime = BlockCreationTime [timeMicrosQQ| 2019-12-10T01:00:00.0 |]
      }

testCoinbaseUpgradeDevnet :: V.ChainId -> BlockHeight -> Assertion
testCoinbaseUpgradeDevnet cid upgradeHeight =
    testUpgradeScript "test/pact/coin-and-devaccts.repl" cid upgradeHeight test
  where
    test (T2 cr mcm) = case (_crLogs cr,mcm) of
      (_,Nothing) -> assertFailure "Expected module cache from successful upgrade"
      (Nothing,_) -> assertFailure "Expected logs from successful upgrade"
      (Just logs,_) ->
        matchLogs (logResults logs) expectedResults

    expectedResults =
      [ ("USER_coin_coin-table", "NoMiner", Just (Number 0.1))
      , ("SYS_modules","fungible-v2",Nothing)
      , ("SYS_modules","coin",Nothing)
      , ("USER_coin_coin-table","sender07",Just (Number 998662.3))
      , ("USER_coin_coin-table","sender09",Just (Number 998662.1))
      ]

testTwentyChainDevnetUpgrades :: TestTree
testTwentyChainDevnetUpgrades = testCaseSteps "Test 20-chain Devnet upgrades" $ \step -> do
      step "Check that 20-chain upgrades fire at block height 150"
      testUpgradeScript "test/pact/twenty-chain-upgrades.repl" (unsafeChainId 0) V.to20ChainsDevelopment test0

      step "Check that 20-chain upgrades do not fire at block heights < 150 and > 150"
      testUpgradeScript "test/pact/twenty-chain-upgrades.repl" (unsafeChainId 0) (V.to20ChainsDevelopment - 1) test1
      testUpgradeScript "test/pact/twenty-chain-upgrades.repl" (unsafeChainId 0) (V.to20ChainsDevelopment + 1) test1

      step "Check that 20-chain upgrades do not fire at on other chains"
      testUpgradeScript "test/pact/twenty-chain-upgrades.repl" (unsafeChainId 1) V.to20ChainsDevelopment test1

      step "Check that 20-chain upgrades succeed even if e7f7 balance is insufficient"
      testUpgradeScript "test/pact/twenty-chain-insufficient-bal.repl" (unsafeChainId 0) V.to20ChainsDevelopment test1
  where
    test0 (T2 cr _) = case _crLogs cr of
      Just logs -> matchLogs (logResults logs)
        [ ("USER_coin_coin-table","NoMiner",Just (Number 0.1))
        , ( "USER_coin_coin-table"
          , "e7f7634e925541f368b827ad5c72421905100f6205285a78c19d7b4a38711805"
          , Just (Number 50.0) -- 100.0 tokens remediated
          )
        ]
      Nothing -> assertFailure "Expected logs from upgrade"

    test1 (T2 cr _) = case _crLogs cr of
      Just logs -> matchLogs (logResults logs)
        [ ("USER_coin_coin-table", "NoMiner", Just (Number 0.1))
        ]
      Nothing -> assertFailure "Expected logs from upgrade"

-- ---------------------------------------------------------------------- --
-- Utils

testUpgradeScript
    :: FilePath
    -> V.ChainId
    -> BlockHeight
    -> (T2 (CommandResult [TxLog Value]) (Maybe ModuleCache) -> IO ())
    -> IO ()
testUpgradeScript script cid bh test = do
    (pdb, mc) <- loadScript script
    r <- tryAllSynchronous $ applyCoinbase v logger pdb noMiner 0.1 (TxContext p def)
        (EnforceCoinbaseFailure True) (CoinbaseUsePrecompiled False) mc
    case r of
      Left e -> assertFailure $ "tx execution failed: " ++ show e
      Right cr -> test cr
  where
    p = parent bh cid

matchLogs :: [(Text, Text, Maybe Value)] -> [(Text, Text, Maybe Value)] -> IO ()
matchLogs expectedResults actualResults
    | length actualResults /= length expectedResults = void $
      assertFailure $ intercalate "\n" $
        [ "matchLogs: length mismatch "
          <> show (length actualResults) <> " /= " <> show (length expectedResults)
        , "actual: " ++ show actualResults
        , "expected: " ++ show expectedResults
        ]
    | otherwise = void $ zipWithM matchLog actualResults expectedResults
  where
    matchLog actual expected = do
      (assertEqual "domain matches" `on` view _1) actual expected
      (assertEqual "key matches" `on` view _2) actual expected
      (assertEqual "balance matches" `on` view _3) actual expected

parent :: BlockHeight -> V.ChainId -> ParentHeader
parent bh cid = ParentHeader $ (someBlockHeader v bh)
    { _blockChainwebVersion = v
    , _blockChainId = cid
    , _blockHeight = pred bh
    }

logResults :: [TxLog Value] -> [(Text, Text, Maybe Value)]
logResults = fmap f
  where
    f l =
      ( _txDomain l
      , _txKey l
      -- This lens is because some of the transacctions happen post 420 fork
      -- So the object representation changes due to the RowData type.
      , l ^? txValue . _Object . ix "balance" <|> l ^? txValue . _Object . ix "$d" . _Object . ix "balance"
      )
