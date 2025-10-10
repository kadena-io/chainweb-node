{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- I have no idea why this warning is being triggered
-- for things that are clearly used
{-# options_ghc -fno-warn-unused-top-binds #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Test func in TransactionExec
--
module Chainweb.Test.Pact4.TransactionTests ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (readMVar)
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (intercalate)
import Data.Text (Text,isInfixOf,unpack)
import qualified System.LogLevel as L

-- internal pact modules

import Pact.Interpreter
import Pact.Parse
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime

-- internal chainweb modules

import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact4.Templates
import Chainweb.Pact4.TransactionExec

import Chainweb.Utils
import Chainweb.Version as V
import Chainweb.Version.RecapDevelopment
import qualified Chainweb.Pact4.ModuleCache as Pact4
import qualified Pact.Core.Guards as Pact5
import qualified Data.Set as Set
import qualified Pact.Core.Names as Pact5


-- ---------------------------------------------------------------------- --
-- Global settings

v :: ChainwebVersion
v = RecapDevelopment

coinReplV1 :: FilePath
coinReplV1 = "pact/coin-contract/v1/coin.repl"

coinReplV4 :: FilePath
coinReplV4 = "pact/coin-contract/v4/coin-v4.repl"

coinReplV5 :: FilePath
coinReplV5 = "pact/coin-contract/v5/coin-v5.repl"

coinReplV6 :: FilePath
coinReplV6 = "pact/coin-contract/coin.repl"

nsReplV1 :: FilePath
nsReplV1 = "pact/namespaces/v1/ns.repl"

nsReplV2 :: FilePath
nsReplV2 = "pact/namespaces/ns.repl"

logger :: GenericLogger
#if DEBUG_TEST
logger = genericLogger L.Info (step . T.unpack)
#else
logger = genericLogger L.Error (\_ -> return ())
#endif

-- ---------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact4.TransactionTests"
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
      , testCase "v6" (ccReplTests coinReplV6)
      ]
    , testGroup "Namespace repl unit tests"
      [ testCase "Ns-v1 repl tests" $ ccReplTests nsReplV1
      , testCase "Ns-v2 repl tests" $ ccReplTests nsReplV2
      ]
    , testCase "Payer Repl Tests" (ccReplTests "pact/gas-payer/gas-payer-v1.repl")
    ]
  , testGroup "Precompiled Statements Tests"
    [ testCase "Basic Injection Test" baseInjTest
    , testCase "Fixed Injection Test" fixedInjTest
    ]
  -- , testGroup "Coinbase Vuln Fix Tests"
  --   [ testCoinbase797DateFix
  --   , testCase "testCoinbaseEnforceFailure" testCoinbaseEnforceFailure
  --   , testCase "testCoinbaseUpgradeDevnet0" (testCoinbaseUpgradeDevnet (unsafeChainId 0) 3)
  --   , testCase "testCoinbaseUpgradeDevnet1" (testCoinbaseUpgradeDevnet (unsafeChainId 1) 4)
  --   ]
  -- , testGroup "20-Chain Fork Upgrade Tests"
  --   [ testTwentyChainDevnetUpgrades
  --   ]
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

loadCC :: FilePath -> IO (PactDbEnv LibState, Pact4.ModuleCache)
loadCC = loadScript

loadScript :: FilePath -> IO (PactDbEnv LibState, Pact4.ModuleCache)
loadScript fp = do
  (r, rst) <- execScript' Quiet fp
  either fail (const $ return ()) r
  let pdb = PactDbEnv
            (view (rEnv . eePactDb) rst)
            (view (rEnv . eePactDbVar) rst)
      mc = view (rEvalState . evalRefs . rsLoadedModules) rst
  -- TODO: setup eval env & run the code & and pass
  return (pdb, Pact4.moduleCacheFromHashMap mc)

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
buildExecWithData = void $ buildExecParsedCode maxBound
  (Just $ object [ "data" .= (1 :: Int) ]) "(+ 1 1)"

buildExecWithoutData :: Assertion
buildExecWithoutData = void $ buildExecParsedCode maxBound Nothing "(+ 1 1)"

badMinerId :: MinerId
badMinerId = MinerId "alpha\" (read-keyset \"miner-keyset\") 9999999.99)(coin.coinbase \"alpha"

minerKeys0 :: MinerGuard
minerKeys0 = MinerGuard $ Pact5.GKeyset $ Pact5.KeySet
    (Set.fromList [Pact5.PublicKeyText "f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"])
    (Pact5.CustomPredicate (fromJuste $ Pact5.parseParsedTyName "default"))

matchLogs :: [(Text, Text, Maybe Value)] -> [(Text, Text, Maybe Value)] -> IO ()
matchLogs expectedResults actualResults
    | length actualResults /= length expectedResults = void $
      assertFailure $ intercalate "\n"
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

logResults :: [TxLogJson] -> [(Text, Text, Maybe Value)]
logResults = fmap go
  where
    go x = case decodeTxLogJson x of
        Left e -> error $ "unable to parse TxLog: " <> show e
        Right (r :: TxLog Value) -> f r
    f l =
      ( _txDomain l
      , _txKey l
      -- This lens is because some of the transacctions happen post 420 fork
      -- So the object representation changes due to the RowData type.
      , l ^? txValue . _Object . (ix "balance" `failing` ix "$d" . _Object . ix "balance")
      )
