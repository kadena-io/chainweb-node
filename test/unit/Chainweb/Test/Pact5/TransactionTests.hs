{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Test.Pact5.TransactionTests (tests) where

import Chainweb.Pact5.Templates
import Chainweb.Miner.Pact
import Control.Lens hiding ((.=))
import Data.Foldable
import Data.Text (unpack)
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Info
import Pact.Core.Pretty
import Pact.Core.Repl
import Pact.Core.Repl.Utils
import Control.Monad (when)
import Data.Text qualified as Text
import Pact.Types.KeySet qualified as Pact4
import Test.Tasty
import Test.Tasty.HUnit
import Data.Map.Strict qualified as Map

-- ---------------------------------------------------------------------- --
-- Global settings

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact5.TransactionTests"
  [ testCase "coin contract v6" $ runReplTest coinReplV6
  , testCase "namespace v1" $ runReplTest nsReplV1
  , testCase "namespace v2" $ runReplTest nsReplV2
  , testCase "miner key injection" injectionTest
  ]

coinReplV6 :: FilePath
coinReplV6 = "pact/pact5/coin-contract/coin.repl"

nsReplV1 :: FilePath
nsReplV1 = "pact/pact5/namespaces/v1/ns.repl"

nsReplV2 :: FilePath
nsReplV2 = "pact/pact5/namespaces/ns.repl"

runReplTest :: FilePath -> Assertion
runReplTest file = do
  (scriptout, rstate) <- execScript False False file
  case scriptout of
    Left e -> failWithErr rstate e
    Right _ -> do
      traverse_ (ensurePassing rstate) (reverse (_replTestResults rstate))
  where
  failWithErr rstate err = do
    let (FileLocSpanInfo f _) = err ^. peInfo
    case Map.lookup f (_replLoadedFiles rstate) of
      Just src -> do
        assertFailure $ unpack $ replError src err
      Nothing -> do
        assertFailure $ renderCompactString err
  ensurePassing rstate (ReplTestResult _testName _loc res) = case res of
    ReplTestPassed -> pure ()
    ReplTestFailed msg ->
      failWithErr rstate (PEExecutionError (EvalError msg) [] _loc)

injectionTest :: Assertion
injectionTest = do
  let (expr, pv) = mkCoinbaseTerm badMinerId minerKeys0 1.0

  assertEqual "Precompiled exploit yields correct code" (renderText expr) $
    "(coin.coinbase \"alpha\" (read-keyset \"miner-keyset\") 9999999.99)"
    <> "(coin.coinbase \"alpha\" (read-keyset \"miner-keyset\") (read-decimal \"reward\"))"

  let stmt = renderText pv

  when ("coinbase" `Text.isInfixOf` stmt) $
    assertFailure "Precompiled statement contains exploitable code"

  when ("read-keyset" `Text.isInfixOf` stmt) $
    assertFailure "Precompiled statement contains exploitable code"

badMinerId :: MinerId
badMinerId = MinerId "alpha\" (read-keyset \"miner-keyset\") 9999999.99)(coin.coinbase \"alpha"

minerKeys0 :: MinerKeys
minerKeys0 = MinerKeys $ Pact4.mkKeySet
    ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
    "default"
