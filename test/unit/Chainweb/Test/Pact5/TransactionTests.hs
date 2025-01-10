{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Test.Pact5.TransactionTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens hiding ((.=))

import Data.Foldable
import Data.Text (unpack)
import qualified Data.Map.Strict as Map

-- internal pact modules

import Pact.Core.Repl
import Pact.Core.Pretty
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Info
import Pact.Core.Repl.Utils

-- ---------------------------------------------------------------------- --
-- Global settings

coinReplV6 :: FilePath
coinReplV6 = "pact/pact5/coin-contract/coin.repl"

nsReplV1 :: FilePath
nsReplV1 = "pact/pact5/namespaces/v1/ns.repl"

nsReplV2 :: FilePath
nsReplV2 = "pact/pact5/namespaces/ns.repl"

runReplTest :: FilePath -> Assertion
runReplTest file = do
  (scriptout, rstate) <- execScript False file
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

tests :: TestTree
tests = testGroup "Chainweb.Test.Pact5.TransactionTests"
  [ testCase "coin contract v6" $ runReplTest coinReplV6
  , testCase "namespace v1" $ runReplTest nsReplV1
  , testCase "namespace v2" $ runReplTest nsReplV2
  ]
