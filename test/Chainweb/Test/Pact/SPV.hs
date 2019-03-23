{-# LANGUAGE QuasiQuotes #-}
module Chainweb.Test.Pact.SPV
( tests
) where

-- |
-- Module: Chainweb.Test.SPV
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- SPV and Pact Service integration tests
--

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

import NeatInterpolation (text)
import Numeric.Natural

import Control.Concurrent.MVar
import Control.Lens hiding (.=)

import Data.Aeson
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)
import Data.Reflection hiding (int)
import qualified Data.Vector as V

-- internal pact modules

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Runtime
import Pact.Types.Server

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MockTx)
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI.Client
import Chainweb.SPV.VerifyProof
import Chainweb.Test.CutDB
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.HashMap hiding (toList)


tests :: TestTree
tests = testGroup "SPV-Pact Integration Tests"
  [ testCaseStepsN "SPV Roundtrip" 10 (spvIntegrationTest version)
  ]
  where
    version = Test petersonChainGraph

-- -------------------------------------------------------------------------- --
-- Utils

type Step = String -> IO ()

testCaseStepsN :: String -> Natural -> (Step -> Assertion) -> TestTree
testCaseStepsN name n t = testGroup name $ fmap steps [1..n]
  where
    steps i = testCaseSteps ("Run test number " <> sshow i) t

-- Find a reachable target chain
--
targetChain :: Cut -> BlockHeader -> IO ChainId
targetChain c srcBlock = do
    cids <- generate (shuffle $ toList $ chainIds_ graph)
    go cids
  where
    graph = _chainGraph c

    go [] = error
        $ "SPV proof test failed to find a reachable target chain. This is a bug in the test code"
        <> ". source block: " <> sshow srcBlock
        <> ". current cut: " <> sshow c
    go (h:t) = if isReachable h then return h else go t

    chainHeight trgChain = _blockHeight (c ^?! ixg trgChain)

    isReachable trgChain
        = _blockHeight srcBlock <= chainHeight trgChain - distance trgChain

    distance x = len $ shortestPath (_chainId srcBlock) x graph


-- -------------------------------------------------------------------------- --
-- Pact Service setup


withPactSetup :: CutDb cas -> (PactServiceEnv -> PactDbState -> IO a) -> IO a
withPactSetup cdb f = do
    let l = newLogger alwaysLog (LogName "pact-spv")
        conf = toCommandConfig $ pactDbConfig (Test petersonChainGraph)
        genv = GasEnv 0 0.0 (constGasModel 0)

    mv <- newMVar cdb
    (cpe, st) <- initConf conf l genv
    void $ saveInitial (cpe ^. cpeCheckpointer) st

    let mpa = \_ _ -> pure V.empty
        spv = pactSpvSupport mv

    let pse = PactServiceEnv mpa cpe spv def

    init >> f pse st
  where
    initConf c l g = case _ccSqlite c of
      Nothing -> do
        e <- mkPureEnv alwaysLog
        cpe <- initInMemoryCheckpointEnv c l g
        st <- mkPureState e c
        pure (cpe,st)
      Just s -> do
        e <- mkSQLiteEnv l False s alwaysLog
        cpe <- initSQLiteCheckpointEnv c l g
        st <- mkSQLiteState e c
        pure (cpe,st)

    init = initialPayloadState Testnet00 (testChainId 0)

buildSpvCmd :: Transaction -> IO Text
buildSpvCmd tx = buildExecParsedCode spvData
    [text| (create-coin (read-msg 'proof)) |]
  where
    spvData = Just $ object
      [ "proof" .= encodeToText tx
      ]

-- -------------------------------------------------------------------------- --
-- SPV Tests

spvIntegrationTest :: ChainwebVersion -> Step -> IO ()
spvIntegrationTest v step = do
    step "setup pact service and spv support"
    withTestCutDb v 100 (\_ _ -> return ()) $ \cutDb -> do
      withPactSetup cutDb $  \pse st -> do
        step "pick random transaction"
        (h, txIx, tx, _) <- randomTransaction cutDb

        step "pick a reachable target chain"
        curCut <- _cut cutDb
        trgChain <- targetChain curCut h

        step "create inclusion proof for transaction"
        proof <- createTransactionProof
          cutDb
              -- cutdb
          trgChain
              -- target chain
          (_chainId h)
              -- source chain id
          (_blockHeight h)
              -- source block height
          txIx
              -- transaction index

        step "build spv creation command from tx"
        cmd <- buildSpvCmd proof

        step "execute cmd in modified environment"
        undefined
