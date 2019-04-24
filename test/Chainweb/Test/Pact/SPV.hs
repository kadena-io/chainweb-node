{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Lens hiding ((.=))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)

import Data.Aeson
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)

-- internal pact modules

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Server
import Pact.Types.RPC

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.SPV.CreateProof
import Chainweb.SPV.VerifyProof
import Chainweb.Test.CutDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

tests :: RocksDb -> TestTree
tests rdb = testGroup "SPV-Pact Integration Tests"
  [ testCaseStepsN "SPV Roundtrip" 10 (spvIntegrationTest rdb version)
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
    cids <- generate (shuffle $ toList $ chainIds c)
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

runRST :: Monad m => ReaderT r (StateT s m) a -> r -> s -> m a
runRST action pse st = fmap fst $
  runStateT (runReaderT action pse) st

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

    let spv = pactSpvSupport mv

    let pse = PactServiceEnv Nothing cpe spv def
        pss = PactServiceState st Nothing

    initCC pse pss >> f pse st
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

    initCC = runRST $
      initialPayloadState Testnet00 $ someChainId Testnet00

createCoinCmd :: Transaction -> IO (ExecMsg ParsedCode)
createCoinCmd tx = buildExecParsedCode spvData
    [text| (create-coin (read-msg 'proof)) |]
  where
    spvData = Just $ object
      [ "proof" .= encodeToText tx
      ]

-- -------------------------------------------------------------------------- --
-- SPV Tests

spvIntegrationTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvIntegrationTest rdb v step = do
    step "setup pact service and spv support"
    withTestCutDb rdb v 100 (\_ _ -> return ()) $ \cutDb -> do
      withPactSetup cutDb $  \_pse _st -> do
        step "pick random transaction"
        (h, txIx, _, _) <- randomTransaction cutDb

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

        step "verify transaction proof"
        t <- verifyTransactionProof cutDb proof

        step "build spv creation command from tx"
        _cmd <- createCoinCmd t

        step "execute spv command"
        undefined
