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

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import NeatInterpolation (text)
import Numeric.Natural

import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)

import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Default (def)
import Data.Foldable
import Data.Functor (void)
import Data.Text
import qualified Data.Vector as V

-- internal pact modules

import Pact.Gas
import Pact.Interpreter
import Pact.Parse
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Server
import Pact.Types.RPC
import Pact.Types.Runtime

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.SPV.CreateProof
import Chainweb.SPV
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version


tests :: TestTree
tests = testGroup "SPV-Pact Integration Tests"
  [ testCaseStepsN "SPV Roundtrip" 1 (spvIntegrationTest version)
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

runRST :: Monad m => ReaderT r (StateT s m) a -> r -> s -> m a
runRST action pse st = fmap fst $
  runStateT (runReaderT action pse) st

-- -------------------------------------------------------------------------- --
-- Pact Service setup


withPactSetup :: CutDb cas -> (PactServiceEnv -> PactServiceState -> IO a) -> IO a
withPactSetup cdb f = do
    let l = newLogger alwaysLog (LogName "pact-spv")
        conf = toCommandConfig $ pactDbConfig (Test petersonChainGraph)
        genv = GasEnv 0 0.0 (constGasModel 0)

    (cpe, st) <- initConf conf l genv
    void $ saveInitial (_cpeCheckpointer cpe) st

    mv <- newMVar cdb

    let spv = pactSpvSupport mv
        pss = PactServiceState st Nothing

    let pse = PactServiceEnv Nothing cpe spv def

    initCC pse pss >> f pse pss
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

    initCC = runRST $ initialPayloadState Testnet00 (unsafeChainId 0)

createCoinCmd
    :: [SomeKeyPair]
    -> (TransactionOutputProof SHA512t_256)
    -> IO ChainwebTransaction
createCoinCmd ks tx = mkPactTx ccData
    [text| (coin.create-coin (read-msg 'proof)) |]
  where
    ccData = Just $ object
      [ "proof" .= encodeToText tx
      , "test-admin-keyset" .= fmap formatB16PubKey ks
      ]

deleteCoinCmd
    :: [SomeKeyPair]
    -> ChainId
    -> IO ChainwebTransaction
deleteCoinCmd ks cid = mkPactTx dcData
    [text|
      (coin.delete-coin $sender00 $cid' $sender01 (read-keyset 'test-admin-keyset) 1.0)
      |]
  where
    sender00 = "sender00"
    sender01 = "sender01"

    cid' = pack . show $ cid

    dcData = Just $ object
      [ "test-admin-keyset" .= fmap formatB16PubKey ks
      ]

mkPactTx :: Maybe Value -> Text -> IO ChainwebTransaction
mkPactTx v t = do
    ks <- testKeyPairs

    let pm = PublicMeta "0" "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)
        d = maybe Null id v

    c <- mkCommand ks pm "1" $ Exec (ExecMsg t d)
    case verifyCommand c of
      ProcSucc c' -> return $
        fmap (\bs -> PayloadWithText bs (c' ^. cmdPayload)) c
      ProcFail e -> throwM . userError $ e


-- -------------------------------------------------------------------------- --
-- SPV Tests

spvIntegrationTest :: ChainwebVersion -> Step -> IO ()
spvIntegrationTest v step = do
    step "setup cutdb"
    withTestCutDb v 0 (\_ _ -> return ()) $ \cutDb -> do
      step "setup pact service and spv support"
      withPactSetup cutDb $ \pse st -> do
        step "pick random transaction"
        (h, outIx, _, _) <- randomTransaction cutDb

        step "pick a reachable target chain"
        curCut <- _cut cutDb
        trgChain <- targetChain curCut h

        let cid = _chainId h
            bhe = _blockHeight h
            bha = _blockHash h

        step "create inclusion proof for transaction"
        proof <- createTransactionOutputProof
          cutDb
          -- cutdb
          trgChain
          -- target chain
          cid
          -- source chain id
          bhe
          -- source block height
          outIx
          -- transaction index

        step "build spv creation command from tx output proof"
        ks <- testKeyPairs
        t <- createCoinCmd ks proof

        step "execute spv command"

        let pse' = pse
              & set (psPublicData . pdChainId) (unsafeGetChainId cid)
              & set (psPublicData . pdBlockHeight) (int bhe)

        let u = V.singleton t
        let ex = execTransactions (Just bha) defaultMiner u

        void $! runRST ex pse' st
