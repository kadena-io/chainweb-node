{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.String.Conv
import Data.Text hiding (head)
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

import Chainweb.BlockHash (nullBlockHash)
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.SPV.CreateProof
import Chainweb.SPV
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version

import Data.CAS
import Data.CAS.HashMap (HashMapCas)
import Data.LogMessage


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

runRST :: Monad m => ReaderT r (StateT s m) a -> r -> s -> m a
runRST action pse st = fmap fst $
  runStateT (runReaderT action pse) st

runRST' :: Monad m => r -> s -> ReaderT r (StateT s m) a -> m a
runRST' r s k = runRST k r s

toOutputBytes :: FullLogTxOutput -> TransactionOutput
toOutputBytes = TransactionOutput . toS . encode . toHashedLogTxOutput

txPairs :: Getter Transactions (V.Vector (Transaction, FullLogTxOutput))
txPairs = to _transactionPairs

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
        fmap (\bs -> PayloadWithText bs (_cmdPayload c')) c
      ProcFail e -> throwM . userError $ e

-- -------------------------------------------------------------------------- --
-- SPV Tests

spvIntegrationTest :: ChainwebVersion -> Step -> IO ()
spvIntegrationTest v step = do
    step "setup cutdb"
    withTestCutDb v 0 (\_ _ -> return ()) $ \cutDb -> do
      step "setup pact service and spv support"
      withPactSetup cutDb $ \pse st -> do

        step "build delete-coin command from test credentials"
        let cid = ChainId 0

        ks <- testKeyPairs
        t <- deleteCoinCmd ks cid

        step "execute delete-coin command, generating pre-spv pact proof"
        void $! runRST' pse st $
          execTransactions (Just nullBlockHash) defaultMiner (V.singleton t)

        step "create chainweb spv proof from pre-spv pact proof"
        -- we only have 1 tx in list of returns, so this is fine
        -- let v = toOutputBytes $ u ^?! txPairs . traverse . _2
        p <- createTransactionOutputProof cutDb cid cid (BlockHeight 0) 1

        step "build create-coin command from test creds and spv proof"
        w <- createCoinCmd ks p
        void $! runRST' pse st $
          execTransactions (Just nullBlockHash) defaultMiner (V.singleton w)
