{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Service.PactInProcApi
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution (in-process) API for Chainweb

module Chainweb.Pact.Service.PactInProcApi
    ( withPactService
    , withPactService'
    , pactMemPoolAccess
    ) where

import Control.Concurrent.Async

import Data.IORef
import Data.Vector (Vector)

import System.LogLevel

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool

import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload.PayloadStore
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

import GHC.Stack (HasCallStack)
import Chainweb.Counter (Counter)
import Chainweb.BlockCreationTime
import Chainweb.Pact.Backend.Types

-- | Initialization for Pact (in process) Api
withPactService
    :: CanReadablePayloadCas tbl
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe (Counter "txFailures")
    -> MempoolConsensus
    -> BlockHeaderDb
    -> PayloadDb tbl
    -> FilePath
    -> PactServiceConfig
    -> (PactQueue -> IO a)
    -> IO a
withPactService ver cid logger txFailuresCounter mpc bhdb pdb pactDbDir config action =
    withSqliteDb cid logger pactDbDir (_pactResetDb config) $ \sqlenv ->
        withPactService' ver cid logger txFailuresCounter mpa bhdb pdb sqlenv config action
  where
    mpa = pactMemPoolAccess mpc $ addLabel ("sub-component", "MempoolAccess") logger

-- | Alternate Initialization for Pact (in process) Api, only used directly in
--   tests to provide memPool with test transactions
withPactService'
    :: CanReadablePayloadCas tbl
    => Logger logger
    => HasCallStack
    => ChainwebVersion
    -> ChainId
    -> logger
    -> Maybe (Counter "txFailures")
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb tbl
    -> SQLiteEnv
    -> PactServiceConfig
    -> (PactQueue -> IO a)
    -> IO a
withPactService' ver cid logger txFailuresCounter memPoolAccess bhDb pdb sqlenv config action = do
    reqQ <- newPactQueue (_pactQueueSize config)
    race (concurrently_ (monitor reqQ) (server reqQ)) (action reqQ) >>= \case
        Left () -> error "Chainweb.Pact.Service.PactInProcApi: pact service terminated unexpectedly"
        Right a -> return a
  where
    server reqQ = runForever logg "pact-service"
        $ PS.runPactService ver cid logger txFailuresCounter reqQ memPoolAccess bhDb pdb sqlenv config
    logg = logFunction logger
    monitor = runPactServiceQueueMonitor $ addLabel ("sub-component", "PactQueue") logger

runPactServiceQueueMonitor :: Logger logger => logger ->  PactQueue -> IO ()
runPactServiceQueueMonitor l pq = do
    let lf = logFunction l
    runForeverThrottled lf "Chainweb.Pact.Service.PactInProcApi.runPactServiceQueueMonitor" 10 (10 * mega) $ do
            queueStats <- getPactQueueStats pq
            logFunctionText l Debug "got latest set of stats from PactQueueMonitor"
            logFunctionJson l Info queueStats
            resetPactQueueStats pq
            approximateThreadDelay 60_000_000 {- 1 minute -}

pactMemPoolAccess
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> MemPoolAccess
pactMemPoolAccess mpc logger = MemPoolAccess
    { mpaGetBlock = pactMemPoolGetBlock mpc logger
    , mpaSetLastHeader = pactMempoolSetLastHeader mpc logger
    , mpaProcessFork = pactProcessFork mpc logger
    , mpaBadlistTx = mempoolAddToBadList (mpcMempool mpc)
    }

pactMemPoolGetBlock
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> BlockFill
    -> (MempoolPreBlockCheck Pact4.UnparsedTransaction to
            -> BlockHeight
            -> BlockHash
            -> BlockCreationTime
            -> IO (Vector to))
pactMemPoolGetBlock mpc theLogger bf validate height hash _btime = do
    logFn theLogger Debug $! "pactMemPoolAccess - getting new block of transactions for "
        <> "height = " <> sshow height <> ", hash = " <> sshow hash
    mempoolGetBlock (mpcMempool mpc) bf validate height hash
  where
   logFn :: Logger l => l -> LogFunctionText -- just for giving GHC some type hints
   logFn l = logFunction l


pactProcessFork
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> (BlockHeader -> IO ())
pactProcessFork mpc theLogger bHeader = do
    let forkFunc = (mpcProcessFork mpc) (logFunction theLogger)
    (reintroTxs, validatedTxs) <- forkFunc bHeader
    logFn theLogger Debug $!
        "pactMemPoolAccess - " <> sshow (length reintroTxs) <> " transactions to reintroduce"
    -- No need to run pre-insert check here -- we know these are ok, and
    -- calling the pre-check would block here (it calls back into pact service)
    mempoolInsert (mpcMempool mpc) UncheckedInsert reintroTxs
    mempoolMarkValidated (mpcMempool mpc) validatedTxs

  where
    logFn :: Logger l => l -> LogFunctionText
    logFn lg = logFunction lg

pactMempoolSetLastHeader
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> (BlockHeader -> IO ())
pactMempoolSetLastHeader mpc _theLogger bHeader = do
    let headerRef = mpcLastNewBlockParent mpc
    atomicWriteIORef headerRef (Just bHeader)
