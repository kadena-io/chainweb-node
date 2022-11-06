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
{-# LANGUAGE TypeOperators #-}
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
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload.PayloadStore
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)

import Data.LogMessage

import GHC.Stack (HasCallStack)

-- | Initialization for Pact (in process) Api
withPactService
    :: PayloadCasLookup cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MempoolConsensus
    -> BlockHeaderDb
    -> PayloadDb cas
    -> FilePath
    -> PactServiceConfig
    -> (PactQueue -> IO a)
    -> IO a
withPactService ver cid logger mpc bhdb pdb pactDbDir config action =
    withSqliteDb cid logger pactDbDir (_pactResetDb config) $ \sqlenv ->
        withPactService' ver cid logger mpa bhdb pdb sqlenv config action
  where
    mpa = pactMemPoolAccess mpc $ addLabel ("sub-component", "MempoolAccess") logger

-- | Alternate Initialization for Pact (in process) Api, only used directly in
--   tests to provide memPool with test transactions
withPactService'
    :: PayloadCasLookup cas
    => Logger logger
    => HasCallStack
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> PactServiceConfig
    -> (PactQueue -> IO a)
    -> IO a
withPactService' ver cid logger memPoolAccess bhDb pdb sqlenv config action = do
    reqQ <- newPactQueue (_pactQueueSize config)
    race (concurrently_ (monitor reqQ) (server reqQ)) (client reqQ) >>= \case
        Left () -> error "Chainweb.Pact.Service.PactInProcApi: pact service terminated unexpectedly"
        Right a -> return a
  where
    client reqQ = action reqQ
    server reqQ = runForever logg "pact-service"
        $ PS.initPactService ver cid logger reqQ memPoolAccess bhDb pdb sqlenv config
    logg = logFunction logger
    monitor = runPactServiceQueueMonitor $ addLabel ("sub-component", "PactQueue") logger

runPactServiceQueueMonitor :: Logger logger => logger ->  PactQueue -> IO ()
runPactServiceQueueMonitor l pq = do
    let lf = logFunction l
    logFunctionText l Info "Initialized PactQueueMonitor"
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
    -> (MempoolPreBlockCheck ChainwebTransaction
            -> BlockHeight
            -> BlockHash
            -> BlockHeader
            -> IO (Vector ChainwebTransaction))
pactMemPoolGetBlock mpc theLogger bf validate height hash _bHeader = do
    logFn theLogger Info $! "pactMemPoolAccess - getting new block of transactions for "
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
