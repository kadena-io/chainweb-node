{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Data.IORef
import Data.Vector (Vector)

import Numeric.Natural (Natural)

import System.LogLevel

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)

import Data.LogMessage

-- | Initialization for Pact (in process) Api
withPactService
    :: PayloadCas cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MempoolConsensus
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> Natural
    -> Natural
    -> (PactQueue -> IO a)
    -> IO a
withPactService ver cid logger mpc bhdb pdb dbDir nodeid resetDb pactQueueSize deepForkLimit action =
    PS.withSqliteDb ver cid logger dbDir nodeid resetDb $ \sqlenv ->
        withPactService' ver cid logger mpa bhdb pdb sqlenv pactQueueSize deepForkLimit action
  where
    mpa = pactMemPoolAccess mpc logger

-- | Alternate Initialization for Pact (in process) Api, only used directly in
--   tests to provide memPool with test transactions
withPactService'
    :: PayloadCas cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MemPoolAccess
    -> BlockHeaderDb
    -> PayloadDb cas
    -> SQLiteEnv
    -> Natural
    -> Natural
    -> (PactQueue -> IO a)
    -> IO a
withPactService' ver cid logger memPoolAccess bhDb pdb sqlenv pactQueueSize deepForkLimit0 action = do
    reqQ <- atomically $ newTBQueue pactQueueSize
    race (server reqQ) (client reqQ) >>= \case
        Left () -> error "pact service terminated unexpectedly"
        Right a -> return a
  where
    deepForkLimit = fromIntegral deepForkLimit0
    client reqQ = action reqQ
    server reqQ = runForever logg "pact-service"
        $ PS.initPactService ver cid logger reqQ memPoolAccess bhDb pdb sqlenv deepForkLimit
    logg = logFunction logger

pactMemPoolAccess
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> MemPoolAccess
pactMemPoolAccess mpc logger = MemPoolAccess
    { mpaGetBlock = pactMemPoolGetBlock mpc logger
    , mpaSetLastHeader = pactMempoolSetLastHeader mpc logger
    , mpaProcessFork = pactProcessFork mpc logger
    }

pactMemPoolGetBlock
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> (MempoolPreBlockCheck ChainwebTransaction
            -> BlockHeight
            -> BlockHash
            -> BlockHeader
            -> IO (Vector ChainwebTransaction))
pactMemPoolGetBlock mpc theLogger validate height hash _bHeader = do
    logFn theLogger Info $! "pactMemPoolAccess - getting new block of transactions for "
        <> "height = " <> sshow height <> ", hash = " <> sshow hash
    mempoolGetBlock (mpcMempool mpc) validate height hash
  where
   logFn :: Logger l => l -> LogFunctionText -- just for giving GHC some type hints
   logFn = logFunction


pactProcessFork
    :: Logger logger
    => MempoolConsensus
    -> logger
    -> (BlockHeader -> IO ())
pactProcessFork mpc theLogger bHeader = do
    let forkFunc = (mpcProcessFork mpc) (logFunction theLogger)
    (reintroTxs, validatedTxs) <- forkFunc bHeader
    (logFn theLogger) Info $! "pactMemPoolAccess - " <> sshow (length reintroTxs)
                           <> " transactions to reintroduce"
    -- No need to run pre-insert check here -- we know these are ok, and
    -- calling the pre-check would block here (it calls back into pact service)
    mempoolInsert (mpcMempool mpc) UncheckedInsert reintroTxs
    mempoolMarkValidated (mpcMempool mpc) $ fmap hasher validatedTxs

  where
    mempool = mpcMempool mpc
    txcfg = mempoolTxConfig mempool
    hasher = txHasher txcfg

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
