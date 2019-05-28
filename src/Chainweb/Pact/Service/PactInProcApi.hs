{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Data.Int
import Data.IORef
import Data.Vector (Vector)

import System.LogLevel

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion)

import Data.LogMessage

-- | Initialization for Pact (in process) Api
withPactService
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MempoolBackend ChainwebTransaction
    -> MVar (CutDb cas)
    -> (TQueue RequestMsg -> IO a)
    -> IO a
withPactService ver cid logger mpc cdbv action
    = let mpc = MempoolConsensus
            {
            }

  withPactService' ver cid logger (pactMemPoolAccess mpc logger) cdbv action

-- | Alternate Initialization for Pact (in process) Api, only used directly in tests to provide memPool
--   with test transactions
withPactService'
    :: Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MemPoolAccess
    -> MVar (CutDb cas)
    -> (TQueue RequestMsg -> IO a)
    -> IO a
withPactService' ver cid logger memPoolAccess cdbv action = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    a <- async (PS.initPactService ver cid logger reqQ memPoolAccess cdbv)
    link a
    r <- action reqQ
    closeQueue reqQ
    return r

-- TODO: get from config
maxBlockSize :: Int64
maxBlockSize = 10000

pactMemPoolAccess
    :: Logger logger
    => MempoolConsensus ChainwebTransaction
    -> logger
    -> MemPoolAccess
pactMemPoolAccess mpc logger = MemPoolAccess
    { mpaGetBlock = pactMemPoolGetBlock mpc logger
    , mpaSetLastHeader = pactMempoolSetLastHeader mpc logger
    , mpaProcessFork = pactProcessFork mpc logger
    }

pactMemPoolGetBlock
    :: Logger logger
    => MempoolConsensus ChainwebTransaction
    -> logger
    -> (BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction))
pactMemPoolGetBlock mpc theLogger height hash _bHeader = do
    logFn theLogger Info $! "pactMemPoolAccess - getting new block of transactions for "
        <> "height = " <> sshow height <> ", hash = " <> sshow hash
    mempoolGetBlock (mpcMempool mpc) maxBlockSize
  where
   logFn :: Logger l => l -> LogFunctionText -- just for giving GHC some type hints
   logFn = logFunction

pactProcessFork
    :: Logger logger
    => MempoolConsensus ChainwebTransaction
    -> logger
    -> (BlockHeader -> IO ())
pactProcessFork mpc theLogger bHeader = do
    let forkFunc = (mpcProcessFork mpc) (logFunction theLogger)
    txHashes <- forkFunc bHeader
    (logFn theLogger) Info $! "pactMemPoolAccess - " <> sshow (length txHashes)
                           <> " transactions to reintroduce"
    mempoolReintroduce (mpcMempool mpc) txHashes
  where
   logFn :: Logger l => l -> LogFunctionText
   logFn lg = logFunction lg

pactMempoolSetLastHeader
    :: Logger logger
    => MempoolConsensus ChainwebTransaction
    -> logger
    -> (BlockHeader -> IO ())
pactMempoolSetLastHeader mpc _theLogger bHeader = do
    let headerRef = mpcLastNewBlockParent mpc
    atomicWriteIORef headerRef (Just bHeader)

closeQueue :: TQueue RequestMsg -> IO ()
closeQueue = sendCloseMsg
