{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Control.Exception (evaluate, finally, mask)
import Control.Monad.STM

import Data.IORef
import Data.Vector (Vector)

import System.LogLevel

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.Mempool
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
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
    -> MempoolConsensus ChainwebTransaction
    -> MVar (CutDb cas)
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (TQueue RequestMsg -> IO a)
    -> IO a
withPactService ver cid logger mpc cdbv bhdb pdb dbDir nodeid resetDb action =
    withPactService' ver cid logger mpa cdbv bhdb pdb dbDir nodeid resetDb action
  where
    mpa = pactMemPoolAccess mpc logger

-- | Alternate Initialization for Pact (in process) Api, only used directly in tests to provide memPool
--   with test transactions
withPactService'
    :: PayloadCas cas
    => Logger logger
    => ChainwebVersion
    -> ChainId
    -> logger
    -> MemPoolAccess
    -> MVar (CutDb cas)
    -> BlockHeaderDb
    -> PayloadDb cas
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (TQueue RequestMsg -> IO a)
    -> IO a
withPactService' ver cid logger memPoolAccess cdbv bhDb pdb dbDir nodeid resetDb action =
    mask $ \rst -> do
        reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
        a <- async $
             PS.initPactService ver cid logger reqQ memPoolAccess cdbv bhDb pdb dbDir nodeid resetDb
        link a
        evaluate =<< rst (action reqQ) `finally` closeQueue reqQ `finally` wait a

-- TODO: get from config
-- TODO: why is this declared both here and in Mempool
maxBlockSize :: GasLimit
maxBlockSize = 100000

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
