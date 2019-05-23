{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

import System.LogLevel

import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Logger
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
withPactService ver cid logger memPool cdbv action
    = withPactService' ver cid logger (pactMemPoolAccess memPool logger) cdbv action

-- | Alternate Initialization for Pact (in process) Api, used only in tests to provide memPool
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

pactMemPoolAccess :: Logger logger => MempoolBackend ChainwebTransaction -> logger -> MemPoolAccess
pactMemPoolAccess mempool theLogger _height _hash bHeader = do
    let forkFunc = (mempoolProcessFork mempool) (logFunction theLogger) -- :: BlockHeader -> IO (V.Vector ChainwebTransaction)
    txHashes <- forkFunc bHeader
    (logFn theLogger) Info $! "pactMemPoolAccess - " <> sshow (length txHashes)
                              <> " transactions to reintroduce"
    mempoolReintroduce mempool txHashes
    mempoolGetBlock mempool maxBlockSize
  where
   logFn :: Logger l => l -> LogFunctionText
   logFn lg = logFunction lg

closeQueue :: TQueue RequestMsg -> IO ()
closeQueue = sendCloseMsg
