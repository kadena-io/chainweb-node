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
    , newBlock
    , validateBlock
    ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Data.Int

import qualified Network.Wai.Handler.Warp as Warp

import Chainweb.BlockHeader
import Chainweb.Logger
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Service.Http.PactApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction

-- | Initialization for Pact (in process) Api
withPactService
    :: Logger logger
    => logger
    -> MempoolBackend ChainwebTransaction
    -> (TQueue RequestMsg -> IO a)
    -> IO a
withPactService logger memPool action
    = withPactService' logger (pactMemPoolAccess memPool) action

-- | Alternate Initialization for Pact (in process) Api, used only in tests to provide memPool
--   with test transactions
withPactService' :: Logger logger => logger -> MemPoolAccess -> (TQueue RequestMsg -> IO a) -> IO a
withPactService' logger memPoolAccess action = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    a <- async (PS.initPactService logger reqQ memPoolAccess)
    link a
    initWebService reqQ (return ()) -- web service for 'local' requests not yet implemented
    r <- action reqQ
    closeQueue reqQ
    return r

-- TODO: get from config
maxBlockSize :: Int64
maxBlockSize = 10000

pactMemPoolAccess :: MempoolBackend ChainwebTransaction -> MemPoolAccess
pactMemPoolAccess mempool _height _hash =
    -- TODO: log request with height hash
    mempoolGetBlock mempool maxBlockSize

initWebService :: TQueue RequestMsg -> IO a -> IO a
initWebService reqQ action = do
    (_port, socket) <- Warp.openFreePort
    withPactServiceApp (Left socket) "127.0.0.1" reqQ action

newBlock :: BlockHeader -> TQueue RequestMsg -> IO (MVar PayloadWithOutputs)
newBlock bHeader reqQ = do
    resultVar <- newEmptyMVar :: IO (MVar PayloadWithOutputs)
    let msg = NewBlockMsg NewBlockReq
          { _newBlockHeader = bHeader
          , _newResultVar = resultVar }
    addRequest reqQ msg
    return resultVar

validateBlock
    :: BlockHeader
    -> PayloadData
    -> TQueue RequestMsg
    -> IO (MVar (Either PactValidationErr PayloadWithOutputs))
validateBlock bHeader plData reqQ = do
    resultVar <- newEmptyMVar :: IO (MVar (Either PactValidationErr PayloadWithOutputs))
    let msg = ValidateBlockMsg ValidateBlockReq
          { _valBlockHeader = bHeader
          , _valResultVar = resultVar
          , _valPayloadData = plData }
    addRequest reqQ msg
    return resultVar

closeQueue :: TQueue RequestMsg -> IO ()
closeQueue = sendCloseMsg
