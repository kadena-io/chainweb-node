{-# LANGUAGE DataKinds #-}
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

import qualified Network.Wai.Handler.Warp as Warp

import Chainweb.BlockHeader
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.Http.PactApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types

-- | Initialization for Pact (in process) Api
withPactService :: ((TQueue RequestMsg) -> IO a) -> IO a
withPactService action = withPactService' tempMemPoolAccess action -- TODO: replace with real mempool

-- | Alternate Initialization for Pact (in process) Api, used only in tests to provide memPool
--   with test transactions
withPactService' :: MemPoolAccess -> ((TQueue RequestMsg) -> IO a) -> IO a
withPactService' memPoolAccess action = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    a <- async (PS.initPactService reqQ memPoolAccess)
    link a
    initWebService reqQ (return ()) -- web service for 'local' requests not yet implemented
    r <- action reqQ
    closeQueue reqQ
    return r

initWebService :: (TQueue RequestMsg) -> IO a -> IO a
initWebService reqQ action = do
    (_port, socket) <- Warp.openFreePort
    withPactServiceApp (Left socket) "127.0.0.1" reqQ $ action

newBlock :: BlockHeader -> TQueue RequestMsg -> IO (MVar Transactions )
newBlock bHeader reqQ = do
    resultVar <- newEmptyMVar :: IO (MVar Transactions)
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg
    return resultVar

validateBlock :: BlockHeader -> TQueue RequestMsg -> IO (MVar Transactions)
validateBlock bHeader reqQ = do
    resultVar <- newEmptyMVar :: IO (MVar Transactions)
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg
    return resultVar

closeQueue :: TQueue RequestMsg -> IO ()
closeQueue = sendCloseMsg

-- TODO: replace reference to this with actual mempool and delete this
tempMemPoolAccess :: BlockHeight -> IO [Transaction]
tempMemPoolAccess _ = error "PactApi - MemPool access not implemented yet"
