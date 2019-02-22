{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Pact.Service.PactApi
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution (in-process) API for Chainweb

module Chainweb.Pact.Service.PactApi
    ( closeQueue
    , initPactExec
    , initPactExec'
    , newBlock
    , validateBlock
    ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Chainweb.BlockHeader
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types

-- | Initialization for Pact (in process) Api
initPactExec :: IO (TVar (TQueue RequestMsg))
initPactExec = do
    initPactExec' tempMemPoolAccess -- TODO: replace with real mempool

-- | Alternate Initialization for Pact (in process) Api, used only in tests to provide memPool
--   with test transactions
initPactExec' :: MemPoolAccess -> IO (TVar (TQueue RequestMsg))
initPactExec' memPoolAccess = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    reqQVar <- atomically $ newTVar reqQ
    a <- async (PS.initPactService reqQVar memPoolAccess)
    link a
    return reqQVar

newBlock :: BlockHeader -> (TVar (TQueue RequestMsg)) -> MVar Transactions -> IO ()
newBlock bHeader reqQ resultVar = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg


validateBlock :: BlockHeader -> (TVar (TQueue RequestMsg)) -> MVar Transactions -> IO ()
validateBlock bHeader reqQ resultVar = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg

closeQueue :: (TVar (TQueue RequestMsg)) -> IO ()
closeQueue reqQ = sendCloseMsg reqQ


-- TODO: replace reference to this with actual mempool and delete this
tempMemPoolAccess :: BlockHeight -> IO [Transaction]
tempMemPoolAccess _ = error "PactApi - MemPool access not implemented yet"
