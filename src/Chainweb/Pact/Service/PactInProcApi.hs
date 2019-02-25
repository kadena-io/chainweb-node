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
    ( closeQueue
    , initPactExec
    , initPactExec'
    , newBlock
    , validateBlock
    ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Control.Monad.STM

import Chainweb.BlockHeader
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types

-- | Initialization for Pact (in process) Api
initPactExec :: IO (TQueue RequestMsg)
initPactExec = initPactExec' tempMemPoolAccess -- TODO: replace with real mempool

-- | Alternate Initialization for Pact (in process) Api, used only in tests to provide memPool
--   with test transactions
initPactExec' :: MemPoolAccess -> IO (TQueue RequestMsg)
initPactExec' memPoolAccess = do
    reqQ <- atomically (newTQueue :: STM (TQueue RequestMsg))
    a <- async (PS.initPactService reqQ memPoolAccess)
    link a
    return reqQ

newBlock :: BlockHeader -> TQueue RequestMsg -> MVar Transactions -> IO ()
newBlock bHeader reqQ resultVar = do
    let msg = RequestMsg
          { _reqRequestType = NewBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg


validateBlock :: BlockHeader -> TQueue RequestMsg -> MVar Transactions -> IO ()
validateBlock bHeader reqQ resultVar = do
    let msg = RequestMsg
          { _reqRequestType = ValidateBlock
          , _reqBlockHeader = bHeader
          , _reqResultVar = resultVar}
    addRequest reqQ msg

closeQueue :: TQueue RequestMsg -> IO ()
closeQueue = sendCloseMsg


-- TODO: replace reference to this with actual mempool and delete this
tempMemPoolAccess :: BlockHeight -> IO [Transaction]
tempMemPoolAccess _ = error "PactApi - MemPool access not implemented yet"
