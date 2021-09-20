-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb

module Chainweb.Pact.Service.PactQueue
    ( addRequest
    , getNextRequest
    , PactQueue
    , PactQueues(..)
    , validateBlockQueue
    , newBlockQueue
    , otherMsgsQueue
    ) where

import Control.Applicative ((<|>))
import Control.Lens (lens, Lens)
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
type PactQueue = TBQueue RequestMsg


-- ACHTUNG: Make sure that only execValidateBlock msgs are found in the "validateBlockQueue" Queue
-- ACHTUNG: Make sure that only execNewBlock msgs are found in the "newBlockQueue" Queue
-- ACHTUNG: Make sure that neither execValidateBlock & execNewBlock msgs are found in the "otherMsgsQueue" Queue
data PactQueues = PactQueues {
     _validateBlockQueue :: PactQueue
   , _newBlockQueue :: PactQueue
   , _otherMsgsQueue :: PactQueue
 }


validateBlockQueue :: Lens PactQueues PactQueues PactQueue PactQueue
validateBlockQueue = lens _validateBlockQueue (\s b -> s { _validateBlockQueue = b})

newBlockQueue :: Lens PactQueues PactQueues PactQueue PactQueue
newBlockQueue = lens _newBlockQueue (\s b -> s { _newBlockQueue = b})

otherMsgsQueue :: Lens PactQueues PactQueues PactQueue PactQueue
otherMsgsQueue = lens _otherMsgsQueue (\s b -> s { _otherMsgsQueue = b})

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest q msg = atomically $ writeTBQueue q msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueues -> IO RequestMsg
getNextRequest qs = atomically $ do
    vb <- tryReadTBQueue $ _validateBlockQueue qs
    nb <- tryReadTBQueue $ _newBlockQueue qs
    om <- tryReadTBQueue $ _otherMsgsQueue qs
    case vb <|> nb <|> om of
      Nothing -> retry
      Just msg -> return msg
