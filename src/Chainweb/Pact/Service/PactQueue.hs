-- |
-- Module: Chainweb.Pact.Service.PactQueue
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact execution service queue for Chainweb

module Chainweb.Pact.Service.PactQueue
    (
      PactQueue
    , PactQueueAccess(..)
    ) where

import Control.Concurrent.STM.TBQueue

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
type PactQueue = TBQueue RequestMsg

data PactQueueAccess = PactQueueAccess
  {
    addRequest :: RequestMsg -> IO ()
  , getNextRequest :: IO RequestMsg
  }
