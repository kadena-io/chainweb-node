
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
    , getNextRequests
    , PactQueue(..)
    ) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Data.Vector (Vector)
import qualified Data.Vector as V


import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
data PactQueue = PactQueue
  { pqPrimaryQueue :: TBQueue RequestMsg
  , pqSecondaryQueue :: TBQueue RequestMsg
  }

pqSecondaryReadLimit :: Int
pqSecondaryReadLimit = 1

pqPrimaryReadLimit :: Int
pqPrimaryReadLimit = 1

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest p msg = atomically $
    case msg of
      ValidateBlockMsg _ -> writeTBQueue (pqPrimaryQueue p) msg
      -- NewBlockMsg _ -> writeTBQueue (pqPrimaryQueue p) msg -- maybe
      _ -> writeTBQueue (pqSecondaryQueue p) msg

getNextRequests :: PactQueue -> IO (Vector RequestMsg)
getNextRequests p = atomically $ do
  let step q1 q2 (n1,n2)
        | n1 <= 0 && n2 <= 0 = do
            return Nothing
        | n1 <= 0 = do
            m <- tryReadTBQueue q2
            return $ fmap (\m' -> (m', (n1,n2-1))) m
        | otherwise = do
            m <- tryReadTBQueue q1
            return $ fmap (\m' -> (m', (n1-1,n2))) m

  V.unfoldrM (step (pqPrimaryQueue p) (pqSecondaryQueue p)) (pqPrimaryReadLimit, pqSecondaryReadLimit)
