
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
    , PactQueue(..)
    ) where

import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Data.DList as D
import Data.List (uncons)

import Chainweb.Pact.Service.Types

-- | The type of the Pact Queue
data PactQueue = PactQueue
  { pqPrimaryQueue :: TBQueue RequestMsg
  , pqSecondaryQueue :: TBQueue RequestMsg
  , pqCachedRequests :: TVar [RequestMsg]
  }

pqSecondaryReadLimit :: Int
pqSecondaryReadLimit = 10

pqPrimaryReadLimit :: Int
pqPrimaryReadLimit = 30

-- | Add a request to the Pact execution queue
addRequest :: PactQueue -> RequestMsg -> IO ()
addRequest p msg = atomically $
    case msg of
      ValidateBlockMsg _ -> writeTBQueue (pqPrimaryQueue p) msg
      -- NewBlockMsg _ -> writeTBQueue (pqPrimaryQueue p) msg -- maybe
      _ -> writeTBQueue (pqSecondaryQueue p) msg

-- | Get the next available request from the Pact execution queue
getNextRequest :: PactQueue -> IO RequestMsg
getNextRequest p = atomically $ do
    xxs <- uncons <$> readTVar (pqCachedRequests p)
    case xxs of
      Just (x, xs) -> do
        writeTVar (pqCachedRequests p) xs
        return x
      Nothing -> do
        mReqs <- D.fromList <$> mapM (const $ tryReadTBQueue (pqPrimaryQueue p)) [1.. pqPrimaryReadLimit]
        nReqs <- D.fromList <$> mapM (const $ tryReadTBQueue (pqSecondaryQueue p)) [1.. pqSecondaryReadLimit]
        case uncons $ formRequests $ D.append mReqs nReqs of
          Nothing -> readTBQueue (pqPrimaryQueue p)
          Just (a, as) -> do
                writeTVar (pqCachedRequests p) as
                return a
  where
    formRequests :: D.DList (Maybe RequestMsg) -> [RequestMsg]
    formRequests dl = foldMap (maybe id (:)) dl []
