-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact PureDb checkpoint module for Chainweb

module Chainweb.Pact.MapPureCheckpoint
  ( restoreCheckpoint
  , initPactCheckpointStore
  , makeCheckpoint
  ) where

import Chainweb.Pact.Types
import qualified Pact.Types.Runtime as P
import Data.IORef
import qualified Data.Map.Strict as Map

makeCheckpoint ::
     P.Hash
  -> Integer
  -> PactDbState'
  -> MapPurePactCheckpointStore
  -> IO ()
makeCheckpoint hash height pactDbState store = atomicModifyIORef' store go
  where
    go m = (Map.insert height (hash, PactDbStatePersist Nothing pactDbState) m, ())

restoreCheckpoint ::
     P.Hash
  -> Integer
  -> MapPurePactCheckpointStore
  -> IO (Maybe PactDbState')
restoreCheckpoint hash height store = do
  m <- readIORef store
  return $ do
    (h, st) <- Map.lookup height m
    if h == hash
      then Just (_pdbspPactDbState st)
      else Nothing

initPactCheckpointStore :: IO MapPurePactCheckpointStore
initPactCheckpointStore = newIORef Map.empty
