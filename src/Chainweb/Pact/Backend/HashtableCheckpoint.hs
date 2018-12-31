-- |
-- Module: Chainweb.Pact.Backend.HashtableCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact Db checkpoint module for Chainweb with hashtable

module Chainweb.Pact.Backend.HashtableCheckpoint
  ( restoreCheckpoint
  , initPactCheckpointStore
  , makeCheckpoint
  ) where

import Chainweb.Pact.Types
import qualified Data.HashTable.IO as H
import qualified Pact.Types.Runtime as P

makeCheckpoint ::
     P.Hash
  -> Integer
  -> PactDbState'
  -> HashTablePactCheckpointStore
  -> IO ()
makeCheckpoint hash height pactDbState table =
  H.insert table height (hash, PactDbStatePersist Nothing pactDbState)

restoreCheckpoint ::
     P.Hash
  -> Integer
  -> HashTablePactCheckpointStore
  -> IO (Maybe PactDbState')
restoreCheckpoint hash height store = do
  mvalue <- H.lookup store height
  return $ do
    (h, st) <- mvalue
    if h == hash
      then Just (_pdbspPactDbState st)
      else Nothing

initPactCheckpointStore :: IO HashTablePactCheckpointStore
initPactCheckpointStore = H.new
