-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact checkpoint module for Chainweb

module Chainweb.Pact.DiskCheckpoint where

import Chainweb.Pact.Types
import qualified Database.SQLite3.Direct as SQ3

import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- makeCheckpoint ::P.Hash -> Integer -> PactDbState p -> OnDiskPactCheckpointStore -> IO ()
makeCheckpoint hash height pactDbState = return ()

-- restoreCheckpoint :: P.Hash -> Integer -> OnDiskPactCheckpointStore SQLite -> IO (Maybe (PactDbState SQLite))
restoreCheckpoint hash height = undefined

-- initPactCheckpointStore :: IO (OnDiskPactCheckpointStore SQLite)
initPactCheckpointStore = undefined
