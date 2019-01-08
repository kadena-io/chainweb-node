-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Data.IORef
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)

-- internal modules

import Chainweb.Pact.Types
import qualified Chainweb.BlockHeader as C
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.Logger as P

initSQLiteCheckpointEnv = undefined

restore = undefined
prepare = undefined
save = undefined
