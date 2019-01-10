-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LL
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Chainweb.Pact.Types
import Chainweb.BlockHeader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.IORef

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules

import qualified Chainweb.BlockHeader as C
import Chainweb.Pact.Types

initSQLiteCheckpointEnv = undefined

restore = undefined
prepare = undefined
save = undefined
