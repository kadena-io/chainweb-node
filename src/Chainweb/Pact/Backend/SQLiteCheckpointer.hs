-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LL
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Data.IORef

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv'
initSQLiteCheckpointEnv = undefined

restore :: BlockHeight -> BlockPayloadHash -> IORef Checkpoint -> IORef Store -> IO ()
restore = undefined

prepare :: BlockHeight -> BlockPayloadHash -> OpMode -> IORef Checkpoint ->  IORef Store
        -> IO (Either String CheckpointData)
prepare = undefined

save :: BlockHeight -> BlockPayloadHash -> CheckpointData -> OpMode -> IORef Checkpoint ->  IORef Store -> IO ()
save = undefined
