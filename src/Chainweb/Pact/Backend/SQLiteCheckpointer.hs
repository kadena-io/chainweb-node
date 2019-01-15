-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

-- import Data.IORef
-- import Control.Concurrent.MVar
--     (MVar, modifyMVarMasked_, newEmptyMVar, newMVar, putMVar, readMVar,
--     takeMVar, withMVarMasked)
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initSQLiteCheckpointEnv = undefined

data SQLiteCheckpointData =
    SQLiteCheckpointData

restore :: SQLiteCheckpointData -> BlockHeight -> BlockPayloadHash -> IO ()
restore = undefined

prepareForValidBlock ::
     SQLiteCheckpointData
  -> BlockHeight
  -> BlockPayloadHash
  -> IO (Either String CheckpointData)
prepareForValidBlock = undefined

prepareForNewBlock ::
       SQLiteCheckpointData
    -> BlockHeight
    -> BlockPayloadHash
    -> IO (Either String CheckpointData)
prepareForNewBlock = undefined

save :: SQLiteCheckpointData -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
save = undefined

discard :: SQLiteCheckpointData -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
discard = undefined
