-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Control.Concurrent.MVar

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initSQLiteCheckpointEnv cmdConfig logger gasEnv = do
    inmem <- newMVar undefined
    return $
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { _cRestore = restore inmem
                      , _cSave = save inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

data Store = Store

restore :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO CheckpointData
restore = undefined

prepareForValidBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)
prepareForValidBlock = undefined

prepareForNewBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String CheckpointData)
prepareForNewBlock = undefined

save :: MVar Store -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
save = undefined

discard :: MVar Store -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
discard = undefined
