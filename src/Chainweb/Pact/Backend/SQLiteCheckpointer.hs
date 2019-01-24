-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HMS

import Control.Concurrent.MVar

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initSQLiteCheckpointEnv cmdConfig logger gasEnv = do
    inmem <- newMVar mempty
    return $
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' inmem
                      , save = save' inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = HashMap (BlockHeight, BlockPayloadHash) PactDbState

restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' = undefined

prepareForValidBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForValidBlock = undefined

prepareForNewBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForNewBlock = undefined

-- prepare/save could change filename (field dbFile) of SQLiteConfig
-- so that its retrieval is possible in a restore.

save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' = undefined

discard :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
discard = undefined
