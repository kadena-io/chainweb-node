-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.InMemoryCheckpointer
    ( initInMemoryCheckpointEnv
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe

import Control.Concurrent.MVar

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

-- MIGHT INCLUDE THIS MODULE LATER
-- import Chainweb.ChainId
-- MIGHT INCLUDE THIS MODULE LATER

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initInMemoryCheckpointEnv cmdConfig logger gasEnv = do
    inmem <- newMVar mempty
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

type Store = HashMap (BlockHeight, BlockPayloadHash) CheckpointData

restore :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO CheckpointData
restore lock height hash = do
    withMVarMasked lock $ \store -> do
        case HMS.lookup (height, hash) store of
            Just newsnap -> return newsnap
            -- This is just a placeholder for right now (the Nothing clause)
            Nothing ->
                fail "InMemoryCheckpointer.restore: There is no checkpoint that can be restored."

-- There is no need for prepare to even exist for the in memory checkpointer.

save :: MVar Store -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
save lock height hash cpdata =
     modifyMVarMasked_ lock (return . HMS.insert (height, hash) cpdata)
