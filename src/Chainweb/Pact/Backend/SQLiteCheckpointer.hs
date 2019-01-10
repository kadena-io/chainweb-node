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

initSQLiteCheckpointEnv :: P.CommandConfig ->  P.Logger -> P.GasEnv -> IO CheckpointEnv'
initSQLiteCheckpointEnv cmdConfig logger gasEnv = do
  theStore <- newIORef HMS.empty
  return $
    CheckpointEnv'
      CheckpointEnv
         { _cpeCheckpointer =
             Checkpointer
               {_cRestore = restore, _cPrepare = prepare, _cSave = save}
         , _cpeCommandConfig = cmdConfig
         , _cpeCheckpointStore = theStore
         , _cpeLogger = logger
         , _cpeGasEnv = gasEnv
         }

restore ::
     BlockHeight
  -> BlockPayloadHash
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockPayloadHash) FilePath)
  -> IO ()
restore _height _hash _cdata _store = undefined

prepare ::
     BlockHeight
  -> BlockPayloadHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockPayloadHash) FilePath)
  -> IO (Either String (HashMap (BlockHeight, BlockPayloadHash) FilePath))
prepare _height _hash _opmode _cdata _store = undefined

save ::
     BlockHeight
  -> BlockPayloadHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockPayloadHash) FilePath)
  -> IO ()
save _height _hash _opmode _cdata _store = undefined
