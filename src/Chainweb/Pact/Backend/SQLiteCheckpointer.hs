-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LL
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Chainweb.Pact.Types
import Chainweb.BlockHash
import Chainweb.BlockHeader
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.Logger as P

import Data.IORef
import qualified Data.HashMap.Strict as HMS -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)

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
  -> BlockHash
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockHash) FilePath)
  -> IO ()
restore _height _hash _cdata _store = undefined

prepare ::
     BlockHeight
  -> BlockHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockHash) FilePath)
  -> IO (Either String (HashMap (BlockHeight, BlockHash) FilePath))
prepare _height _hash _opmode _cdata _store = undefined

save ::
     BlockHeight
  -> BlockHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockHash) FilePath)
  -> IO ()
save _height _hash _opmode _cdata _store = undefined
