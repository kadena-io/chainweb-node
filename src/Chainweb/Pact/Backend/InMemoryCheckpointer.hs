-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact PureDb checkpoint module for Chainweb
module Chainweb.Pact.Backend.InMemoryCheckpointer where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P
import qualified Pact.Types.Logger as P

import qualified Data.HashMap.Strict as HMS -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)
import Data.IORef

initInMemoryCheckpointEnv :: P.CommandConfig ->  P.Logger -> P.GasEnv -> IO CheckpointEnv'
initInMemoryCheckpointEnv cmdConfig logger gasEnv = do
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
  -> IORef (HashMap (BlockHeight, BlockHash) CheckpointData)
  -> IO ()
restore height hash cdata store = do
  s <- readIORef store
  maybe (return ()) (validate cdata) (HMS.lookup (height, hash) s)
  where
    validate = undefined

prepare ::
     BlockHeight
  -> BlockHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockHash) CheckpointData)
  -> IO (Either String (HashMap (BlockHeight, BlockHash) CheckpointData))
prepare _ _ _ _ = fmap Right . readIORef

save ::
     BlockHeight
  -> BlockHash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (BlockHeight, BlockHash) CheckpointData)
  -> IO ()
save _height _hash _opmode _cdata _store = return ()
