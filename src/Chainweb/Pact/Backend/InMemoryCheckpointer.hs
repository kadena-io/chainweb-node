-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact PureDb checkpoint module for Chainweb
{-# LANGUAGE TupleSections #-}
module Chainweb.Pact.Backend.InMemoryCheckpointer where

import qualified Chainweb.BlockHeader as C
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
      (CheckpointEnv
         { _cpeCheckpointer =
             Checkpointer
               {_cRestore = restore, _cPrepare = prepare, _cSave = save}
         , _cpeCommandConfig = cmdConfig
         , _cpeCheckpointStore = theStore
         , _cpeLogger = logger
         , _cpeGasEnv = gasEnv
         })

restore ::
     C.BlockHeight
  -> P.Hash
  -> CheckpointData
  -> IORef (HashMap (C.BlockHeight, P.Hash) CheckpointData)
  -> IO ()
restore height hash cdata store = do
  s <- readIORef store
  maybe (return ()) (validate cdata) (HMS.lookup (height, hash) s)
  where
    validate = undefined

-- I'm going to leave this alone for now.
prepare ::
     C.BlockHeight
  -> P.Hash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (C.BlockHeight, P.Hash) CheckpointData)
  -> IO (Either String (HashMap (C.BlockHeight, P.Hash) CheckpointData))
prepare _ _ opmode _ =
  case opmode of
    NewBlock -> fmap Right . readIORef
    Validation -> undefined

save ::
     C.BlockHeight
  -> P.Hash
  -> OpMode
  -> CheckpointData
  -> IORef (HashMap (C.BlockHeight, P.Hash) CheckpointData)
  -> IO ()
save height hash opmode cdata store =
  case opmode of
    Validation -> return () -- We are discarding the block.
    NewBlock -> atomicModifyIORef store (\store' -> (HMS.insert (height,hash) cdata store', ()))
