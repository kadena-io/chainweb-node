{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.InMemoryCheckpointer where

import qualified Data.HashMap.Strict as HMS
import Data.IORef
import qualified Data.Map.Strict as M

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv'
initInMemoryCheckpointEnv cmdConfig logger gasEnv = do
    theStore <- newIORef HMS.empty
    theIndex <- newIORef M.empty
    return $
        CheckpointEnv'
            CheckpointEnv
                { _cpeCheckpointer =
                      Checkpointer {_cRestore = restore, _cPrepare = prepare, _cSave = save}
                , _cpeCommandConfig = cmdConfig
                , _cpeCheckpoint = theStore
                , _cpeCheckpointStore = theIndex
                , _cpeLogger = logger
                , _cpeGasEnv = gasEnv
                }

restore :: BlockHeight -> BlockPayloadHash -> IORef Checkpoint -> IORef Store -> IO ()
restore height hash checkpoint store = do
    store' <- readIORef store
    case M.lookup (height, hash) store' of
        Just newsnap -> atomicWriteIORef checkpoint newsnap
       -- This is just a placeholder for right now (the Nothing clause)
        Nothing -> fail "There is no checkpoint that can be restored."

prepare ::
       BlockHeight -> BlockPayloadHash -> OpMode -> IORef Checkpoint ->  IORef Store ->  IO (Either String CheckpointData)
prepare height hash opmode checkpoint store =
    case opmode of
        Validation -> do
            cursnap <- readIORef checkpoint
            return $ case HMS.lookup (height, hash) cursnap of
                       Just v -> Right v
                       Nothing -> Left "InMemoryCheckpointer.prepare: CheckpointData is not present."
        NewBlock -> do
            store' <- readIORef store
            case M.lookup (height, hash) store' of
                Just snap -> do
                    atomicWriteIORef checkpoint snap
                    return $ Left "We only prepare an environment for new blocks"
                Nothing -> return $ Left "Cannot prepare"

save :: BlockHeight -> BlockPayloadHash -> CheckpointData -> OpMode -> IORef Checkpoint ->  IORef Store -> IO ()
save height hash cdata opmode checkpoint store =
  case opmode of
    Validation -> do
      atomicModifyIORef'
        checkpoint
        (flip (,) () . (HMS.insert (height, hash) cdata))
      checkpoint' <- readIORef checkpoint
      atomicModifyIORef'
        store
        (\m ->
           case M.lookup (height, hash) m of
             Nothing -> (M.insert (height, hash) checkpoint' m, ())
             Just _ -> (m, ()))
    NewBlock -> return ()
