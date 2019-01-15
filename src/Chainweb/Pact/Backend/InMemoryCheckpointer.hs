-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.InMemoryCheckpointer where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Concurrent.MVar (MVar, newMVar, withMVarMasked)

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initInMemoryCheckpointEnv cmdConfig logger gasEnv = do
  store <- newIORef mempty
  checkpoint <- newIORef mempty
  inmem <- newMVar (InMemoryCheckpointData checkpoint store)
  return $
      CheckpointEnv
        { _cpeCheckpointer =
            Checkpointer
              {_cRestore = restore inmem, _cPrepare = prepare inmem, _cSave = save inmem}
        , _cpeCommandConfig = cmdConfig
        , _cpeLogger = logger
        , _cpeGasEnv = gasEnv
        }

data InMemoryCheckpointData = InMemoryCheckpointData
    { _inMemCheckpoint :: IORef (HashMap (BlockHeight, BlockPayloadHash) CheckpointData)
    , _inMemStore :: IORef (Map (BlockHeight, BlockPayloadHash) (HashMap (BlockHeight, BlockPayloadHash) CheckpointData))
    }

restore :: MVar InMemoryCheckpointData ->  BlockHeight -> BlockPayloadHash -> IO ()
restore lock height hash = do
  withMVarMasked lock $ \cdata -> do
    store <- readIORef $ _inMemStore cdata
    case M.lookup (height, hash) store of
        Just newsnap -> atomicWriteIORef (_inMemCheckpoint cdata) newsnap
       -- This is just a placeholder for right now (the Nothing clause)
        Nothing -> fail "There is no checkpoint that can be restored."

-- prepare :: IORef Checkpoint ->  IORef Store -> BlockHeight -> BlockPayloadHash -> OpMode -> IO (Either String CheckpointData)
prepare :: MVar InMemoryCheckpointData -> BlockHeight -> BlockPayloadHash -> OpMode -> IO (Either String CheckpointData)
prepare lock height hash opmode =
  withMVarMasked lock $ \cdata -> do
    checkpoint <- readIORef $ _inMemCheckpoint cdata
    store <- readIORef $ _inMemStore cdata
    case opmode of
      Validation ->
        return $
        case HMS.lookup (height, hash) checkpoint of
          Just v -> Right v
          Nothing ->
            Left "InMemoryCheckpointer.prepare: CheckpointData is not present."
      NewBlock -> do
        case M.lookup (height, hash) store of
          Just snap -> do
            atomicWriteIORef (_inMemCheckpoint cdata) snap
            return $ Left "We only prepare an environment for new blocks"
          Nothing -> return $ Left "Cannot prepare"

-- save :: IORef Checkpoint ->  IORef Store ->  BlockHeight -> BlockPayloadHash -> CheckpointData -> OpMode -> IO ()
save :: MVar InMemoryCheckpointData ->  BlockHeight -> BlockPayloadHash -> CheckpointData -> OpMode -> IO ()
save lock height hash cpdata opmode =
  withMVarMasked lock $ \cdata -> do
    checkpoint <- readIORef $ _inMemCheckpoint cdata
    case opmode of
      Validation -> do
        atomicModifyIORef'
          (_inMemCheckpoint cdata)
          (\s -> (HMS.insert (height, hash) cpdata s, ()))
        atomicModifyIORef'
          (_inMemStore cdata)
          (\m ->
             case M.lookup (height, hash) m of
               Nothing -> (M.insert (height, hash) checkpoint m, ())
               Just _ -> (m, ()))
      NewBlock -> return ()
