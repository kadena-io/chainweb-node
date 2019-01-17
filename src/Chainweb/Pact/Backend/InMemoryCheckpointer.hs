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
                      { _cRestore = restore inmem
                      , _cPrepareForValidBlock = prepareForValidBlock inmem
                      , _cPrepareForNewBlock = prepareForNewBlock inmem
                      , _cSave = save inmem
                      , _cDiscard = discard inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Checkpoint = (HashMap (BlockHeight, BlockPayloadHash) CheckpointData)

type Store
     = Map (BlockHeight, BlockPayloadHash) (HashMap (BlockHeight, BlockPayloadHash) CheckpointData)

data InMemoryCheckpointData = InMemoryCheckpointData
    { _inMemCheckpoint :: IORef Checkpoint
    , _inMemStore :: IORef Store
    }

restore :: MVar InMemoryCheckpointData -> BlockHeight -> BlockPayloadHash -> IO ()
restore lock height hash = do
    withMVarMasked lock $ \cdata -> do
        store <- readIORef $ _inMemStore cdata
        case M.lookup (height, hash) store of
            Just newsnap -> atomicWriteIORef (_inMemCheckpoint cdata) newsnap
       -- This is just a placeholder for right now (the Nothing clause)
            Nothing -> fail "There is no checkpoint that can be restored."

prepareForValidBlock
    :: MVar InMemoryCheckpointData
    -> BlockHeight
    -> BlockPayloadHash
    -> IO (Either String CheckpointData)
prepareForValidBlock lock height hash =
    withMVarMasked lock $ \cdata -> do
        checkpoint <- readIORef $ _inMemCheckpoint cdata
        return $
            case HMS.lookup (height, hash) checkpoint of
                Just v -> Right v
                Nothing -> Left "InMemoryCheckpointer.prepare: CheckpointData is not present."

prepareForNewBlock
    :: MVar InMemoryCheckpointData
    -> BlockHeight
    -> BlockPayloadHash
    -> IO (Either String CheckpointData)
prepareForNewBlock lock height hash =
    withMVarMasked lock $ \cdata -> do
        store <- readIORef $ _inMemStore cdata
        case M.lookup (height, hash) store of
            Just snap -> do
                atomicWriteIORef (_inMemCheckpoint cdata) snap
                return $ Left "We only prepare an environment for new blocks"
            Nothing -> return $ Left "Cannot prepare"

save :: MVar InMemoryCheckpointData -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
save lock height hash cpdata =
    withMVarMasked lock $ \cdata -> do
        checkpoint <- readIORef $ _inMemCheckpoint cdata
        atomicModifyIORef' (_inMemCheckpoint cdata) (\s -> (HMS.insert (height, hash) cpdata s, ()))
        atomicModifyIORef'
            (_inMemStore cdata)
            (\m ->
                 case M.lookup (height, hash) m of
                     Nothing -> (M.insert (height, hash) checkpoint m, ())
                     Just _ -> (m, ()))

discard :: MVar InMemoryCheckpointData -> BlockHeight -> BlockPayloadHash -> CheckpointData -> IO ()
discard _ _ _ _ = return ()
