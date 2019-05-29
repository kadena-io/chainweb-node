{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Concurrent.MVar

import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initInMemoryCheckpointEnv :: P.Logger -> P.GasEnv -> IO CheckpointEnv
initInMemoryCheckpointEnv logger gasEnv = do
    inmem <- newMVar mempty
    return $!
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' inmem
                      , restoreInitial = restoreInitial' inmem
                      , save = save' inmem
                      , saveInitial = saveInitial' inmem
                      , discard = discard' inmem
                      }
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = HashMap (BlockHeight, BlockHash) PactDbState

restore' :: MVar Store -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
restore' lock height hash = do
    withMVarMasked lock $ \store -> do
        case HMS.lookup (height, hash) store of
            Just dbstate -> return $! Right $! dbstate
            Nothing -> return $! Left $!
              "InMemoryCheckpointer: Restore not found: height=" <> show height
              <> ", hash=" <> show hash
              <> ", known=" <> show (HMS.keys store)

restoreInitial' :: MVar Store -> IO (Either String PactDbState)
restoreInitial' lock = do
    let bh = nullBlockHash
    restore' lock (BlockHeight 0) bh

saveInitial' :: MVar Store -> PactDbState -> IO (Either String ())
saveInitial' lock p@PactDbState {..} = do
    let bh = nullBlockHash
    save' lock (BlockHeight 0) bh p

save' :: MVar Store -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
save' lock height hash p@PactDbState {..} = do
     Right <$> modifyMVar_ lock (return . HMS.insert (height, hash) p)

discard' :: MVar Store -> PactDbState -> IO (Either String ())
discard' _ _ = return (Right ())
