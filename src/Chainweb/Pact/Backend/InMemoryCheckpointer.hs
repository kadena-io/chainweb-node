{-# LANGUAGE LambdaCase #-}
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

import qualified Pact.Interpreter as P
import qualified Pact.PersistPactDb as P
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
                      { restore = restore' inmem
                      , save = save' inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = HashMap (BlockHeight, BlockPayloadHash) PactDbState

restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' lock height hash = do
    withMVarMasked lock $ \store -> do
        case HMS.lookup (height, hash) store of
            Just old -> do
              let dbstate = tostate old
              case _pdbsDbEnv dbstate of
                Env' (P.PactDbEnv {..}) ->
                  takeMVar pdPactDbVar >>= \case
                    P.DbEnv {..} -> openDb _db
              return dbstate
            -- This is just a placeholder for right now (the Nothing clause)
            Nothing ->
                fail "InMemoryCheckpointer.restore: There is no checkpoint that can be restored."
  where
    tostate = id

-- There is no need for prepare to even exist for the in memory checkpointer.

save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' lock height hash cpdata@(PactDbState {..}) = do

     -- Saving off checkpoint.
     modifyMVarMasked_ lock (return . HMS.insert (height, hash) cpdata)

     -- Closing database connection.
     case _pdbsDbEnv of
       Env' (P.PactDbEnv {..}) ->
         takeMVar pdPactDbVar >>= \case
           P.DbEnv {..} -> closeDb _db
