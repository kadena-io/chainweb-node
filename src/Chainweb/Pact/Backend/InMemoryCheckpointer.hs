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

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict as M

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Catch

import qualified Pact.Persist as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

-- MIGHT INCLUDE THIS MODULE LATER
-- import Chainweb.ChainId
-- MIGHT INCLUDE THIS MODULE LATER

data DataToSave = DataToSave
  { _dtfTxRecord :: M.Map P.TxTable [P.TxLog A.Value]
  , _dtfTxId :: Maybe P.TxId
  , _dtfCommandState :: P.CommandState
  }

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

data InMemCheckpointException = RestoreNotFoundException deriving Show

instance Exception InMemCheckpointException

restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' lock height hash = do
    withMVarMasked lock $ \store -> do
        case HMS.lookup (height, hash) store of
            Just dbstate -> do
              case _pdbsDbEnv dbstate of
                EnvPersist' (PactDbEnvPersist {..}) ->
                  case _pdepEnv of
                    P.DbEnv {..} -> openDb _db
              return dbstate

            Nothing -> throwM RestoreNotFoundException

save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' lock height hash PactDbState {..} = do

     -- Saving off checkpoint.
     modifyMVarMasked_ lock (return . HMS.insert (height, hash) undefined)

     -- Closing database connection.
     case _pdbsDbEnv of
       EnvPersist' (PactDbEnvPersist {..}) ->
         case _pdepEnv of
           P.DbEnv {..} -> closeDb _db
