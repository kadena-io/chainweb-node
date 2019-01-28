{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

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

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initSQLiteCheckpointEnv cmdConfig logger gasEnv = do
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

type Store = HashMap (BlockHeight, BlockPayloadHash) DataToFill

data DataToFill = DataToFill

-- This should open a connection with the assumption that there is not
--  any connection open. There should be tests that assert this
--  essential aspect of the 'restore' semantics.
restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' lock height hash = do
    withMVarMasked lock $ \store -> do
      case HMS.lookup (height, hash) store of
        Just cdata -> do
          let dbstate = tostate cdata
          case _pdbsDbEnv dbstate of
            Env' (P.PactDbEnv {..}) ->
              takeMVar pdPactDbVar >>= \case
                P.DbEnv {..} -> openDb _db
          return dbstate

        -- This is just a placeholder for right now (the Nothing clause)
        Nothing ->
          fail
            "InMemoryCheckpointer.restore: There is no checkpoint that can be restored."
  where
    tostate = undefined

-- Prepare/Save should change the field 'dbFile' (the filename of the
-- current database) of SQLiteConfig so that the retrieval of the
-- database (referenced by the aforementioned filename) is possible in
-- a 'restore'.

prepareForValidBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForValidBlock = undefined

prepareForNewBlock ::
       MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForNewBlock = undefined

-- This should close the database connection currently open upon
-- arrival in this function. The database should either be closed (or
-- throw an error) before departure from this function. There should
-- be tests that assert this essential aspect of the 'save' semantics.
save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' lock height hash PactDbState {..} = do

    -- Saving off checkpoint.
    let datatofill = DataToFill
    modifyMVarMasked_ lock (return . HMS.insert (height, hash) datatofill)

    -- Closing database connection.
    case _pdbsDbEnv of
      Env' (P.PactDbEnv {..}) ->
        takeMVar pdPactDbVar >>= \case
          P.DbEnv {..} -> closeDb _db
