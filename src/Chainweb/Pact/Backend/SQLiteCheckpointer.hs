{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
-- import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
-- import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
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


data DataToFill = DataToFill
  { _dtfTxRecord :: M.Map P.TxTable [P.TxLog A.Value]
  , _dtfTxId :: Maybe P.TxId
  , _dtfSQLiteConfig :: P.SQLiteConfig
  , _dtfCommandState :: P.CommandState
  }

makeLenses ''DataToFill

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

changeSQLFilePath ::
       FilePath
    -> (FilePath -> FilePath -> FilePath)
    -> P.SQLiteConfig
    -> P.SQLiteConfig
changeSQLFilePath fp f (P.SQLiteConfig dbFile pragmas) =
    P.SQLiteConfig (f fp dbFile) pragmas

reinitDbEnv :: P.Loggers -> P.Persister P.SQLite -> DataToFill -> IO PactDbState
reinitDbEnv loggers funrec (DataToFill {..}) = do
  _db <- P.initSQLite _dtfSQLiteConfig loggers
  return (PactDbState (EnvPersist' (PactDbEnvPersist undefined (P.DbEnv {..}))) _dtfCommandState)
  where
    _persist = funrec
    _logger = P.newLogger loggers (fromString "<to fill with something meaningful>")
    _txRecord = _dtfTxRecord
    _txId = _dtfTxId

data SQLiteCheckpointException = RestoreNotFoundException deriving Show

instance Exception SQLiteCheckpointException

-- This should open a connection with the assumption that there is not
--  any connection open. There should be tests that assert this
--  essential aspect of the 'restore' semantics.
restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' lock height hash = do
    withMVarMasked lock $ \store -> do

      case HMS.lookup (height, hash) store of

        Just cdata -> do

          -- Open a database connection.
          dbstate <- reinitDbEnv P.neverLog P.persister cdata
          case _pdbsDbEnv dbstate of
            EnvPersist' (PactDbEnvPersist {..}) ->
              case _pdepEnv of
                P.DbEnv {..} -> openDb _db
          return dbstate

        Nothing -> throwM RestoreNotFoundException

-- Prepare/Save should change the field 'dbFile' (the filename of the
-- current database) of SQLiteConfig so that the retrieval of the
-- database (referenced by the aforementioned filename) is possible in
-- a 'restore'.

-- prepareForValidBlock :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForValidBlock = undefined

-- prepareForNewBlock :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
prepareForNewBlock = undefined

-- This should close the database connection currently open upon
-- arrival in this function. The database should either be closed (or
-- throw an error) before departure from this function. There should
-- be tests that assert this essential aspect of the 'save' semantics.
save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' lock height hash PactDbState {..}
    -- Saving off checkpoint.
 = do
  let datatofill = DataToFill undefined undefined undefined undefined
  modifyMVarMasked_ lock (return . HMS.insert (height, hash) datatofill)
    -- Closing database connection.
  case _pdbsDbEnv of
    EnvPersist' (PactDbEnvPersist {..}) ->
      case _pdepEnv of
        P.DbEnv {..} -> closeDb _db
