{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import qualified Data.Aeson as A
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial hiding (store)
import qualified Data.ByteString as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.String

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import System.Directory
import System.IO.Extra

import Text.Printf

import qualified Pact.Persist as P
import qualified Pact.Persist.SQLite as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Persistence as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Orphans
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

type Store = HashMap (BlockHeight, BlockPayloadHash) FilePath

reinitDbEnv :: P.Loggers -> P.Persister P.SQLite -> SaveData P.SQLite -> IO PactDbState
reinitDbEnv loggers funrec (SaveData {..}) = do
    _db <- maybe (throwM ConfigurationException) (flip P.initSQLite loggers) _sSQLiteConfig
    return (PactDbState (EnvPersist' (PactDbEnvPersist P.pactdb (P.DbEnv {..}))) _sCommandState)
    where
    _persist = funrec
    _logger = P.newLogger loggers (fromString "<to fill with something meaningful>") -- TODO: Needs a better message
    _txRecord = _sTxRecord
    _txId = _sTxId

data SQLiteCheckpointException
  = RestoreNotFoundException
  | CheckpointDecodeException String
  | SaveKeyNotFoundException
  | ConfigurationException
  | VersionException (Maybe String) String
  | PrefixNotSetException
  | SaveDbException
  deriving (Show)

instance Exception SQLiteCheckpointException

-- This should open a connection with the assumption that there is not
--  any connection open. There should be tests that assert this
--  essential aspect of the 'restore' semantics.
restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO PactDbState
restore' lock height hash = do
  withMVarMasked lock $ \store -> do
    case HMS.lookup (height, hash) store of
      Just cfile ->

        withTempFile $ \copy_c_file -> do

          --check that filename has the right version.
          flip (maybe (throwM (VersionException Nothing saveDataVersion))) (versionCheck cfile) $
               \version ->
                 if version /= saveDataVersion
                    then throwM (VersionException (Just version) saveDataVersion)
                    else do
                         copyFile cfile copy_c_file
                         -- read back SaveData from copied file
                         cdata <-
                           do bytes <- B.readFile copy_c_file
                              either (throwM . CheckpointDecodeException) return (decode bytes)
                         -- create copy of the sqlite file
                         withTempFile $ \copy_sqlite_file -> do
                           let copy_data = over (sSQLiteConfig . _Just) (changeSQLFilePath copy_sqlite_file const) cdata
                           -- Open a database connection.
                           dbstate <- reinitDbEnv P.neverLog P.persister copy_data
                           -- Some unpacking is necessary (look at datatypes).
                           case _pdbsDbEnv dbstate of
                             EnvPersist' (PactDbEnvPersist {..}) ->
                               case _pdepEnv of
                                 P.DbEnv {..} -> openDb _db
                           return dbstate

      Nothing -> throwM RestoreNotFoundException
  where
    versionCheck filename = getFirst $ foldMap (First . L.stripPrefix "version=") $ splitOn "_" filename

changeSQLFilePath :: FilePath -> (FilePath -> FilePath -> FilePath) -> P.SQLiteConfig -> P.SQLiteConfig
changeSQLFilePath fp f (P.SQLiteConfig dbFile pragmas) =
    P.SQLiteConfig (f fp dbFile) pragmas

-- This should close the database connection currently open upon
-- arrival in this function. The database should either be closed (or
-- throw an error) before departure from this function. There should
-- be tests that assert this essential aspect of the 'save' semantics.
save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO ()
save' lock height hash PactDbState {..} =
  withMVarMasked lock $ \store ->
    case HMS.lookup (height, hash) store of
      Just _ -> throwM SaveKeyNotFoundException
      Nothing -> do
        case _pdbsDbEnv of
          EnvPersist' (p@(PactDbEnvPersist {..})) -> do
            case _pdepEnv of
              P.DbEnv {..} -> do
                closeDb _db
                (mf, toSave) <- saveDb p _pdbsState
                let dbFile = P.dbFile <$> (_sSQLiteConfig toSave)
                    newdbFile = (const properName) <$> dbFile
                flip (maybe (throwM PrefixNotSetException)) mf $
                     \prefix -> do
                       let sd = encode toSave
                       B.writeFile (prefix ++ properName) sd
                       fromMaybe (throwM SaveDbException) (liftM2 copyFile dbFile newdbFile)
  where
    properName = printf "DbCheckpointAtHash=(%s)AndHeight=(%s).txt" (show hash) (show height)
