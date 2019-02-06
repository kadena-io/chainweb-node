{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Pact.Backend.SQLiteCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact SQLite checkpoint module for Chainweb
module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as L
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.String

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Except

import System.Directory
import System.IO.Extra

import Text.Printf

import qualified Pact.Persist as P
import qualified Pact.Persist.SQLite as P
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

type Store = HashMap (BlockHeight, BlockPayloadHash) FilePath

reinitDbEnv :: P.Loggers -> P.Persister P.SQLite -> SaveData P.SQLite -> IO (Either String PactDbState)
reinitDbEnv loggers funrec SaveData {..} = runExceptT $ do
    _db <- ExceptT $ maybe err (fmap Right . (`P.initSQLite` loggers)) _sSQLiteConfig
    return (PactDbState (EnvPersist' (PactDbEnvPersist P.pactdb (P.DbEnv {..}))) _sCommandState)
    where
    err = return $ Left $ "SQLiteCheckpointer.reinitDbEnv: Configuration exception"
    _persist = funrec
    _logger = P.newLogger loggers (fromString "<to fill with something meaningful>") -- TODO: Needs a better message
    _txRecord = _sTxRecord
    _txId = _sTxId

-- This should open a connection with the assumption that there is not
--  any connection open. There should be tests that assert this
--  essential aspect of the 'restore' semantics.
restore' :: MVar Store -> BlockHeight -> BlockPayloadHash -> IO (Either String PactDbState)
restore' lock height hash = do
  withMVar lock $ \store -> do
    case HMS.lookup (height, hash) store of
      Just cfile ->

        withTempFile $ \copy_c_file -> do

          --check that filename has the right version.
          flip (maybe (return $ Left (err_version "nothing" saveDataVersion))) (versionCheck cfile) $
               \version ->
                 if version /= saveDataVersion
                    then return $ Left (err_version version saveDataVersion)
                    else runExceptT $ do

                         -- read back SaveData from copied file
                         (savedata_bytes, cdata) <- do
                           bytes <- liftIO $ B.readFile copy_c_file
                           ExceptT $ return $ (first err_decode $ go bytes)

                         -- an "atomic copy"
                         liftIO $ B.writeFile copy_c_file savedata_bytes

                         ExceptT $ withTempFile $ \copy_sqlite_file -> do

                           -- create copy of the sqlite file
                           let copy_data = over (sSQLiteConfig . _Just) (changeSQLFilePath copy_sqlite_file const) cdata

                           -- Open a database connection.
                           reinitDbEnv P.neverLog P.persister copy_data

      Nothing -> err_restore
  where
    err_version = printf "Version exception %s %s"
    err_decode = printf "SQLiteCheckpointer.restore': Checkpoint decode exception= %s"
    err_restore = return $ Left "SQLiteCheckpointException.restore': Restore not found exception"
    versionCheck filename = getFirst $ foldMap (First . L.stripPrefix "version=") $ splitOn "_" filename
    go bytes = do
      decoded <- decode bytes
      return (bytes, decoded)

-- atomic_copy_file :: FilePath -> FilePath -> IO ()
-- atomic_copy_file file newfile =
--   withTempFile  (\f -> undefined)

changeSQLFilePath :: FilePath -> (FilePath -> FilePath -> FilePath) -> P.SQLiteConfig -> P.SQLiteConfig
changeSQLFilePath fp f (P.SQLiteConfig dbFile pragmas) =
    P.SQLiteConfig (f fp dbFile) pragmas

-- This should close the database connection currently open upon
-- arrival in this function. The database should either be closed (or
-- throw an error) before departure from this function. There should
-- be tests that assert this essential aspect of the 'save' semantics.
save' :: MVar Store -> BlockHeight -> BlockPayloadHash -> PactDbState -> IO (Either String ())
save' lock height hash PactDbState {..} =
  withMVar lock $ \store ->
    case HMS.lookup (height, hash) store of
      Just _ -> return $ Left "SQLiteCheckpointer.save': Save key not found exception"
      Nothing -> do
        case _pdbsDbEnv of
          EnvPersist' (p@(PactDbEnvPersist {..})) ->
            case _pdepEnv of
              P.DbEnv {..} -> do
                closeDb _db
                (mf, toSave) <- saveDb p _pdbsState
                let dbFile = P.dbFile <$> (_sSQLiteConfig toSave)
                    newdbFile = properName <$ dbFile
                flip (maybe (return $ Left "SQLiteCheckpointer.save': PrefixNotSetException")) mf $
                     \prefix -> do
                       let sd = encode toSave
                       B.writeFile (prefix ++ properName) sd
                       sequence $ fromMaybe (Left "SQLiteCheckpointer.save': Save db exception") (Right <$> liftM2 copyFile dbFile newdbFile)
  where
    properName = printf "chk.%s.%s" (show hash) (show height)
