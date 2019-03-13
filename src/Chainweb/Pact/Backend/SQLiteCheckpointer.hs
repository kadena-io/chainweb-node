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
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as L
import Data.List.Split
import Data.Monoid
import Data.Serialize
import Data.String

import Control.Concurrent.MVar
import Control.Lens
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
import Chainweb.BlockHash
import Chainweb.Pact.Backend.Types

initSQLiteCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv
initSQLiteCheckpointEnv cmdConfig logger gasEnv = do
    inmem <- newMVar mempty
    pdbLock <- newEmptyMVar
    return $
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' inmem pdbLock
                      , restoreInitial = undefined pdbLock
                      , save = save' inmem pdbLock
                      , saveInitial = undefined pdbLock
                      , discard = discard' inmem pdbLock
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = HashMap (BlockHeight, BlockHash) FilePath

reinitDbEnv :: P.Loggers -> P.Persister P.SQLite -> SaveData P.SQLite -> IO (Either String PactDbState)
reinitDbEnv loggers funrec savedata = runExceptT $ do
    db <- maybeToExceptT err (`P.initSQLite` loggers) (_sSQLiteConfig savedata)
    return (PactDbState
               (EnvPersist' (PactDbEnvPersist P.pactdb (mkDbEnv db)))
               (_sCommandState savedata)
               (_sPactTxId savedata))
    where
        mkDbEnv db = P.DbEnv db persist logger txRecord txId
        err = "SQLiteCheckpointer.reinitDbEnv: Configuration exception"
        persist = funrec
        logger = P.newLogger loggers (fromString "<to fill with something meaningful>") -- TODO: Needs a better message
        txRecord = _sTxRecord savedata
        txId = _sTxId savedata

maybeToExceptT :: Monad m => e -> (a -> m b) -> Maybe a -> ExceptT e m b
maybeToExceptT f g = ExceptT . maybe (return $ Left f) (fmap Right . g)

-- NOTE: on a restore', the MVar on the PactDbState SHOULD be empty.
restore' :: MVar Store -> MVar PactDbState -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
restore' store_lock pdb_lock height hash =
    withMVar store_lock $ \store ->
        case HMS.lookup (height, hash) store of
            Nothing -> err_restore
            Just chk_file -> do

                --check that filename has the right version.
                flip (maybe (return $ Left (err_version "nothing" saveDataVersion)))
                    (versionCheck chk_file) $ \version ->
                        if version /= saveDataVersion
                            then return $ Left (err_version version saveDataVersion)
                            else do (copy_sqlite_file, deleteFileAction) <- newTempFileWithin "./"

                                    pdbstate <- runExceptT $ do

                                    -- read back SaveData from copied file
                                      cdata <- do bytes <- liftIO $ B.readFile chk_file
                                                  ExceptT $ return (first err_decode $ decode bytes)

                                      -- make copy of SaveData
                                      let copy_data = over (sSQLiteConfig . _Just) (changeSQLFilePath copy_sqlite_file const) cdata

                                      -- reinit sql db and get PactDbState
                                      pdbstate_to_lock <- ExceptT $ reinitDbEnv P.neverLog P.persister copy_data

                                      liftIO $ putMVar pdb_lock pdbstate_to_lock

                                      -- attach GC finalizer to
                                      -- PactDbState which will delete
                                      -- the SQLite file.
                                      liftIO $ void $ mkWeakMVar pdb_lock deleteFileAction

                                      return pdbstate_to_lock

                                    -- If an error occurs, still make
                                    -- sure to delete the file.
                                    when (isLeft pdbstate) deleteFileAction
                                    return pdbstate

    where
        err_version = printf "Version exception %s %s"
        err_decode = printf "SQLiteCheckpointer.restore': Checkpoint decode exception= %s"
        err_restore = return $ Left "SQLiteCheckpointException.restore': Restore not found exception"
        versionCheck filename = getFirst $ foldMap (First . L.stripPrefix "version=") $
            splitOn "_" filename

-- You'll be able to get rid of this function when the appropriate lenses are include in Pact.
changeSQLFilePath :: FilePath -> (FilePath -> FilePath -> FilePath) -> P.SQLiteConfig -> P.SQLiteConfig
changeSQLFilePath fp f (P.SQLiteConfig dbFile pragmas) = P.SQLiteConfig (f fp dbFile) pragmas

save' :: MVar Store -> MVar PactDbState -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
save' store_lock pdb_lock height hash pactdbstate =
    withMVar store_lock $ \store -> case HMS.lookup (height, hash) store of
        Just _ -> return $ Left msgSaveKeyError
        Nothing -> do
            runExceptT $ case _pdbsDbEnv pactdbstate of
                EnvPersist' (pactdbenvpersist@(PactDbEnvPersist _ _dbEnv)) -> case _dbEnv of
                    dbEnv -> do
                        ExceptT $ closeDb (P._db dbEnv)
                        -- Then "save" it. Really we're computing the SaveData data and the valid
                        -- prefix for naming the file containing serialized Pact values.
                        (mprefix, toSave) <- liftIO $ saveDb pactdbenvpersist
                                             (_pdbsState pactdbstate) (_pdbsTxId pactdbstate)
                        let dbFile = P._dbFile <$> (_sSQLiteConfig toSave)
                        let newdbFile = properName <$ dbFile
                        flip (maybe (ExceptT $ return $ Left msgPrefixError)) mprefix $ \prefix -> do
                            -- Save serialized Pact values.
                            let sd = encode toSave
                            liftIO $ B.writeFile (prefix ++ properName) sd
                            -- Copy the database file (the connection SHOULD -- be dead).
                            tempfile <- liftIO $ fst <$> newTempFileWithin "./" -- should we use Path instead of FilePath here?
                            -- We write to a temporary file THEN rename it to
                            -- get an atomic copy of the database file.
                            contents <- maybeToExceptT msgDbFileError B.readFile dbFile
                            liftIO $ B.writeFile tempfile contents
                            maybeToExceptT msgWriteDbError (renameFile tempfile) newdbFile
    where
        properName      = printf "chk.%s.%s" (show hash) (show height)
        msgPrefixError  = "SQLiteCheckpointer.save': Prefix not set exception"
        msgDbFileError  = "SQLiteCheckpointer.save': Copy dbFile error"
        msgWriteDbError = "SQLiteCheckpointer.save': Write db error"
        msgSaveKeyError = "SQLiteCheckpointer.save': Save key not found exception"

-- What should the state of the lock on the pactdbstate be here?
discard' :: MVar Store -> MVar PactDbState -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
discard' _ _ _ _ pactdbstate =
  case _pdbsDbEnv pactdbstate of
    EnvPersist' (PactDbEnvPersist _ _dbEnv) ->
      case _dbEnv of
        dbEnv -> closeDb (P._db dbEnv)
