{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Backup
    ( BackupEnv(..)
    , BackupOptions(..)
    , BackupStatus(..)
    , makeBackup
    , checkBackup
    ) where

import Control.Lens

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Data.HashSet(HashSet)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import System.Directory
import System.FilePath
import System.LogLevel

import Pact.Types.SQLite
import Servant

import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Pact.Backend.Utils(chainDbFileName, withSqliteDb)
import Chainweb.Utils

import Chainweb.Storage.Table.RocksDB

data BackupOptions = BackupOptions
    { _backupIdentifier :: !FilePath
    , _backupPact :: !Bool
    }

data BackupEnv logger = BackupEnv
  { _backupRocksDb :: !RocksDb
  , _backupDir :: !FilePath
  , _backupPactDbDir :: !FilePath
  , _backupChainIds :: !(HashSet ChainId)
  , _backupLogger :: !logger
  }

data BackupStatus
    = BackupDone | BackupInProgress | BackupFailed

instance HasTextRepresentation BackupStatus where
    toText BackupDone = "backup-done"
    toText BackupInProgress = "backup-in-progress"
    toText BackupFailed = "backup-failed"

    fromText "backup-done" = return BackupDone
    fromText "backup-in-progress" = return BackupInProgress
    fromText "backup-failed" = return BackupFailed
    fromText t =
        throwM $ TextFormatException $ "HasTextRepresentation BackupStatus: invalid BackupStatus " <> sshow t

instance MimeRender PlainText BackupStatus where
    mimeRender = const (TL.encodeUtf8 . TL.fromStrict . toText)

instance MimeUnrender PlainText BackupStatus where
    mimeUnrender = const (over _Left show . fromText . TL.toStrict . TL.decodeUtf8)

makeBackup :: Logger logger => BackupEnv logger -> BackupOptions -> IO ()
makeBackup env options = do
    logCr Info ("making backup to " <> T.pack thisBackup)
    createDirectoryIfMissing True (thisBackup </> "0" </> "sqlite")
    -- we don't create the rocksDb checkpoint folder ourselves,
    -- RocksDB fails if it exists
    T.writeFile (thisBackup </> "status") (toText BackupInProgress)
    doBackup `catch` \(ex :: SomeException) -> do
        T.writeFile (thisBackup </> "0" </> "status") (toText BackupFailed)
        logCr Error ("backup to " <> T.pack thisBackup <> " failed: " <> sshow ex)
        throwM ex
    T.writeFile (thisBackup </> "status") (toText BackupDone)
  where
    logCr = logFunctionText (_backupLogger env)
    thisBackup = _backupDir env </> _backupIdentifier options
    doBackup = do
        logCr Info ("making rocksdb checkpoint to " <> T.pack (thisBackup </> "0" </> "rocksDb"))
        -- maxBound ~ always flush WAL log before checkpoint, under the assumption
        -- that it's not too time-consuming
        checkpointRocksDb (_backupRocksDb env) maxBound (thisBackup </> "0" </> "rocksDb")
        logCr Info "rocksdb checkpoint made"
        when (_backupPact options) $ do
            logCr Info $ "backing up pact databases" <> T.pack thisBackup
            forConcurrently_ (_backupChainIds env) $ \cid -> do
                withSqliteDb cid (_backupLogger env) (_backupPactDbDir env) False $ \db ->
                    void $ qry db
                        ("VACUUM main INTO ?")
                        [SText $ fromString (thisBackup </> "0" </> "sqlite" </> chainDbFileName cid)]
                        []
            logCr Info $ "pact databases backed up"

checkBackup :: Logger logger => BackupEnv logger -> FilePath -> IO (Maybe BackupStatus)
checkBackup env name = do
    let thisBackup = _backupDir env </> name
    logFunctionText (_backupLogger env) Info $ "checking backup " <> T.pack name
    exists <- doesFileExist (thisBackup </> "status")
    if exists
    then
        fmap Just . fromText =<< T.readFile (thisBackup </> "status")
    else
        return Nothing
