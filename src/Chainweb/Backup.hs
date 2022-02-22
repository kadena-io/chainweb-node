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
    , BackupStatus(..)
    , makeBackup
    , checkBackup
    ) where

import Control.Lens
import Control.Monad.Catch
import Data.CAS.RocksDB
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict(HashMap)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import System.Directory
import System.FilePath
import System.LogLevel

import Servant

import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Logger
import Chainweb.Utils
import Chainweb.WebPactExecutionService(_pactBackup)

data BackupEnv logger = BackupEnv
  { _backupRocksDb :: !RocksDb
  , _backupDir :: !FilePath
  , _backupChainResources :: !(HashMap ChainId (ChainResources logger))
  , _backupLogger :: !logger
  }

data BackupStatus 
    = BackupDone | BackupInProgress | BackupFailed

instance HasTextRepresentation BackupStatus where
    toText BackupDone = "backup-done"
    toText BackupInProgress = "backup-in-progress"
    toText BackupFailed = "backup-failed"

    fromText "backup-done" = return BackupDone
    fromText "backup-in-progress" = return BackupDone
    fromText "backup-failed" = return BackupFailed
    fromText t = 
        throwM $ TextFormatException $ "HasTextRepresentation BackupStatus: invalid BackupStatus " <> sshow t

instance MimeRender PlainText BackupStatus where
    mimeRender = const (TL.encodeUtf8 . TL.fromStrict . toText)

instance MimeUnrender PlainText BackupStatus where
    mimeUnrender = const (over _Left show . fromText . TL.toStrict . TL.decodeUtf8)

makeBackup :: Logger logger => BackupEnv logger -> FilePath -> IO ()
makeBackup env name = do
    logFunctionText (_backupLogger env) Info ("making backup to " <> T.pack thisBackup)
    createDirectoryIfMissing False (_backupDir env)
    createDirectoryIfMissing False thisBackup
    createDirectoryIfMissing False (thisBackup </> "sqlite")
    -- we don't create the rocksDb checkpoint folder ourselves, 
    -- RocksDB fails if it exists
    T.writeFile (thisBackup </> "status") (toText BackupInProgress)
    result <- try doBackup 
    case result of
        Left (ex :: SomeException) -> do
            T.writeFile (thisBackup </> "status") (toText BackupFailed)
            logFunctionText (_backupLogger env) Error ("backup to " <> T.pack thisBackup <> " failed: " <> sshow ex)
            throwM ex
        Right () -> return ()
    where
    thisBackup = _backupDir env </> name
    doBackup = do
        logFunctionText (_backupLogger env) Info ("making rocksdb checkpoint to " <> T.pack (thisBackup </> "rocksDb"))
        -- maxBound ~ always flush WAL log before checkpoint, under the assumption 
        -- that it's not too time-consuming
        checkpointRocksDb (_backupRocksDb env) maxBound (thisBackup </> "rocksDb")
        logFunctionText (_backupLogger env) Info "rocksdb checkpoint made"
        for_ (_backupChainResources env) $ \cr ->  do
            let logCr = logFunctionText
                    $ addLabel ("component", "pact")
                    $ addLabel ("sub-component", "backup")
                    $ _chainResLogger cr
            logCr Info $ "backing up pact database to " <> T.pack thisBackup
            void $ _pactBackup (_chainResPact cr) (thisBackup </> "sqlite")
            logCr Info $ "pact db backed up"
        T.writeFile (thisBackup </> "status") (toText BackupDone)

checkBackup :: BackupEnv logger -> FilePath -> IO (Maybe BackupStatus)
checkBackup env name = do
    let thisBackup = _backupDir env </> name
    logFunctionText (_backupLogger env) Info $ "checking backup " <> name
    exists <- doesFileExist (thisBackup </> "status")
    if exists 
    then 
        fmap Just . fromText =<< T.readFile (thisBackup </> "status")
    else 
        return Nothing
