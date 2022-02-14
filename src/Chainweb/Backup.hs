{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Backup 
    ( BackupEnv(..)
    , makeBackup
    ) where

import Data.CAS.RocksDB
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict(HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.LogLevel

import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Logger
import Chainweb.Time
import Chainweb.WebPactExecutionService(_pactBackup)

data BackupEnv logger = BackupEnv
  { _backupRocksDb :: !RocksDb
  , _backupDir :: !FilePath
  , _backupChainResources :: !(HashMap ChainId (ChainResources logger))
  , _backupLogger :: !logger
  }

makeBackup :: Logger logger => BackupEnv logger -> IO Text
makeBackup env = do
    Time (epochToNow :: TimeSpan Integer) <- getCurrentTimeIntegral
    let time = microsToText (timeSpanToMicros epochToNow)
    let thisBackup = _backupDir env </> T.unpack time 
    logFunctionText (_backupLogger env) Info ("making backup to " <> T.pack thisBackup)
    createDirectoryIfMissing False (_backupDir env)
    createDirectoryIfMissing False thisBackup
    createDirectoryIfMissing False (thisBackup </> "sqlite")
    -- TODO: log rocksdb checkpoint making
    logFunctionText (_backupLogger env) Info ("making rocksdb checkpoint to " <> T.pack (thisBackup </> "rocksDb"))
    -- 0 ~ never flush WAL log before checkpoint, under the assumption 
    -- that it's not very helpful
    checkpointRocksDb (_backupRocksDb env) 0 (thisBackup </> "rocksDb")
    logFunctionText (_backupLogger env) Info "rocksdb checkpoint made"
    for_ (_backupChainResources env) $ \cr ->  do
        let logCr = logFunctionText
                $ addLabel ("component", "pact")
                $ addLabel ("sub-component", "backup")
                $ _chainResLogger cr
        logCr Info $ "backing up pact database to " <> T.pack thisBackup
        void $ _pactBackup (_chainResPact cr) (thisBackup </> "sqlite")
        logCr Info $ "pact db backed up"
    return time
