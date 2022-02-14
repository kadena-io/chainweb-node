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
  { _backupRocksDb :: RocksDb
  , _backupDir :: FilePath
  , _backupChainResources :: HashMap ChainId (ChainResources logger)
  }

makeBackup :: Logger logger => BackupEnv logger -> IO Text
makeBackup env = do
    Time (epochToNow :: TimeSpan Integer) <- getCurrentTimeIntegral
    let time = microsToText (timeSpanToMicros epochToNow)
    -- maxBound ~ always flush WAL log before checkpoint, under the assumption
    -- that it's not much work
    let thisBackup = _backupDir env </> T.unpack time 
    createDirectoryIfMissing False (_backupDir env)
    createDirectoryIfMissing False thisBackup
    createDirectoryIfMissing False (thisBackup </> "rocksDb")
    createDirectoryIfMissing False (thisBackup </> "sqlite")
    -- TODO: log rocksdb checkpoint making
    checkpointRocksDb (_backupRocksDb env) maxBound (thisBackup </> "rocksDb")
    for_ (_backupChainResources env) $ \cr ->  do
        let logCr = logFunctionText
                $ addLabel ("component", "pact")
                $ addLabel ("sub-component", "backup")
                $ _chainResLogger cr
        logCr Info $ "backing up pact database to " <> T.pack thisBackup
        void $ _pactBackup (_chainResPact cr) (thisBackup </> "sqlite")
        logCr Info $ "pact db backed up"
    return time
