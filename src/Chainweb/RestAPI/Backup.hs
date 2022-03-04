{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | An endpoint for making database backups. Also used to synchronize 
-- databases between nodes.

module Chainweb.RestAPI.Backup
    ( BackupApi
    , someBackupApi
    , someBackupServer
    ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Servant

import qualified Chainweb.Backup as Backup
import Chainweb.Logger
import Chainweb.Time

import Chainweb.RestAPI.Utils

type BackupApi 
  =    "make-backup" :> Post '[PlainText] Text
  :<|> "check-backup" :> Capture "backup-name" FilePath :> Get '[PlainText] Backup.BackupStatus

someBackupApi :: SomeApi
someBackupApi = SomeApi (Proxy @BackupApi)

someBackupServer :: Logger logger => Backup.BackupEnv logger -> SomeServer
someBackupServer backupEnv = 
    SomeServer (Proxy @BackupApi) handler
  where
    noSuchBackup = err404 { errBody = "no such backup" }
    makeBackup = liftIO $ do
        nextBackupName <- getNextBackupName
        _ <- async $ Backup.makeBackup backupEnv (T.unpack nextBackupName)
        return nextBackupName
    checkBackup backupName = liftIO $ do
        status <- Backup.checkBackup backupEnv backupName
        maybe (throwM noSuchBackup) pure status 
    handler = makeBackup :<|> checkBackup

getNextBackupName :: IO Text
getNextBackupName = do
    Time (epochToNow :: TimeSpan Integer) <- getCurrentTimeIntegral
    return $ microsToText (timeSpanToMicros epochToNow)

