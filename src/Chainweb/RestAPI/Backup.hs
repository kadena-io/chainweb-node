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

import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Servant

import Chainweb.Backup
import Chainweb.Logger

import Chainweb.RestAPI.Utils

type BackupApi 
  =    "make-backup" :> Get '[PlainText] Text
  :<|> "backup-status" :> Get '[Text] 

someBackupApi :: SomeApi
someBackupApi = SomeApi (Proxy @BackupApi)

someBackupServer :: Logger logger => BackupEnv logger -> SomeServer
someBackupServer backupEnv = 
    SomeServer (Proxy @BackupApi) handler
  where
    handler = liftIO (makeBackup backupEnv)

