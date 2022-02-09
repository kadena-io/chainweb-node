{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

import Chainweb.RestAPI.Utils

type BackupApi = "make-backup" :> Get '[PlainText] Text

someBackupApi :: SomeApi
someBackupApi = SomeApi (Proxy @BackupApi)

someBackupServer :: IO Text -> SomeServer
someBackupServer makeBackup = 
    SomeServer (Proxy @BackupApi) handler
  where
    handler = liftIO makeBackup 

