{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Chainweb.Backup qualified as Backup
import Chainweb.Logger
import Chainweb.RestAPI.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Servant
import System.IO.Unsafe
import System.LogLevel

type BackupApi_
  =    "make-backup" :> QueryFlag "backupPact" :> PostAccepted '[PlainText] Text
  :<|> "check-backup" :> Capture "backup-name" FilePath :> Get '[PlainText] Backup.BackupStatus

type BackupApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc BackupApi_

backupApi :: forall (v :: ChainwebVersionT). Proxy (BackupApi v)
backupApi = Proxy

globalCurrentBackup :: TVar (Maybe Text)
globalCurrentBackup = unsafePerformIO $! newTVarIO Nothing
{-# NOINLINE globalCurrentBackup #-}

someBackupApi :: ChainwebVersion -> SomeApi
someBackupApi (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = SomeApi $ backupApi @v

someBackupServer :: (HasVersion, Logger logger) => Backup.BackupEnv logger -> SomeServer
someBackupServer backupEnv = case implicitVersion of
    (FromSingChainwebVersion (SChainwebVersion :: Sing vT)) ->
        SomeServer (Proxy @(BackupApi vT)) $ makeBackup :<|> checkBackup
  where
    noSuchBackup = setErrText "no such backup" err404
    makeBackup backupPactFlag = liftIO $ do
        nextBackupIdentifier <- getNextBackupIdentifier
        join $ atomically $ do
            current <- readTVar globalCurrentBackup
            case current of
                Nothing -> do
                    writeTVar globalCurrentBackup (Just nextBackupIdentifier)
                    return $ doBackup backupPactFlag nextBackupIdentifier
                Just b -> do
                    let logg = logFunctionText (Backup._backupLogger backupEnv) Info $
                            "requested backup, but backup " <> b <> " is already in progress."
                    return $ b <$ logg
    doBackup backupPactFlag nextBackupIdentifier = do
        _ <- async $ do
            Backup.makeBackup backupEnv options `finally`
                atomically (writeTVar globalCurrentBackup Nothing)
        return nextBackupIdentifier
      where
        options = Backup.BackupOptions
            { Backup._backupIdentifier = T.unpack nextBackupIdentifier
            , Backup._backupPact = backupPactFlag
            }
    checkBackup backupIdentifier = liftIO $ do
        status <- Backup.checkBackup backupEnv backupIdentifier
        maybe (throwM noSuchBackup) pure status

getNextBackupIdentifier :: IO Text
getNextBackupIdentifier = do
    Time (epochToNow :: TimeSpan Integer) <- getCurrentTimeIntegral
    return $ toText (timeSpanToMicros epochToNow)
