{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | An endpoint for making database backups. Also used to synchronize
-- databases between nodes.

module Chainweb.RestAPI.Backup
    ( BackupApi
    , newBackupApi
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import System.IO.Unsafe
import System.LogLevel

import Servant
import Web.DeepRoute
import Web.DeepRoute.Wai

import qualified Chainweb.Backup as Backup
import Chainweb.Logger
import Chainweb.Time

import Chainweb.RestAPI.Utils
import Chainweb.Version
import Chainweb.Utils

type BackupApi_
  =    "make-backup" :> QueryFlag "backupPact" :> PostAccepted '[PlainText] Text
  :<|> "check-backup" :> Capture "backup-name" FilePath :> Get '[PlainText] Backup.BackupStatus

type BackupApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc BackupApi_

globalCurrentBackup :: TVar (Maybe Text)
globalCurrentBackup = unsafePerformIO $! newTVarIO Nothing
{-# NOINLINE globalCurrentBackup #-}

makeBackupHandler :: (MonadIO m, Logger logger) => Backup.BackupEnv logger -> Bool -> m Text
makeBackupHandler backupEnv backupPactFlag = liftIO $ do
    nextBackupIdentifier <- getNextBackupIdentifier
    join $ atomically $ do
        current <- readTVar globalCurrentBackup
        case current of
            Nothing -> do
                writeTVar globalCurrentBackup (Just nextBackupIdentifier)
                return $ doBackup nextBackupIdentifier
            Just b -> do
                let logg = logFunctionText (Backup._backupLogger backupEnv) Info $
                        "requested backup, but backup " <> b <> " is already in progress."
                return $ b <$ logg
    where
    doBackup nextBackupIdentifier = do
        _ <- async $ do
            Backup.makeBackup backupEnv options `finally`
                atomically (writeTVar globalCurrentBackup Nothing)
        return nextBackupIdentifier
        where
        options = Backup.BackupOptions
            { Backup._backupIdentifier = T.unpack nextBackupIdentifier
            , Backup._backupPact = backupPactFlag
            }

checkBackupHandler :: (MonadIO m, Logger logger) => Backup.BackupEnv logger -> FilePath -> m Backup.BackupStatus
checkBackupHandler backupEnv backupIdentifier = liftIO $ do
    status <- Backup.checkBackup backupEnv backupIdentifier
    maybe noSuchBackup pure status

noSuchBackup :: IO a
noSuchBackup = errorWithStatus notFound404 "no such backup"

newBackupApi :: Logger logger => Backup.BackupEnv logger -> Route Application
newBackupApi backupEnv = fold
    [ seg "make-backup" $ endpoint methodPost "text/plain" $ \req resp -> do
        backupPact <- getParams req (queryParamOptional "backup-pact") >>= \case
            Nothing -> return False
            Just QueryParamNoValue -> return True
            Just (QueryParamValue (_ :: Text)) ->
                errorWithStatus badRequest400 "backupPact must not be set to a value, it should be empty if provided"
        resp . responseLBS accepted202 [] . LBS.fromStrict . T.encodeUtf8 =<< makeBackupHandler backupEnv backupPact
    , seg "check-backup" $ capture $ endpoint methodGet "text/plain" $ \backupIdent _ resp -> do
        resp . responseLBS ok200 [] . LBS.fromStrict . T.encodeUtf8 . toText =<< checkBackupHandler backupEnv backupIdent
    ]

getNextBackupIdentifier :: IO Text
getNextBackupIdentifier = do
    Time (epochToNow :: TimeSpan Integer) <- getCurrentTimeIntegral
    return $ microsToText (timeSpanToMicros epochToNow)
