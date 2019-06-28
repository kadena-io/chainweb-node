{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Pact.ChainwebPactDb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--

module Chainweb.Pact.Backend.Utils where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Exception.Safe (tryAny)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State.Strict

import Control.Monad.Reader

import Data.Bits
import Data.ByteString hiding (pack)
import Data.String
import Data.String.Conv
import Data.Text (Text, pack)
import Database.SQLite3.Direct as SQ3

import Prelude hiding (log)

import System.Directory (removeFile)
import System.IO.Extra

-- pact

import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term
    (KeySetName(..), ModuleName(..), NamespaceName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.Pact.Backend.SQLite.DirectV2
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types

runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> BlockHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv  dbenv bs)  -> do
    (a,s) <- runStateT (runReaderT (runBlockHandler m) dbenv) bs
    return (BlockEnv dbenv s, a)

callDb :: (MonadCatch m, MonadReader (BlockDbEnv SQLiteEnv) m, MonadIO m) => Text -> (Database -> IO b) -> m b
callDb callerName action = do
  c <- view (bdbenvDb . sConn)
  res <- tryAny $ liftIO $ action c
  case res of
    Left err -> internalError $ "callDb (" <> callerName <> "): " <> (pack $ show err)
    Right r -> return r

withSavepoint
    :: SavepointName
    -> BlockHandler SQLiteEnv a
    -> BlockHandler SQLiteEnv a
withSavepoint name action = mask $ \resetMask -> do
    resetMask $ beginSavepoint name
    go resetMask `catches` handlers
  where
    go resetMask = do
        r <- resetMask action `onException` rollbackSavepoint name
        commitSavepoint name
        liftIO $ evaluate r
    throwErr s = internalError $ "withSavepoint (" <> asString name <> "): " <> pack s
    handlers = [ Handler $ \(e :: PactException) -> throwErr (show e)
               , Handler $ \(e :: SomeException) -> throwErr ("non-pact exception: " <> show e)
               ]

-- for debugging
withTrace :: Database -> (Utf8 -> IO ()) -> IO a -> IO a
withTrace db tracer dbaction = do
  setTrace db (Just tracer)
  a <- dbaction
  setTrace db Nothing
  return a

beginSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
beginSavepoint name =
  callDb "beginSavepoint" $ \db -> exec_ db $ "SAVEPOINT [" <> toS (asString name) <> "];"

commitSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
commitSavepoint name =
  callDb "commitSavepoint" $ \db -> exec_ db $ "RELEASE SAVEPOINT [" <> toS (asString name) <> "];"

rollbackSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
rollbackSavepoint name =
  callDb "rollbackSavepoint" $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> toS (asString name) <> "];"

data SavepointName = RewindSavepoint | Block | DbTransaction |  PreBlock | PactDbTransaction
  deriving (Eq, Ord, Enum)

instance Show SavepointName where
  show RewindSavepoint = "rewind"
  show Block = "block"
  show DbTransaction = "db-transaction"
  show PreBlock = "preblock"
  show PactDbTransaction = "pact-transaction"

instance AsString SavepointName where
  asString = Data.Text.pack . show

withBlockEnv :: a -> (MVar a -> IO b) -> IO b
withBlockEnv blockenv f = newMVar blockenv >>= f

readBlockEnv :: (a -> b) -> MVar a -> IO b
readBlockEnv f = fmap f . readMVar

withSQLiteConnection :: String -> [Pragma] -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps todelete action = bracket opener closer action
  where
    closer c = do
      void $ close_v2 $ _sConn c
      when todelete (removeFile file)
    opener = do
      e <- open2 file
      case e of
        Left (err, msg) ->
          internalError $
            "withSQLiteConnection: Can't open db with "
            <> asString (show err) <> ": " <> asString (show msg)
        Right r -> do
          runPragmas r ps
          return $ mkSQLiteEnv r
    mkSQLiteEnv connection =
      SQLiteEnv connection
      (SQLiteConfig file ps)

withTempSQLiteConnection :: [Pragma] -> (SQLiteEnv -> IO c) -> IO c
withTempSQLiteConnection ps action =
  withTempFile (\file -> withSQLiteConnection file ps False action)

domainTableName :: Domain k v -> Utf8
domainTableName = Utf8 . toS . asString

convKeySetName :: KeySetName -> Utf8
convKeySetName (KeySetName name) = Utf8 $ toS name

convModuleName :: ModuleName -> Utf8
convModuleName (ModuleName name _) = Utf8 $ toS name

convNamespaceName :: NamespaceName -> Utf8
convNamespaceName (NamespaceName name) = Utf8 $ toS name

convRowKey :: RowKey -> Utf8
convRowKey (RowKey name) = Utf8 $ toS name

convPactId :: PactId -> Utf8
convPactId = Utf8 . toS . show

expectSingleRowCol :: Show a => String -> [[a]] -> IO a
expectSingleRowCol _ [[s]] = return s
expectSingleRowCol s v =
  internalError $
  "expectSingleRowCol: "
  <> asString s <>
  " expected single row and column result, got: "
  <> asString (show v)

expectSingle :: Show a => String -> [a] -> IO a
expectSingle _ [s] = return s
expectSingle desc v =
  internalError $
  "Expected single-" <> asString (show desc) <> " result, got: " <>
  asString (show v)


instance StringConv Text Utf8 where
  strConv l = Utf8 . strConv l

instance StringConv Utf8 Text where
  strConv l (Utf8 bytestring) = strConv l bytestring

instance StringConv ByteString Utf8 where
  strConv l = Utf8 . strConv l

instance StringConv Utf8 ByteString where
  strConv l (Utf8 bytestring) = strConv l bytestring

instance StringConv String Utf8 where
  strConv l = Utf8 . strConv l

instance StringConv Utf8 String where
  strConv l (Utf8 bytestring) = strConv l bytestring

fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = NORMAL",
  "journal_mode = WAL",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]


open2 :: String -> IO (Either (Error, Utf8) Database)
open2 file = open_v2 (fromString file) (collapseFlags [sqlite_open_readwrite , sqlite_open_create , sqlite_open_fullmutex]) Nothing
-- Nothing corresponds to the nullPtr

collapseFlags :: [SQLiteFlag] -> SQLiteFlag
collapseFlags xs =
  if Prelude.null xs then error "collapseFlags: You must pass a non-empty list"
  else Prelude.foldr1 (.|.) xs

sqlite_open_readwrite, sqlite_open_create, sqlite_open_fullmutex :: SQLiteFlag
sqlite_open_readwrite = 0x00000002
sqlite_open_create = 0x00000004
sqlite_open_fullmutex = 0x00010000
