{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Control.Exception hiding (try)
import Control.Exception.Safe hiding (bracket)
import Control.Lens
import Control.Monad.State.Strict

import Control.Monad.Reader

import Data.String
import Data.String.Conv
import Data.ByteString hiding (pack)
import Data.Text (pack, Text)
import Database.SQLite3.Direct as SQ3

import System.Directory (removeFile)
import System.IO.Extra

-- pact


import Pact.Types.Persistence
import Pact.Types.SQLite
import Pact.Types.Term(KeySetName(..), NamespaceName(..), ModuleName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types

runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> BlockHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv  db bs)  -> do
    (a,s) <- runStateT (runReaderT (runBlockHandler m) db) bs
    return (BlockEnv db s, a)

callDb :: (MonadCatch m, MonadReader SQLiteEnv m, MonadIO m) => Text -> (Database -> IO b) -> m b
callDb callerName action = do
  c <- view sConn
  res <- tryAny $ liftIO $ action c
  case res of
    Left err -> internalError $ "callDb (" <> callerName <> "): " <> (pack $ show err)
    Right r -> return r

withSavepoint :: SavepointName -> BlockHandler SQLiteEnv a -> BlockHandler SQLiteEnv a
withSavepoint name action = do
  beginSavepoint name
  result <- try action
  case result of
    Right r -> do
      commitSavepoint name
      return r
    Left (PactInternalError err) -> do
      rollbackSavepoint name
      internalError $
        "withSavepoint (" <> asString name <> "): " <> (pack $ show err)
    Left err -> do
      rollbackSavepoint name
      internalError $ "withSavepoint: The impossible happened: " <> (pack $ show err)


beginSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
beginSavepoint name =
  callDb "beginSavepoint" $ \db -> exec_ db $ "SAVEPOINT [" <> toS (asString name) <> "];"

commitSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
commitSavepoint name =
  callDb "commitSavepoint" $ \db -> exec_ db $ "RELEASE SAVEPOINT [" <> toS (asString name) <> "];"

rollbackSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
rollbackSavepoint name =
  callDb "rollbackSavepoint" $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT [" <> toS (asString name) <> "];"

data SavepointName = Block | DbTransaction |  PreBlock
  deriving (Eq, Ord, Enum)

instance Show SavepointName where
  show (Block) = "block"
  show (DbTransaction) = "transaction"
  show (PreBlock) = "preblock"

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
      void $ close $ _sConn c
      when todelete (removeFile file)
    opener = do
      e <- open $ fromString file
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
