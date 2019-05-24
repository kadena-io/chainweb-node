{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.ByteString as BS
import Data.String
import Data.String.Conv
import Database.SQLite3.Direct as SQ3

import System.Directory (removeFile)
import System.IO.Extra

-- pact


import Pact.Types.Persistence
import Pact.Types.Pretty (prettyString, viaShow)
import Pact.Types.Runtime (throwDbError)
import Pact.Types.SQLite
import Pact.Types.Term(KeySetName(..), NamespaceName(..), ModuleName(..), PactId(..))
import Pact.Types.Util (AsString(..))

-- chainweb

import Chainweb.Pact.Backend.Types


runBlockEnv :: MVar (BlockEnv SQLiteEnv) -> BlockHandler SQLiteEnv a -> IO a
runBlockEnv e m = modifyMVar e $
  \(BlockEnv  db bs)  -> do
    (a,s) <- runStateT (runReaderT (runBlockHandler m) db) bs
    return (BlockEnv db s, a)

callDb :: (MonadReader SQLiteEnv m, MonadIO m) => (Database -> IO b) -> m b
callDb action = do
  c <- view sConn
  liftIO $ action c

withPreBlockSavepoint :: BlockHandler SQLiteEnv a -> BlockHandler SQLiteEnv a
withPreBlockSavepoint action = do
  beginSavepoint "PREBLOCK"
  result <- tryAny action
  case result of
    Left (SomeException err) -> do
      rollbackSavepoint "PREBLOCK"
      throwM err
    Right r -> do
      commitSavepoint "PREBLOCK"
      return r

beginSavepoint :: SavepointName ->  BlockHandler SQLiteEnv ()
beginSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "SAVEPOINT " <> (Utf8 name)

commitSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
commitSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "RELEASE SAVEPOINT " <> Utf8 name

rollbackSavepoint :: SavepointName -> BlockHandler SQLiteEnv ()
rollbackSavepoint (SavepointName name) =
  callDb $ \db -> exec_ db $ "ROLLBACK TRANSACTION TO SAVEPOINT " <> Utf8 name

newtype SavepointName = SavepointName BS.ByteString
  deriving (Eq, Ord, IsString)
  deriving newtype Show

withBlockEnv :: a -> (MVar a -> IO b) -> IO b
withBlockEnv blockenv f = newMVar blockenv >>= f

readBlockEnv :: (a -> b) -> MVar a -> IO b
readBlockEnv f mvar = do
  a <- readMVar mvar
  return (f a)

withSQLiteConnection :: String -> [Pragma] -> Bool -> (SQLiteEnv -> IO c) -> IO c
withSQLiteConnection file ps todelete action = do
  result <- bracket opener (close . _sConn) action
  when todelete (removeFile file)
  return result
  where
    opener = do
      e <- open $ fromString file
      case e of
        Left (err,msg) ->
          throwM $ userError $ "PactInternalError: Can't open db with "  <> show err <> ": " <> show msg
        Right r -> return $ mkSQLiteEnv r
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
expectSingleRowCol s v = throwDbError $ "PactInternalError: " <> prettyString s <> " expected single row and column result, got: " <> viaShow v

expectSingle :: Show a => String -> [a] -> IO a
expectSingle _ [s] = return s
expectSingle desc v = throwDbError $ "Expected single-" <> prettyString desc <> " result, got: " <> viaShow v
