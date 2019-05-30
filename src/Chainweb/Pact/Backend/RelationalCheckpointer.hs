{-# LANGUAGE OverloadedStrings   #-}

-- |
-- Module: Chainweb.Pact.RelationalCheckpointer
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emmanuel Denloye <emmanuel@kadena.io>
-- Stability: experimental
--
-- Pact Checkpointer for Chainweb
module Chainweb.Pact.Backend.RelationalCheckpointer where

import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Serialize hiding (get)

import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))

-- pact
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils

initRelationalCheckpointerEnv ::
     Db -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointerEnv db loggr gasEnv = do
  let checkpointer =
        CheckpointerNew
          { restoreNew = innerRestore db
          , restoreInitialNew = innerRestoreInitial db
          , saveNew = innerSave db
          , discardNew = innerDiscard db
          }
  return $
    CheckpointEnv
      { _cpeCheckpointer = undefined $ checkpointer
      , _cpeLogger = loggr
      , _cpeGasEnv = gasEnv
      }

initRelationalCheckpointerNew :: Db -> Logger -> GasEnv -> IO (CheckpointEnvNew SQLiteEnv)
initRelationalCheckpointerNew db loggr gasEnv = do
  let checkpointer =
        CheckpointerNew
          { restoreNew = innerRestore db
          , restoreInitialNew = innerRestoreInitial db
          , saveNew = innerSave db
          , discardNew = innerDiscard db
          }
  return $
    CheckpointEnvNew
      { _cpeCheckpointerNew = checkpointer
      , _cpeLoggerNew = loggr
      , _cpeGasEnvNew = gasEnv
      }

type Db = MVar (BlockEnv SQLiteEnv)

type ParentHash = BlockHash

innerRestore :: Db -> BlockHeight -> Maybe ParentHash -> IO (Either String (BlockEnv SQLiteEnv))
innerRestore dbenv bh hsh = runBlockEnv dbenv $ do
  e <- tryAny $ do
    withSavepoint PreBlock (handleVersion bh hsh)
    beginSavepoint Block
  case e of
    Left err -> return $ Left ("restore :" <> show err)
    Right _ -> Right <$> do
      senv <- ask
      bs <- get
      return $ BlockEnv senv bs

-- Assumes that BlockState has been initialized properly.
innerRestoreInitial :: Db -> IO (Either String (BlockEnv SQLiteEnv))
innerRestoreInitial dbenv = runBlockEnv dbenv $ do
    r <- callDb (\db ->
              qry db
              "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
              [SInt 0, SBlob (Data.Serialize.encode nullBlockHash)]
              [RInt])
    single <- liftIO $ expectSingle "row" r
    case single of
      [SInt 0] -> Right <$> do
        senv <- ask
        bs <- get
        beginSavepoint Block
        return $ BlockEnv senv bs
      _ -> return $ Left $ "restoreInitial: The genesis state cannot be recovered!"

innerSave ::
     Db -> BlockHeight -> BlockHash -> IO (Either String ())
innerSave dbenv bh hsh = do
    result <- tryAny $ runBlockEnv dbenv $ do
      commitSavepoint Block
      blockHistoryInsert bh hsh
      {- move to version maintenance -}
      bs <- gets _bsBlockVersion
      versionHistoryInsert bs
      {- move to version maintenance -}
    return $ case result of
      Left err -> Left $ "save: " <> show err
      Right _ -> Right ()

innerDiscard :: Db -> IO (Either String ())
innerDiscard dbenv = do
  result <- tryAny $ runBlockEnv dbenv $ do
    rollbackSavepoint Block
  return $ case result of
    Left err -> Left $ "discard: " <> show err
    Right _ -> Right ()
