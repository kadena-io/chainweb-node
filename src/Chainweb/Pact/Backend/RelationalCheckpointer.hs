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
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Serialize hiding (get)

import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))

-- pact
import Pact.Types.SQLite
import Pact.Types.Pretty
import Pact.Types.Runtime

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types

initRelationalCheckpointer ::
     Db -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer db loggr gasEnv = do
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

innerRestore :: Db -> BlockHeight -> BlockHash -> Maybe ExecutionMode -> IO (Either String (BlockEnv SQLiteEnv))
innerRestore dbenv bh hsh mmode = runBlockEnv dbenv $ do
  e <- tryAny $ do
    withPreBlockSavepoint $ do
      bsMode .= mmode
      v <- detectVersionChange bh hsh
      versionUpdate v
    beginSavepoint "BLOCK"
  case e of
    Left err -> return $ Left ("restore :" <> show err)
    Right _ -> Right <$> do
      senv <- ask
      bs <- get
      return $ BlockEnv senv bs

-- Assumes that BlockState has been initialized properly.
innerRestoreInitial :: Db -> Maybe ExecutionMode -> IO (Either String (BlockEnv SQLiteEnv))
innerRestoreInitial dbenv _mmode = runBlockEnv dbenv $ do
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
        beginSavepoint "BLOCK"
        return $ BlockEnv senv bs
      _ -> return $ Left $ "restoreInitial: The genesis state cannot be recovered!"

innerSave ::
     Db -> BlockHeight -> BlockHash -> TxId -> IO (Either String ())
innerSave dbenv bh hsh txid = do
    modifyMVar_ dbenv (pure . set (benvBlockState . bsTxId) txid)
    result <- tryAny $ runBlockEnv dbenv $ do
      commitSavepoint "BLOCK"
      blockHistoryInsert bh hsh
      bs <- gets _bsBlockVersion
      versionHistoryInsert bs
    return $ case result of
      Left err -> Left $ "save: " <> show err
      Right _ -> Right ()

innerDiscard :: Db -> IO (Either String ())
innerDiscard dbenv = do
  result <- tryAny $ runBlockEnv dbenv $ do
    rollbackSavepoint "BLOCK"
  return $ case result of
    Left err -> Left $ "discard: " <> show err
    Right _ -> Right ()

expectSingle :: Show a => String -> [a] -> IO a
expectSingle _ [s] = return s
expectSingle desc v = throwDbError $ "Expected single-" <> prettyString desc <> " result, got: " <> viaShow v
