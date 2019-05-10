{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
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
-- import Pact.PersistPactDb (DbEnv(..))
-- import Pact.Persist.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types

initRelationalCheckpointer ::
     Db -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer dbconn loggr gasEnv = do
  let checkpointer =
        CheckpointerNew
          { restoreNew = innerRestore dbconn
          , restoreInitialNew = innerRestoreInitial dbconn
          , saveNew = innerSave dbconn
          , saveInitialNew = innerSaveInitial dbconn
          , discardNew = innerDiscard dbconn
          }
  return $
    CheckpointEnv
      { _cpeCheckpointer = undefined $ checkpointer
      , _cpeLogger = loggr
      , _cpeGasEnv = gasEnv
      }

type Db = MVar (CWDbEnv SQLiteEnv)

innerRestore :: Db -> BlockHeight -> BlockHash -> IO (Either String (SQLiteEnv, TxId, Maybe ExecutionMode))
innerRestore dbenv bh hsh = runCWDb dbenv $ runExceptT $ withExceptT ((mappend "restore :") .show) $ do
  -- TODO: Refactor so that savepoint is explicit!
  ExceptT $ tryAny $ preBlock $ do
    v <- detectVersionChange bh hsh
    versionUpdate v
  ExceptT $ tryAny $ beginSavepoint "BLOCK"
  sqlenv <- ask
  txid <- gets _bsTxId
  mode <- gets _bsMode
  return (sqlenv, txid, mode)

-- Assumes that BlockState has been initialized properly.
innerRestoreInitial :: Db -> IO (Either String (SQLiteEnv, TxId, Maybe ExecutionMode))
innerRestoreInitial dbenv = runCWDb dbenv $ do
    r <- callDb (\db ->
              qry db
              "SELECT * FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
              [SInt 0, SBlob (Data.Serialize.encode nullBlockHash)]
              [RInt, RBlob])
    single <- liftIO $ expectSing "row" r
    case single of
      [SInt _, SBlob _] -> do
        sqlenv <- ask
        txid <- gets _bsTxId
        mode <- gets _bsMode
        return $ Right (sqlenv, txid, mode)
      _ -> return $ Left $ "restoreInitial: The genesis state cannot be recovered!"

innerSave ::
     Db -> BlockHeight -> BlockHash -> (SQLiteEnv, TxId) -> IO (Either String ())
innerSave dbenv bh hsh (sqlenv, txid) = do
    modifyMVar_ dbenv (pure . set (cwBlockState . bsTxId) txid . set cwDb sqlenv)
    result <- tryAny $ runCWDb dbenv $ do
      commitSavepoint "BLOCK"
      systemInsert (BlockHistory bh hsh)
      bs <- gets _bsBlockVersion
      systemInsert (VersionHistory bs)
    return $ case result of
      Left err -> Left $ "save: " <> show err
      Right _ -> Right ()

-- We could remove this function entirely.
innerSaveInitial :: Db -> (SQLiteEnv, TxId) -> IO (Either String ())
innerSaveInitial _dbenv _ = return (Right ())

innerDiscard :: Db -> (SQLiteEnv, TxId) -> IO (Either String ())
innerDiscard dbenv (sqlenv, txid) = do
  modifyMVar_ dbenv (pure . set (cwBlockState . bsTxId) txid . set cwDb sqlenv)
  result <- tryAny $ runCWDb dbenv $ do
    rollbackSavepoint "BLOCK"
  return $ case result of
    Left err -> Left $ "discard: " <> show err
    Right _ -> Right ()

expectSing :: Show a => String -> [a] -> IO a
expectSing _ [s] = return s
expectSing desc v = throwDbError $ "Expected single-" <> prettyString desc <> " result, got: " <> viaShow v
