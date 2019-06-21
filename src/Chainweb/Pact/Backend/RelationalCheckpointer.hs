{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Pact.Backend.RelationalCheckpointer
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emmanuel Denloye <emmanuel@kadena.io>
-- Stability: experimental
--
-- Pact Checkpointer for Chainweb
module Chainweb.Pact.Backend.RelationalCheckpointer
  ( initRelationalCheckpointer
  , initRelationalCheckpointer'
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (gets, get)

import Data.Serialize hiding (get)
import Data.List (intercalate)
-- import qualified Data.Text as T
  -- import qualified Data.Text.IO as T

import Prelude hiding (log)

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..), Logging(..))
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)


initRelationalCheckpointer :: BlockState -> SQLiteEnv -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer bstate sqlenv loggr gasEnv =
  snd <$> initRelationalCheckpointer' bstate sqlenv loggr gasEnv

-- for testing
initRelationalCheckpointer' :: BlockState -> SQLiteEnv -> Logger -> GasEnv -> IO (PactDbEnv', CheckpointEnv)
initRelationalCheckpointer' bstate sqlenv loggr gasEnv = do
  let dbenv = BlockDbEnv sqlenv loggr
  db <- newMVar (BlockEnv dbenv bstate)
  runBlockEnv db initSchema
  return $
    (PactDbEnv' (PactDbEnv chainwebPactDb db),
     CheckpointEnv
      { _cpeCheckpointer =
          Checkpointer
            (doRestore db)
            (doSave db)
            (doDiscard db)
      , _cpeLogger = loggr
      , _cpeGasEnv = gasEnv
      })

type Db = MVar (BlockEnv SQLiteEnv)


dump :: BlockHandler SQLiteEnv ()
dump = do
  bstate <- get
  log "INFO" $ "dumping bstate here:" <> show bstate
  log "INFO" "dumping blockhistory here"
  txts <- callDb "dumping" $ \db -> do
      qry_ db "SELECT * FROM BlockHistory;" [RInt,RBlob,RInt] >>= mapM go
  log "INFO" (intercalate ":" txts)
    -- log "Info" "dumping versionhistory\n"
    -- qry_ db "SELECT * FROM VersionHistory;" [RInt,RInt,RInt] >>= print

go :: [SType] -> IO String
go [SInt a, SBlob blob, SInt b] = do
  let getshit = either error id
  return $
    "row= " <> show a <> " " <>
    show ((getshit $ Data.Serialize.decode blob) :: BlockHash) <>
    " " <>
    show b
go _ = error "whatever"

doRestore :: Db -> Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore dbenv (Just (bh, hash)) = do
  runBlockEnv dbenv $ do
    dump
    txid <- withSavepoint PreBlock $ handleVersion bh hash
    dump
    assign bsTxId txid
    beginSavepoint Block
  return $ PactDbEnv' $ PactDbEnv chainwebPactDb dbenv
doRestore dbenv Nothing = do
  runBlockEnv dbenv $ do
    r <- callDb "doRestoreInitial"
         $ \db -> qry db
                  "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
              [SInt 0, SBlob (encode nullBlockHash)]
              [RInt]
    liftIO (expectSingle "row" r) >>= \case
      [SInt 0] -> do
        withSavepoint DbTransaction $
           callDb "doRestoreInitial: resetting tables" $ \db -> do
             exec_ db "DELETE FROM BlockHistory;"
             exec_ db "DELETE FROM VersionHistory;"
             exec_ db "DELETE FROM [SYS:KeySets];"
             exec_ db "DELETE FROM [SYS:Modules];"
             exec_ db "DELETE FROM [SYS:Namespaces];"
             exec_ db "DELETE FROM [SYS:Pacts];"
             exec_ db "DELETE FROM UserTables;"
             tblNames <- qry_ db "SELECT tablename FROM UserTables;" [RText]
             forM_ tblNames $ \tbl -> case tbl of
               [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
               _ -> internalError "Something went wrong when resetting tables."
        beginSavepoint Block
      _ -> internalError "restoreInitial: The genesis state cannot be recovered!"
  return $ PactDbEnv' $ PactDbEnv chainwebPactDb dbenv

doSave :: Db -> BlockHash -> IO ()
doSave dbenv hash = runBlockEnv dbenv $ do
  commitSavepoint Block
  nextTxId <- gets _bsTxId
  (BlockVersion height _) <- gets _bsBlockVersion
  blockHistoryInsert height hash nextTxId

doDiscard :: Db -> IO ()
doDiscard dbenv = runBlockEnv dbenv $ rollbackSavepoint Block
