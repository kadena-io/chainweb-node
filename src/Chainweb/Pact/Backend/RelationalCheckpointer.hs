{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Data.Serialize

-- pact

import Pact.Interpreter (PactDbEnv(..))
import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))
import Pact.Types.SQLite

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Service.Types (internalError)

initRelationalCheckpointer :: BlockState -> SQLiteEnv -> Logger -> GasEnv -> IO (PactDbEnv', CheckpointEnv)
initRelationalCheckpointer bstate sqlenv loggr gasEnv = do
  let dbenv = BlockDbEnv sqlenv loggr
  db <- newMVar (BlockEnv dbenv bstate)
  runBlockEnv db initSchema
  return $
    (PactDbEnv' (PactDbEnv chainwebpactdb db),
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

doRestore :: Db -> Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore dbenv (Just (bh, hash)) = do
  runBlockEnv dbenv $ do
    withSavepoint PreBlock $ handleVersion bh hash
    beginSavepoint Block
  return $ PactDbEnv' $ PactDbEnv chainwebpactdb dbenv
doRestore dbenv Nothing = do
  runBlockEnv dbenv $ do
    r <- callDb "doRestoreInitial"
         $ \db -> qry db
                  "SELECT COUNT(*) FROM BlockHistory WHERE blockheight = ? AND hash = ?;"
              [SInt 0, SBlob (Data.Serialize.encode nullBlockHash)]
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
             tblNames <- qry_ db "SELECT tablename FROM UserTables;" [RText]
             exec_ db "DELETE FROM UserTables;"
             forM_ tblNames $ \tbl -> case tbl of
               [SText t] -> exec_ db ("DROP TABLE [" <> t <> "];")
               _ -> internalError "Something went wrong when resetting tables."
        beginSavepoint Block
      _ -> internalError "restoreInitial: The genesis state cannot be recovered!"
  return $ PactDbEnv' $ PactDbEnv chainwebpactdb dbenv

doSave :: Db -> BlockHash -> IO ()
doSave dbenv hash = runBlockEnv dbenv $ do
  commitSavepoint Block
  (BlockVersion height _) <- gets _bsBlockVersion
  blockHistoryInsert height hash

doDiscard :: Db -> IO ()
doDiscard dbenv = runBlockEnv dbenv (rollbackSavepoint Block)
