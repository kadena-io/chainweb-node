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

-- initRelationalCheckpointerEnv ::
--      Db -> Logger -> GasEnv -> IO CheckpointEnv
-- initRelationalCheckpointerEnv db loggr gasEnv =
--   pure $
--     CheckpointEnv
--       { _cpeCheckpointer = undefined $ CheckpointerNew $ checkpointerNew db
--       , _cpeLogger = loggr
--       , _cpeGasEnv = gasEnv
--       }


initRelationalCheckpointer :: BlockState -> SQLiteEnv -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer bstate sqlenv loggr gasEnv = do
  db <- newMVar (BlockEnv sqlenv bstate)
  runBlockEnv db initSchema
  return $
    CheckpointEnv
      { _cpeCheckpointer =
          Checkpointer
            (doRestore db)
            -- (doRestoreInitial db)
            (doSave db)
            (doDiscard db)
      , _cpeLogger = loggr
      , _cpeGasEnv = gasEnv
      }

type Db = MVar (BlockEnv SQLiteEnv)

doRestore :: Db -> Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore dbenv (Just (bh, hash)) = do
  runBlockEnv dbenv $ do
    withSavepoint PreBlock (handleVersion bh hash)
    beginSavepoint Block
  return $ PactDbEnv' $ PactDbEnv chainwebpactdb dbenv
doRestore dbenv Nothing = do
  runBlockEnv dbenv $ do
    r <- callDb "doRestoreInitial"
         $ \db -> qry db
                  "SELECT COUNT(*)\
                  \ FROM BlockHistory\
                  \ WHERE blockheight = ? AND hash = ?;"
              [SInt 0, SBlob (Data.Serialize.encode nullBlockHash)]
              [RInt]
    liftIO (expectSingle "row" r) >>= \case
      [SInt 0] -> beginSavepoint Block
      _ -> internalError "restoreInitial: The genesis state cannot be recovered!"
  return $ PactDbEnv' $ PactDbEnv chainwebpactdb dbenv

-- -- Assumes that BlockState has been initialized properly.
-- -- This function only works (as in not throw an error) at genesis.
-- doRestoreInitial :: Db -> IO PactDbEnv'
-- doRestoreInitial dbenv = do
--   runBlockEnv dbenv $ do
--     r <- callDb "doRestoreInitial"
--          $ \db -> qry db
--                   "SELECT COUNT(*)\
--                   \ FROM BlockHistory\
--                   \ WHERE blockheight = ? AND hash = ?;"
--               [SInt 0, SBlob (Data.Serialize.encode nullBlockHash)]
--               [RInt]
--     liftIO (expectSingle "row" r) >>= \case
--       [SInt 0] -> beginSavepoint Block
--       _ -> internalError "restoreInitial: The genesis state cannot be recovered!"
--   return $ PactDbEnv' $ PactDbEnv chainwebpactdb dbenv

doSave :: Db -> BlockHash -> IO ()
doSave dbenv hash = runBlockEnv dbenv $ do
  commitSavepoint Block
  (BlockVersion height _) <- gets _bsBlockVersion
  blockHistoryInsert height hash

doDiscard :: Db -> IO ()
doDiscard dbenv = runBlockEnv dbenv (rollbackSavepoint Block)
