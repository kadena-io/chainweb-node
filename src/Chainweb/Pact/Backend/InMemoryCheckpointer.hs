{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module: Chainweb.Pact.InMemoryCheckpointer
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.InMemoryCheckpointer
    ( initInMemoryCheckpointEnv
    ) where

import Control.Concurrent.MVar

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Text (pack)


import Pact.Interpreter
import Pact.PersistPactDb
import Pact.Persist.Pure
import Pact.Types.Logger
import Pact.Types.Runtime hiding (hash)

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types (internalError)



data Store = Store
  { _theStore :: HashMap (BlockHeight, BlockHash) (DbEnv PureDb)
  , _tempSaveHeight :: Maybe BlockHeight
  , _dbenv :: MVar (DbEnv PureDb)
  }

initInMemoryCheckpointEnv :: Loggers -> Logger -> GasEnv -> IO CheckpointEnv
initInMemoryCheckpointEnv loggers logger gasEnv = do
    pdenv@(PactDbEnv _ env) <- mkPureEnv loggers
    initSchema pdenv
    genesis <- readMVar env
    inmem <- newMVar (Store mempty Nothing env)
    return $
        (CheckpointEnv
            { _cpeCheckpointer =
                Checkpointer
                    (doRestore genesis inmem)
                    (doSave inmem)
                    (doDiscard inmem)
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            })

doRestore :: DbEnv PureDb -> MVar Store ->  Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore _ lock (Just (height, hash)) =
    modifyMVarMasked lock $ \store -> do
      case HMS.lookup (pred height, hash) (_theStore store) of
            Just dbenv -> do
              mdbenv <- newMVar dbenv
              let store' = store { _tempSaveHeight = Just height, _dbenv = mdbenv }
              return $ (store', PactDbEnv' (PactDbEnv pactdb mdbenv))
            Nothing -> internalError $
              "InMemoryCheckpointer: Restore not found: height=" <> pack (show (pred height))
              <> ", hash=" <> pack (show hash)
              <> ", known=" <> pack (show (HMS.keys (_theStore store)))
doRestore genesis lock Nothing =
  modifyMVarMasked lock $ \_ -> do
    gen <- newMVar genesis
    return $! (Store mempty Nothing gen, PactDbEnv' $ PactDbEnv pactdb gen)

doSave :: MVar Store -> BlockHash -> IO ()
doSave lock hash = modifyMVar_ lock $ \(Store store mheight dbenv) -> case mheight of
  Nothing -> do
    env <- readMVar dbenv
    return $ Store (HMS.insert (BlockHeight 0, hash) env store) mheight dbenv
  Just height -> do
    env <- readMVar dbenv
    return $ Store (HMS.insert (height, hash) env store) mheight dbenv

doDiscard :: MVar Store -> IO ()
doDiscard _ = return ()
