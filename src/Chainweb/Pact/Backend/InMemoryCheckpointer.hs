{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Pact.Persist.Pure
import Pact.PersistPactDb
import Pact.Types.Logger
import Pact.Types.Runtime hiding (hash)

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types (internalError)



data Store = Store
  { _theStore :: HashMap (BlockHeight, BlockHash) (DbEnv PureDb)
  , _lastBlock :: Maybe (BlockHeight, BlockHash)
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
                    (doGetLatest inmem)
                    id  -- in-mem doesn't require tx rewind
                    (doLookupBlock inmem)
                    (doGetBlockParent inmem)
                    (error "TODO: registerSuccessfulTx")
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            })

doRestore :: DbEnv PureDb -> MVar Store ->  Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore _ lock (Just (height, hash)) =
    modifyMVarMasked lock $ \store -> do
      let bh = (pred height, hash)
      case HMS.lookup bh (_theStore store) of
            Just dbenv -> do
              mdbenv <- newMVar dbenv
              let !store' = store { _lastBlock = Just bh, _dbenv = mdbenv }
              return $! (store', PactDbEnv' (PactDbEnv pactdb mdbenv))
            Nothing -> internalError $
              "InMemoryCheckpointer: Restore not found: height=" <> pack (show (pred height))
              <> ", hash=" <> pack (show hash)
              <> ", known=" <> pack (show (HMS.keys (_theStore store)))
doRestore genesis lock Nothing =
    modifyMVarMasked lock $ \_ -> do
      gen <- newMVar genesis
      return $! (Store mempty Nothing gen, PactDbEnv' $ PactDbEnv pactdb gen)

doSave :: MVar Store -> BlockHash -> IO ()
doSave lock hash = modifyMVar_ lock $ \(Store store mheight dbenv) -> do
    env <- readMVar dbenv
    let bh = case mheight of
               Nothing -> (BlockHeight 0, hash)
               Just (height, _) -> (succ height, hash)
    return $ Store (HMS.insert bh env store) (Just bh) dbenv

doGetLatest :: MVar Store -> IO (Maybe (BlockHeight, BlockHash))
doGetLatest lock = withMVar lock $ \(Store _ m _) -> return m

doDiscard :: MVar Store -> IO ()
doDiscard _ = return ()

doLookupBlock :: MVar Store -> (BlockHeight, BlockHash) -> IO Bool
doLookupBlock lock x = withMVar lock $ \(Store s _ _) ->
                       return $! HMS.member x s

doGetBlockParent :: MVar Store -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
doGetBlockParent _lock (_bh, _hash) = error msg
  where
    msg =
      "getBlockParent: This function shouldn't be used with the in-memory checkpointer.\
      \ There exists more than one choice of hash, and there is no intelligble way to distinguish the correct hash."
