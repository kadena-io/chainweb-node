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
import Control.Monad (when)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Text (pack)

import Pact.Interpreter
import Pact.Persist.Pure
import Pact.PersistPactDb
import Pact.Types.Hash (PactHash)
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
  , _playedTxs :: MVar (HashMap PactHash BlockHeight)
  }

initInMemoryCheckpointEnv :: Loggers -> Logger -> GasEnv -> IO CheckpointEnv
initInMemoryCheckpointEnv loggers logger gasEnv = do
    pdenv@(PactDbEnv _ env) <- mkPureEnv loggers
    initSchema pdenv
    genesis <- readMVar env
    played <- newMVar mempty
    inmem <- newMVar (Store mempty Nothing env played)
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
                    (doRegisterSuccessful inmem)
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            })

doRestore :: DbEnv PureDb -> MVar Store ->  Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore _ lock (Just (height, hash)) =
    modifyMVarMasked lock $ \store -> do
      let ph = pred height
      let bh = (ph, hash)
      case HMS.lookup bh (_theStore store) of
            Just dbenv -> do
              mdbenv <- newMVar dbenv
              played <- readMVar (_playedTxs store)
                        >>= newMVar . HMS.filterWithKey (\_ v -> v <= ph)

              let !store' = store { _lastBlock = Just bh
                                  , _dbenv = mdbenv
                                  , _playedTxs = played
                                  }
              return $! (store', PactDbEnv' (PactDbEnv pactdb mdbenv))
            Nothing -> internalError $
              "InMemoryCheckpointer: Restore not found: height=" <> pack (show (pred height))
              <> ", hash=" <> pack (show hash)
              <> ", known=" <> pack (show (HMS.keys (_theStore store)))
doRestore genesis lock Nothing =
    modifyMVarMasked lock $ \_ -> do
      gen <- newMVar genesis
      played <- newMVar mempty
      return $! (Store mempty Nothing gen played,
                 PactDbEnv' (PactDbEnv pactdb gen))

doSave :: MVar Store -> BlockHash -> IO ()
doSave lock hash = modifyMVar_ lock $ \(Store store mheight dbenv played) -> do
    env <- readMVar dbenv
    let bh = case mheight of
               Nothing -> (BlockHeight 0, hash)
               Just (height, _) -> (succ height, hash)
    return $ Store (HMS.insert bh env store) (Just bh) dbenv played

doGetLatest :: MVar Store -> IO (Maybe (BlockHeight, BlockHash))
doGetLatest lock = withMVar lock $ \(Store _ m _ _) -> return m

doDiscard :: MVar Store -> IO ()
doDiscard _ = return ()

doLookupBlock :: MVar Store -> (BlockHeight, BlockHash) -> IO Bool
doLookupBlock lock x = withMVar lock $ \(Store s _ _ _) ->
                       return $! HMS.member x s

doGetBlockParent :: MVar Store -> (BlockHeight, BlockHash) -> IO (Maybe BlockHash)
doGetBlockParent _lock (_bh, _hash) = error msg
  where
    msg =
      "getBlockParent: This function shouldn't be used with the in-memory checkpointer.\
      \ There exists more than one choice of hash, and there is no intelligble way to distinguish the correct hash."

doRegisterSuccessful :: MVar Store -> PactHash -> IO ()
doRegisterSuccessful lock hash =
    withMVar lock $
    \(Store _ lastBlock _ played) -> modifyMVar_ played $
    \mp -> do
        let b = HMS.member hash mp
        when b $ fail $ "duplicate transaction for hash " ++ show hash
        let !bh = getBh lastBlock
        return $! HMS.insert hash bh mp
  where
    getBh Nothing = 0
    getBh (Just (h, _)) = succ h
