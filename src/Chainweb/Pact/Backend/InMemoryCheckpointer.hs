{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (pack)
import Data.Tuple.Strict

import Pact.Interpreter
import Pact.Persist.Pure
import Pact.PersistPactDb
import Pact.Types.Hash (PactHash)
import Pact.Types.Logger

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types (internalError)



data Store = Store
  { _theStore :: HashMap (BlockHeight, BlockHash) (DbEnv PureDb)
  , _lastBlock :: Maybe (BlockHeight, BlockHash)
  , _dbenv :: MVar (DbEnv PureDb)
  , _playedTxs :: MVar (HashSet PactHash, HashMap PactHash (BlockHeight, BlockHash))
  }

initInMemoryCheckpointEnv :: Loggers -> Logger -> IO CheckpointEnv
initInMemoryCheckpointEnv loggers logger = do
    pdenv@(PactDbEnv _ env) <- mkPureEnv loggers
    initSchema pdenv
    genesis <- readMVar env
    played <- newMVar mempty
    inmem <- newMVar (Store mempty Nothing env played)
    return $
        (CheckpointEnv
            { _cpeCheckpointer =
                Checkpointer
                {
                      _cpRestore = doRestore genesis inmem
                    , _cpSave = doSave inmem
                    , _cpDiscard = doDiscard inmem
                    , _cpGetLatestBlock = doGetLatest inmem
                    , _cpBeginCheckpointerBatch = noop -- in-mem doesn't batch
                    , _cpCommitCheckpointerBatch = noop
                    , _cpDiscardCheckpointerBatch = doDiscard inmem
                    , _cpLookupBlockInCheckpointer = doLookupBlock inmem
                    , _cpGetBlockParent = doGetBlockParent inmem
                    , _cpRegisterProcessedTx = doRegisterSuccessful inmem
                    , _cpLookupProcessedTx = doLookupSuccessful inmem
                    , _cpGetBlockHistory = \_ _ -> error "unimplemented"
                    , _cpGetHistoricalLookup = \_ _ _ -> error "unimplemnted"
                }
            , _cpeLogger = logger
            })
  where
    noop = return ()

doRestore :: DbEnv PureDb -> MVar Store ->  Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore _ lock (Just (height, hash)) =
    modifyMVarMasked lock $ \store -> do
      let ph = pred height
      let bh = (ph, hash)
      let filt (_, !mp) = let !mp' = HMS.filterWithKey (\_ (v, _) -> v <= ph) mp
                              !out = (mempty, mp')
                          in newMVar out
      case HMS.lookup bh (_theStore store) of
            Just dbenv -> do
              mdbenv <- newMVar dbenv
              played <- readMVar (_playedTxs store) >>= filt

              let !store' = store { _lastBlock = Just bh
                                  , _dbenv = mdbenv
                                  , _playedTxs = played
                                  }
              return (store', PactDbEnv' (PactDbEnv pactdb mdbenv))
            Nothing -> internalError $
              "InMemoryCheckpointer: Restore not found: height=" <> pack (show (pred height))
              <> ", hash=" <> pack (show hash)
              <> ", known=" <> pack (show (HMS.keys (_theStore store)))
doRestore genesis lock Nothing =
    modifyMVarMasked lock $ \_ -> do
      gen <- newMVar genesis
      played <- newMVar mempty
      return (Store mempty Nothing gen played,
                 PactDbEnv' (PactDbEnv pactdb gen))

doSave :: MVar Store -> BlockHash -> IO ()
doSave lock hash = modifyMVar_ lock $ \(Store store mheight dbenv played) -> do
    env <- readMVar dbenv
    let bh = case mheight of
               Nothing -> (BlockHeight 0, hash)
               Just (height, _) -> (succ height, hash)
    modifyMVar_ played $ updatePlayed bh
    return $ Store (HMS.insert bh env store) (Just bh) dbenv played
  where
    updatePlayed bh (pset, mp) =
        let !mp' = foldl' (\m h -> HMS.insert h bh m) mp pset
            !out = (mempty, mp')
        in return $! out

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
    \(Store _ _ _ played) -> modifyMVar_ played $
    \(pset, mp) -> do
        let b = HMS.member hash mp || HashSet.member hash pset
        -- TODO: need a better error recovery story here, because we need to
        -- be able to blacklist the bad transaction(s)
        when b $ fail $ "duplicate transaction for hash " ++ show hash
        let !pset' = HashSet.insert hash pset
        let out = (pset', mp)
        return $! out

doLookupSuccessful :: MVar Store -> PactHash -> IO (Maybe (T2 BlockHeight BlockHash))
doLookupSuccessful lock hash =
    withMVar lock $
    \(Store _ _ _ played) -> do
        (_, mp) <- readMVar played
        return $! fmap toS $! HMS.lookup hash mp
  where
    toS (a,b) = T2 a b
