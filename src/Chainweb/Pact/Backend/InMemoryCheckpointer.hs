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

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
-- import Data.Map (toList)
-- import Data.Text (pack)

import Control.Concurrent.MVar

-- import Control.Exception

import Pact.Interpreter
import Pact.PersistPactDb
-- import Pact.Persist
import Pact.Persist.Pure
import Pact.Types.Logger
import Pact.Types.Runtime hiding (hash)

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.Types (internalError)

-- checkpointer ::
--      MVar Store
--   -> Maybe (BlockHeight, ParentHash)
--   -> (PactDbEnv' -> IO CheckpointerResult)
--   -> (CheckpointerResult -> CheckpointAction)
--   -> IO CheckpointerResult
-- checkpointer lock mHeightHash action saveDiscardPredicate = do
--   toSave <- newEmptyMVar
--   result <- bracket (restore' toSave) (const $ return ()) action
--   case saveDiscardPredicate result of
--     Discard -> return ()
--     (Save hash) -> do
--       p <- readMVar toSave
--       case mHeightHash of
--         -- We are saving the "results" of genesis.
--         Nothing -> modifyMVar_ lock (return . HMS.insert (Just (0, hash)) p)
--         Just (height, _) -> modifyMVar_ lock (return . HMS.insert (Just (height, hash)) p)
--   return result
--   where
--     restore' toput = withMVarMasked  lock $ \store -> do
--       case HMS.lookup mHeightHash store of
--         Just pactdbenv -> do
--           putMVar toput pactdbenv
--           return $ PactDbEnv' pactdbenv
--         -- TODO
--         Nothing -> internalError $ case mHeightHash of
--           Just (bh, parentHash) ->
--               "InMemoryCheckpointer: Restore not found: height=" <> pack (show bh)
--               <> ", hash=" <> pack (show parentHash)
--               <> ", known=" <> pack (show (HMS.keys store))
--           Nothing -> "Something went wrong at genesis. There is nothing here!"



data Store = Store
  { _theStore :: HashMap (BlockHeight, BlockHash) (DbEnv PureDb)
  , _tempSaveHeight :: Maybe BlockHeight
  , _dbenv :: MVar (DbEnv PureDb)
  }

initInMemoryCheckpointEnv :: Loggers -> Logger -> GasEnv -> IO (PactDbEnv', CheckpointEnv)
initInMemoryCheckpointEnv loggers logger gasEnv = do
    pdenv@(PactDbEnv _ env) <- mkPureEnv loggers
    initSchema pdenv
    inmem <- newMVar (Store mempty Nothing env)
    return $
        (PactDbEnv' pdenv,
          CheckpointEnv
            { _cpeCheckpointer =
                Checkpointer
                    (doRestore inmem)
                    -- (doRestoreInitial inmem)
                    (doSave inmem)
                    (doDiscard inmem)
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            })

doRestore :: MVar Store ->  Maybe (BlockHeight, ParentHash) -> IO PactDbEnv'
doRestore lock (Just (height, hash)) = do
    pactdbenv <- withMVarMasked lock $ \store -> do
        case HMS.lookup (pred height, hash) (_theStore store) of
            Just dbenv -> do
              mdbenv <- newMVar dbenv
              return $ PactDbEnv' (PactDbEnv pactdb mdbenv)
            Nothing -> internalError $
              "InMemoryCheckpointer: Restore not found: height=" <> pack (show (pred height))
              <> ", hash=" <> pack (show hash)
              <> ", known=" <> pack (show (HMS.keys (_theStore store)))

    modifyMVar_ lock (\s ->  pure $ s { _tempSaveHeight = Just height })
    return pactdbenv
doRestore lock Nothing = withMVarMasked lock $ \store -> return $ PactDbEnv' (PactDbEnv pactdb (_dbenv store))

-- doRestoreInitial :: MVar Store -> IO PactDbEnv'
-- doRestoreInitial lock = withMVarMasked lock $ \store ->
--   return $ PactDbEnv' (PactDbEnv pactdb (_currentPactDbEnv store))

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
