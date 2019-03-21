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

import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe

import Control.Concurrent.MVar
import Control.Monad (when)

import qualified Pact.PersistPactDb as P (DbEnv(..))
import qualified Pact.Types.Logger as P (Logger(..))
import qualified Pact.Types.Runtime as P (GasEnv(..))
import qualified Pact.Types.Server as P (CommandConfig(..))

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> Maybe Int -> IO CheckpointEnv
initInMemoryCheckpointEnv cmdConfig logger gasEnv mbound = do
    inmem <- newMVar (mempty, 0)
    let bound = fromMaybe 10 mbound
    return $
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' inmem bound
                      , restoreInitial = restoreInitial' inmem bound
                      , save = save' inmem bound
                      , saveInitial = saveInitial' inmem bound
                      , discard = discard' inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = (HashMap (BlockHeight, BlockHash) PactDbState, Int)

restore' :: MVar Store -> Int -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
restore' lock bound bh@(BlockHeight height) hash =
    withMVarMasked lock $ \(store, maxheight) ->
      return $
        case HMS.lookup (bh, hash) store of
          Just dbstate -> if bound == 0 || fromIntegral height > maxheight - bound && fromIntegral height < maxheight
                  then Right dbstate
                  else Left $ "InMemoryCheckpointer.restore': This blockeight (" ++ show bh ++ ") is no longer in the checkpointer"
          Nothing -> Left "InMemoryCheckpointer.restore':Restore not found exception"

restoreInitial' :: MVar Store -> Int -> IO (Either String PactDbState)
restoreInitial' lock bound = do
    let bh = nullBlockHash
    restore' lock bound (BlockHeight 0) bh

saveInitial' :: MVar Store -> Int -> PactDbState -> IO (Either String ())
saveInitial' lock bound p@PactDbState {..} = do
    let bh = nullBlockHash
    save' lock bound (BlockHeight 0) bh p

save' :: MVar Store -> Int -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
save' lock bound height hash p@PactDbState {..} = do

     -- Saving off checkpoint.
     modifyMVar_ lock (return . bimap (HMS.insert (height, hash) p) succ)

     -- Prune the store.
     pruneStore bound lock

     -- "Closing" database connection.
     case _pdbsDbEnv of
       EnvPersist' PactDbEnvPersist {..} ->
         case _pdepEnv of
           P.DbEnv {..} -> closeDb _db

discard' :: MVar Store -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
discard' _ _ _ _ = return (Right ())

pruneStore :: Int -> MVar Store -> IO ()
pruneStore bound lock = when (bound > 0) $
    modifyMVar_ lock $ \(hmap, maxheight) ->
                               return (HMS.filterWithKey (go maxheight) hmap, maxheight)
  where
    go mh (BlockHeight height, _) _ =
        mh <= bound || ((fromIntegral height) > mh - bound && (fromIntegral height) < mh)
