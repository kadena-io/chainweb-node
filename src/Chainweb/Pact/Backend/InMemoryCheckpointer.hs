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

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe

import Control.Concurrent.MVar
import Control.Monad (when)

import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

import Numeric.Natural

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

-- MIGHT INCLUDE THIS MODULE LATER
-- import Chainweb.ChainId
-- MIGHT INCLUDE THIS MODULE LATER

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> Maybe Natural -> IO CheckpointEnv
initInMemoryCheckpointEnv cmdConfig logger gasEnv mbound = do
    inmem <- newMVar mempty
    let bound = fromMaybe 10 mbound
    return $
        CheckpointEnv
            { _cpeCheckpointer =
                  Checkpointer
                      { restore = restore' inmem
                      , restoreInitial = restoreInitial' inmem
                      , save = save' inmem bound
                      , saveInitial = saveInitial' inmem bound
                      , discard = discard' inmem
                      }
            , _cpeCommandConfig = cmdConfig
            , _cpeLogger = logger
            , _cpeGasEnv = gasEnv
            }

type Store = HashMap (BlockHeight, BlockHash) PactDbStateRecency

data PactDbStateRecency
  = NoRecency PactDbState       -- "no" as in disregard
  | Recency PactDbState
            {-# UNPACK #-}!Natural

restore' :: MVar Store -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
restore' lock height hash = do
    withMVarMasked lock $ \store -> do
        case HMS.lookup (height, hash) store of
            Just dbstate' -> case dbstate' of
                NoRecency dbstate  -> return $ Right dbstate
                Recency dbstate _ -> return $ Right dbstate
            Nothing -> return $ Left "InMemoryCheckpointer.restore':Restore not found exception"

restoreInitial' :: MVar Store -> IO (Either String PactDbState)
restoreInitial' lock = do
    let bh = nullBlockHash
    restore' lock (BlockHeight 0) bh

saveInitial' :: MVar Store -> Natural -> PactDbState -> IO (Either String ())
saveInitial' lock bound p@PactDbState {..} = do
    let bh = nullBlockHash
    save' lock bound (BlockHeight 0) bh p

save' :: MVar Store -> Natural -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
save' lock bound height hash p@PactDbState {..} = do

     -- Saving off checkpoint.

     let dataToSave = if bound == 0
            then NoRecency p
            else Recency p 0

     let upRecency a@(NoRecency _) = a
         upRecency (Recency a r) = Recency a (r+1)

     modifyMVar_ lock (return . HMS.insert (height, hash) dataToSave . HMS.map upRecency)

     -- prune the store
     pruneStore bound lock

     -- Closing database connection.
     case _pdbsDbEnv of
       EnvPersist' PactDbEnvPersist {..} ->
         case _pdepEnv of
           P.DbEnv {..} -> closeDb _db

discard' :: MVar Store -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
discard' _ _ _ _ = return (Right ())

pruneStore :: Natural -> MVar Store -> IO ()
pruneStore bound lock = when (bound > 0) (modifyMVar_ lock (return . HMS.mapMaybe go))
  where
    go a@(NoRecency _)  = Just a
     -- ^ this case should never be reached, but GHC doesn't realize that.
    go p@(Recency _ r) = if r < bound
        then Just p
        else Nothing
