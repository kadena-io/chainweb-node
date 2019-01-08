-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact PureDb checkpoint module for Chainweb
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.Pact.Backend.InMemoryCheckpointer where

import qualified Chainweb.BlockHeader as C
import Chainweb.Pact.Backend.Types
import Data.Foldable
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

import Control.Lens
import Control.Monad.State
import qualified Data.HashMap.Strict as HMS -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)
import Data.IORef
import qualified Data.Map.Strict as M

initInMemoryCheckpointEnv :: P.CommandConfig -> P.Logger -> P.GasEnv -> IO CheckpointEnv'
initInMemoryCheckpointEnv cmdConfig logger gasEnv = do
    theStore <- newIORef HMS.empty
    theIndex <- newIORef M.empty
    return $
        CheckpointEnv'
            CheckpointEnv
                { _cpeCheckpointer =
                      Checkpointer {_cRestore = restore, _cPrepare = prepare, _cSave = save}
                , _cpeCommandConfig = cmdConfig
                , _cpeCheckpointStore = theStore
                , _cpeCheckpointStoreIndex = theIndex
                , _cpeLogger = logger
                , _cpeGasEnv = gasEnv
                }

type CIndex = M.Map (C.BlockHeight, P.Hash) Store

type Store = HashMap (C.BlockHeight, P.Hash) CheckpointData

type CheckM a = StateT (Store, CIndex) IO a

restore :: C.BlockHeight -> P.Hash -> CheckM ()
restore height hash = do
    cindex <- snd <$> get
    case M.lookup (height, hash) cindex of
        Just snapshot -> _1 .= snapshot
       -- This is just a placeholder for right now (the Nothing clause)
        Nothing -> fail "There is no snapshot that can be restored."

prepare :: C.BlockHeight -> P.Hash -> OpMode -> CheckM (Either String CheckpointData)
prepare height hash =
    \case
        Validation -> do
            curStore <- fst <$> get
            return $
                maybe
                    (Left "InMemoryCheckpointer.prepare: No current store")
                    Right
                    (HMS.lookup (height, hash) curStore)
        NewBlock -> do
            cindex <- snd <$> get
            case M.lookup (height, hash) cindex of
                Just snapshot -> do
                    _1 .= snapshot
                    return $ Left "We only prepare an environment for new blocks"
                Nothing -> return $ Left "Cannot prepare"

save :: C.BlockHeight -> P.Hash -> CheckpointData -> OpMode -> CheckM ()
save height hash cdata =
    \case
        Validation -> modifying _1 (HMS.insert (height, hash) cdata)
        NewBlock -> return ()
