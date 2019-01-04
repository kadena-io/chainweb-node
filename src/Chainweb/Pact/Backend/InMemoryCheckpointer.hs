-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
-- Pact PureDb checkpoint module for Chainweb
module Chainweb.Pact.Backend.InMemoryCheckpointer
  ( initInMemoryCheckpointer
  , initInMemoryStore
  ) where

import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types
import qualified Pact.Types.Runtime as P

import qualified Data.HashMap.Strict as HMS -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe

initInMemoryCheckpointer :: Checkpointer'
initInMemoryCheckpointer = Checkpointer'
  Checkpointer {_cRestore = restore, _cPrepare = prepare, _cSave = save}

initInMemoryStore :: IO (IORef Store)
initInMemoryStore = newIORef HMS.empty

restore :: BlockHeight -> P.Hash -> CheckpointData -> IORef Store -> IO ()
restore height hash cdata store = do
  s <- readIORef store
  maybe (return ()) (validate cdata) (HMS.lookup (height, hash) s)
  where
    validate = undefined

prepare ::
     BlockHeight
  -> P.Hash
  -> OpMode
  -> CheckpointData
  -> IORef Store
  -> IO (Either String Store)
prepare _ _ _ _ = fmap Right . readIORef

save ::
     BlockHeight -> P.Hash -> OpMode -> CheckpointData -> IORef Store -> IO ()
save height hash opmode cdata store = undefined
