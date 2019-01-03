-- |
-- Module: Chainweb.Pact.MapPureCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact PureDb checkpoint module for Chainweb

module Chainweb.Pact.Backend.InMemoryCheckpointer
       (initInMemoryCheckpointer
       , initInMemoryStore)
        where

import Chainweb.BlockHeader
import Chainweb.Pact.Types
import Chainweb.Pact.Backend.Types
import qualified Pact.Types.Runtime as P

import qualified Data.HashMap.Strict as HMS      -- as per Greg's suggestion
import Data.HashMap.Strict (HashMap)

initInMemoryCheckpointer :: Checkpointer (HashMap (BlockHeight, P.Hash) CheckpointData)
initInMemoryCheckpointer =
  Checkpointer
    { _cRestore = restore
    , _cPrepare = prepare
    , _cSave = save
    }


initInMemoryStore :: HashMap (BlockHeight, P.Hash) CheckpointData
initInMemoryStore = HMS.empty

restore ::
     BlockHeight
  -> P.Hash
  -> CheckpointData
  -> HashMap (BlockHeight, P.Hash) CheckpointData
  -> IO ()
restore height hash cdata store = undefined

prepare ::
     BlockHeight
  -> P.Hash
  -> OpMode
  -> CheckpointData
  -> HashMap (BlockHeight, P.Hash) CheckpointData
  -> IO (Either String c)
prepare height hash opmode cdata store = undefined

save :: BlockHeight -> P.Hash -> OpMode -> CheckpointData -> c -> IO ()
save height hash opmode cdata store = undefined
