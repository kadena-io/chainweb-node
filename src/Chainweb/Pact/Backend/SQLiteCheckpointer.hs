-- |
-- Module: Chainweb.Pact.DiskCheckpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact SQLite checkpoint module for Chainweb

module Chainweb.Pact.Backend.SQLiteCheckpointer where

import Chainweb.Pact.Types
import Chainweb.Pact.Backend.Types
import Data.IORef
import Chainweb.BlockHeader
import qualified Pact.Types.Runtime as P


type DiskStore = IO [FilePath]

initSQLiteCheckpointer :: Checkpointer'
initSQLiteCheckpointer = undefined
  -- Checkpointer'
    -- { Checkpointer
      -- { _cRestore = restore, _cPrepare = prepare, _cSave = save} }

initSQLiteStore :: IO (IORef DiskStore)
initSQLiteStore =  newIORef undefined

restore :: BlockHeight -> P.Hash -> CheckpointData -> IORef DiskStore -> IO ()
restore _height _hash _cdata _store = undefined

prepare ::
     BlockHeight
  -> P.Hash
  -> OpMode
  -> CheckpointData
  -> IORef DiskStore
  -> IO (Either String DiskStore)
prepare _ _ _ _ _ = undefined

save ::
     BlockHeight -> P.Hash -> OpMode -> CheckpointData -> IORef DiskStore -> IO ()
save _height _hash _opmode _cdata _store = undefined
