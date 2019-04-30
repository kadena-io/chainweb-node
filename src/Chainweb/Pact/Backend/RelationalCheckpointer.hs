-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emmanuel Denloye <emmanuel@kadena.io>
-- Stability: experimental
--
-- Pact Checkpointer for Chainweb
module Chainweb.Pact.Backend.RelationalCheckpointer where

import Control.Concurrent.MVar


-- pact
import Pact.Types.Server (CommandConfig(..))
import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))
import Pact.PersistPactDb (DbEnv(..))
import Pact.Persist.SQLite (SQLite(..))

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types

initRelationalCheckpointer ::
     DbConnection -> CommandConfig -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer dbconn cmdConfig logger gasEnv = do
  let checkpointer =
        Checkpointer
          { restore = innerRestore dbconn
          , restoreInitial = innerRestoreInitial dbconn
          , save = innerSave dbconn
          , saveInitial = innerSaveInitial dbconn
          , discard = innerDiscard dbconn
          }
  return $
    CheckpointEnv
      { _cpeCheckpointer = checkpointer
      , _cpeCommandConfig = cmdConfig
      , _cpeLogger = logger
      , _cpeGasEnv = gasEnv
      }

type DbConnection = MVar (DbEnv SQLite)

innerRestore ::
     DbConnection -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
innerRestore _dbconn _bh _hash = return (Right undefined)

innerRestoreInitial :: DbConnection -> IO (Either String PactDbState)
innerRestoreInitial _dbconn = return (Right undefined)

innerSave ::
     DbConnection
  -> BlockHeight
  -> BlockHash
  -> PactDbState
  -> IO (Either String ())
innerSave _dbconn _bh _hash _state = return (Right ())

innerSaveInitial :: DbConnection -> PactDbState -> IO (Either String ())
innerSaveInitial _dbconn _state = return (Right ())

innerDiscard :: DbConnection -> PactDbState -> IO (Either String ())
innerDiscard _dbconn _state = undefined
