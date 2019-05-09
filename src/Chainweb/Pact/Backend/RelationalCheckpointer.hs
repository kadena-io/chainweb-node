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

import Database.SQLite3.Direct as SQ3

import Pact.Types.Gas (GasEnv(..))
import Pact.Types.Logger (Logger(..))
-- pact
-- import Pact.PersistPactDb (DbEnv(..))
-- import Pact.Persist.SQLite (SQLite(..))

-- chainweb
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.Types

initRelationalCheckpointer ::
     Db -> Logger -> GasEnv -> IO CheckpointEnv
initRelationalCheckpointer dbconn loggr gasEnv = do
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
      , _cpeLogger = loggr
      , _cpeGasEnv = gasEnv
      }

type Db = MVar SQLiteEnv

innerRestore :: Db -> BlockHeight -> BlockHash -> IO (Either String PactDbState)
innerRestore _dbconn _bh _hash = return (Right undefined)

innerRestoreInitial :: Db -> IO (Either String PactDbState)
innerRestoreInitial _dbconn = return (Right undefined)

innerSave ::
     Db -> BlockHeight -> BlockHash -> PactDbState -> IO (Either String ())
innerSave _dbconn _bh _hash _state = return (Right ())

innerSaveInitial :: Db -> PactDbState -> IO (Either String ())
innerSaveInitial _dbconn _state = return (Right ())

innerDiscard :: Db -> PactDbState -> IO (Either String ())
innerDiscard _dbconn _state = undefined
