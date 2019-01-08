module Chainweb.Pact.Backend.SqliteDb where

import Chainweb.Pact.Types

import Pact.Types.Server
import Pact.Interpreter
import Pact.Persist.SQLite ()
import Pact.PersistPactDb
import qualified Pact.Persist.SQLite as P

import qualified Data.Map.Strict as M

mkSQLiteState :: PactDbEnv (DbEnv P.SQLite) -> CommandConfig -> IO PactDbState
mkSQLiteState env cmdCfg = do
  initSchema env
  let theState = PactDbState
        { _pdbsCommandConfig = cmdCfg
        , _pdbsDbEnv = Env' env
        , _pdbsState = CommandState initRefStore M.empty
        }
  return theState
