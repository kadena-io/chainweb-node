-- |
-- Module: Chainweb.Pact.Backend.SqliteDb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.SqliteDb where

import qualified Data.Map.Strict as M

import Pact.Interpreter
import Pact.Persist.SQLite ()
import qualified Pact.Persist.SQLite as P
import Pact.PersistPactDb
import Pact.Types.Server

-- internal modules

import Chainweb.Pact.Types

mkSQLiteState :: PactDbEnv (DbEnv P.SQLite) -> CommandConfig -> IO PactDbState
mkSQLiteState env cmdCfg = do
    initSchema env
    let theState =
            PactDbState
                { _pdbsCommandConfig = cmdCfg
                , _pdbsDbEnv = Env' env
                , _pdbsState = CommandState initRefStore M.empty
                }
    return theState
