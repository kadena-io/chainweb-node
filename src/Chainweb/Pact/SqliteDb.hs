{-# LANGUAGE RecordWildCards #-}

module Chainweb.Pact.SqliteDb where

import Chainweb.Pact.Types

import qualified Pact.Gas as P
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger
import Pact.Interpreter
import Pact.Persist.SQLite ()
import Pact.PersistPactDb
import qualified Pact.Persist.SQLite as P

import qualified Data.Map.Strict as M
import Data.Maybe

mkSQLiteState :: PactDbEnv (DbEnv P.SQLite) -> CommandConfig -> Logger -> IO PactDbState
mkSQLiteState env cfg@CommandConfig {..} logger = do
  initSchema env
  let gasLimit = fromMaybe 0 _ccGasLimit
  let gasRate = fromMaybe 0 _ccGasRate
  let gasEnv = GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
  let theState = PactDbState
        { _pdbsCommandConfig = cfg
        , _pdbsDbEnv = Env' env
        , _pdbsState = CommandState initRefStore M.empty
        , _pdbsLogger = logger
        , _pdbsGasEnv = gasEnv
        }
  return theState
