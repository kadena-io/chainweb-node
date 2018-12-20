{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Pact.SqliteDb where

import Chainweb.Pact.Types

import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server
import Pact.Types.Logger
import Pact.Gas
import Pact.Interpreter
import qualified Pact.Persist as P
import Pact.Persist.SQLite ()
import Pact.PersistPactDb
import qualified Pact.Persist.SQLite as PSL
import qualified Pact.Persist.Pure as Pure

import qualified Data.Map.Strict as M
import Data.Maybe

initService :: CommandConfig -> Loggers -> IO PactDbState
initService cfg@CommandConfig {..} loggers = do
  let logger = newLogger loggers "PactService"
  let klog = logLog logger "INIT"
  let gasLimit = fromMaybe 0 _ccGasLimit
  let gasRate = fromMaybe 0 _ccGasRate
  let gasEnv = GasEnv (fromIntegral gasLimit) 0.0 (constGasModel (fromIntegral gasRate))
  klog "Initializing pact SQLLite"
  env <- mkSQLiteEnv logger True (fromJust _ccSqlite) loggers
  mkState env cfg logger gasEnv

mkState :: PactDbEnv (DbEnv PSL.SQLite) -> CommandConfig -> Logger -> GasEnv
              -> IO PactDbState
mkState env cfg logger gasEnv = do
  initSchema env
  return PactDbState
    { _pdbsCommandConfig = cfg
    , _pdbsDbEnv = env
    , _pdbsState = CommandState initRefStore M.empty
    , _pdbsLogger = logger
    , _pdbsGasEnv = gasEnv
    }
