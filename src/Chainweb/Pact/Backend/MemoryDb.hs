{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Pact.Backend.MemoryDb where

import Chainweb.Pact.Types

import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.PersistPactDb as P
import Pact.Types.Server as P

import qualified Data.Map.Strict as M

mkPureState :: P.PactDbEnv (P.DbEnv P.PureDb) -> P.CommandConfig -> IO PactDbState
mkPureState env cmdCfg = do
    P.initSchema env
    return $
        PactDbState
            { _pdbsCommandConfig = cmdCfg
            , _pdbsDbEnv = Env' env
            , _pdbsState = P.CommandState P.initRefStore M.empty
            }
