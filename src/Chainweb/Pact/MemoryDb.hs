{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Pact.MemoryDb where

import Chainweb.Pact.Types

import qualified Pact.Gas as P
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.Server as P
import qualified Pact.Types.Logger as P
import qualified Pact.Persist.Pure as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Interpreter as P

import qualified Data.Map.Strict as M
import Data.Maybe

mkPureState :: P.PactDbEnv (P.DbEnv P.PureDb) -> P.CommandConfig -> P.Logger -> IO PactDbState'
mkPureState env cfg@CommandConfig {..} logger = do
  P.initSchema env
  let gasLimit = fromMaybe 0 _ccGasLimit
  let gasRate = fromMaybe 0 _ccGasRate
  let gasEnv = GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
  let theState = PactDbState
        { _pdbsCommandConfig = cfg
        , _pdbsDbEnv = env
        , _pdbsState = P.CommandState P.initRefStore M.empty
        , _pdbsLogger = logger
        , _pdbsGasEnv = gasEnv
        }
  return $ PactDbState' theState
