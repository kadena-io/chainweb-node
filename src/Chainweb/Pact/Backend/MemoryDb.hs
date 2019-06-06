{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.Backend.MemoryDb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.MemoryDb where

import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.PersistPactDb as P

-- internal modules
import Chainweb.Pact.Types
import Chainweb.Pact.Utils

mkPureState :: P.PactDbEnv (P.DbEnv P.PureDb) -> IO PactDbState
mkPureState env = do
    P.initSchema env
    envPersist' <- toEnvPersist' (Env' env)
    return $
        PactDbState
            { _pdbsDbEnv = envPersist' }

-- mkPurePactDbEnv' :: P.PactDbEnv (P.DbEnv P.PureDb) -> IO PactDbEnv'
-- mkPurePactDbEnv' = pure . PactDbEnv'
