{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Pact.Backend.MemoryDb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
module Chainweb.Pact.Backend.MemoryDb where

import qualified Data.Map.Strict as M

import qualified Pact.Interpreter as P
import qualified Pact.Persist.Pure as P
import qualified Pact.PersistPactDb as P
import qualified Pact.Types.Command as P
import Pact.Types.Server as P

-- internal modules
import Chainweb.Pact.Types
import Chainweb.Pact.Utils

mkPureState :: P.PactDbEnv (P.DbEnv P.PureDb) -> P.CommandConfig -> IO PactDbState
mkPureState env _cmdCfg = do
    P.initSchema env
    envPersist' <- toEnvPersist' (Env' env)
    return $
        PactDbState
            { _pdbsDbEnv = envPersist'
            , _pdbsState = P.CommandState P.initRefStore M.empty
            , _pdbsExecMode = P.Transactional 1
            }
