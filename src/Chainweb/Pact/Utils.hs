{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Chainweb.Pact.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Pact service for Chainweb

module Chainweb.Pact.Utils
    ( toEnv'
    , toEnvPersist'
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

<<<<<<< HEAD
import System.Directory
=======
import Pact.Interpreter as P
>>>>>>> master

import Pact.Interpreter
import Pact.PersistPactDb
import Pact.Persist.SQLite (SQLite(..), closeSQLite, initSQLite, persister)
import Pact.Types.SQLite
import Pact.Types.Logger

import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Types

toEnv' :: EnvPersist' -> IO Env'
toEnv' (EnvPersist' ep') = do
    let thePactDb = _pdepPactDb $! ep'
    let theDbEnv = _pdepEnv $! ep'
    env <- mkPactDbEnv thePactDb theDbEnv
    return $! Env' env

toEnvPersist' :: Env' -> IO EnvPersist'
toEnvPersist' (Env' pactDbEnv) = do
    let mVar = pdPactDbVar $! pactDbEnv -- :: MVar (P.DbEnv a)
    !dbEnv <- readMVar $! mVar           -- :: P.DbEnv a
    let pDbEnvPersist = PactDbEnvPersist
          { _pdepPactDb = pdPactDb pactDbEnv -- :: P.PactDb (P.DbEnv a)
          , _pdepEnv = dbEnv
          }
    return $! EnvPersist' pDbEnvPersist
