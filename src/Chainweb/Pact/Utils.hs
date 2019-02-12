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
    , closePactDb
    ) where

import Control.Concurrent.MVar

import Pact.Interpreter as P
import Pact.PersistPactDb as P

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

-- This is a band-aid solution; We're just going to close the
-- database connection here to be safe.
closePactDb :: PactDbState -> IO ()
closePactDb =  go . _pdbsDbEnv
  where
    go (EnvPersist' (PactDbEnvPersist _ dbEnv)) =
      (either fail return) =<< closeDb (P._db dbEnv)
