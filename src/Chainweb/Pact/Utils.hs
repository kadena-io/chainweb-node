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
    ( -- * persistence
      toEnv'
    , toEnvPersist'
      -- * combinators
    , aeson
    ) where

import Data.Aeson

import Control.Concurrent.MVar

import Pact.Interpreter as P

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

-- | This is the recursion principle of an 'Aeson' 'Result' of type 'a'.
-- Similar to 'either', 'maybe', or 'bool' combinators
--
aeson :: (String -> b) -> (a -> b) -> Result a -> b
aeson f _ (Error a) = f a
aeson _ g (Success a) = g a
