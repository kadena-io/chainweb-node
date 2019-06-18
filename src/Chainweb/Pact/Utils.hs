{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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
    , printDebug
    , showableDebug
    , debug
    , showable
    ) where

import Data.Aeson
import Data.Foldable (traverse_)

import Control.Concurrent.MVar
import Control.Lens (Getter, to, view)

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

-- ------------------------------------------------------------------------ --
-- debugging


-- | existential form for printable values
--
data Showable = forall a . Show a => Showable a

-- | smuggle values of type 'Show a => a'
--
debug :: forall a. Show a => a -> Showable
debug = Showable

-- | get the string value of a showable
--
showable :: Getter Showable String
showable = to (\(Showable a) -> show a)

-- | Apply some unital debug function on some traversable of showables
--
showableDebug :: (Traversable f, Applicative g) => (Showable -> g ()) -> f Showable -> g ()
showableDebug = traverse_

-- | Given a list of Showable values, print them
--
printDebug :: [Showable] -> IO ()
printDebug = showableDebug ((<*) (putStrLn "------------------------") . print . view showable)
