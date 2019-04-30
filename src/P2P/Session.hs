{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: P2P.Session
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Session
(
-- * Log Function
  LogFunction
, LogFunctionText
, defaultLogFunction

-- * P2P Client Session
, P2pSession

-- * Reexports
, ClientEnv
) where

import Control.Monad

import qualified Data.Text.IO as T

import Servant.Client

import System.LogLevel

-- Internal modules

import Data.LogMessage
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Log Function

defaultLogFunction :: LogFunction
defaultLogFunction l = when (l >= Warn) . T.putStrLn . logText

-- -------------------------------------------------------------------------- --
-- P2P Client Session

type P2pSession = LogFunction -> ClientEnv -> PeerInfo -> IO Bool
