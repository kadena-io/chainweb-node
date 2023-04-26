{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: P2P.Session
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The type of a P2P network session.
module P2P.Session
  ( -- * Log Function
    LogFunction,
    LogFunctionText,
    defaultLogFunction,

    -- * P2P Client Session
    P2pSession,

    -- * Reexports
    ClientEnv,
  )
where

import Control.Monad
-- Internal modules

import Data.LogMessage
import qualified Data.Text.IO as T
import P2P.Peer
import Servant.Client
import System.LogLevel

-- -------------------------------------------------------------------------- --
-- Log Function

defaultLogFunction :: LogFunction
defaultLogFunction l = when (l >= Warn) . T.putStrLn . logText

-- -------------------------------------------------------------------------- --
-- P2P Client Session

type P2pSession = LogFunction -> ClientEnv -> PeerInfo -> IO Bool
