{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.CutDB.Sync
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB.Sync
( syncSession
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.STM

import qualified Data.Text as T

import GHC.Generics

import Servant.Client

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Utils
import Chainweb.Version

import P2P.Session

-- -------------------------------------------------------------------------- --
-- Client Env

data CutClientEnv = CutClientEnv
    { _envChainwebVersion :: !ChainwebVersion
    , _envClientEnv :: !ClientEnv
    }
    deriving (Generic)

runClientThrowM :: ClientM a -> ClientEnv -> IO a
runClientThrowM req = fromEitherM <=< runClientM req

putCut
    :: CutClientEnv
    -> CutHashes
    -> IO ()
putCut (CutClientEnv v env) = void . flip runClientThrowM env . cutPutClient v

getCut
    :: CutClientEnv
    -> IO CutHashes
getCut (CutClientEnv v env) = runClientThrowM (cutGetClient v) env

-- -------------------------------------------------------------------------- --
-- Sync Session

syncSession :: ChainwebVersion -> CutDb -> P2pSession
syncSession v db logg env = do
    race_
        (void $ S.mapM_ send $ S.map (cutToCutHashes Nothing) $ cutStream db)
        (forever $ receive db >> threadDelay 1000000)
            -- FIXME make this configurable or dynamic

    -- this code must not be reached
    void $ logg @T.Text Error "unexpectedly exited cut sync session"
    return False
  where
    cenv = CutClientEnv v env

    send c = do
        putCut cenv c
        logg @T.Text Debug $ "put cut " <> sshow c

    receive db = do
        c <- getCut cenv
        logg @T.Text Debug $ "got cut " <> sshow c
        atomically $ addCutHashes db c
