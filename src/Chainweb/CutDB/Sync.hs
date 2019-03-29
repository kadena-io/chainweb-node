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

import qualified Data.Text as T

import GHC.Generics

import Servant.Client

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Utils
import Chainweb.Version

import P2P.Peer
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

syncSession
    :: ChainwebVersion
    -> Bool
        -- ^ Whether to include the local peer as origin in outgoing cuts.
    -> PeerInfo
    -> CutDb cas
    -> P2pSession
syncSession v useOrigin p db logg env = do
    race_
        (S.mapM_ send $ S.map (cutToCutHashes origin) $ cutStream db)
        (forever $ receive >> threadDelay 2000000 {- 2 seconds -})
            -- Usually we rely on blocks being pushed to us, but every 3
            -- seconds we pull.

            -- FIXME make this configurable or dynamic
            -- FIXME use Etag along with if-non-match precondition.

    -- this code must not be reached
    logg @T.Text Error "unexpectedly exited cut sync session"
    return False
  where
    cenv = CutClientEnv v env

    send c = do
        putCut cenv c
        logg @T.Text Debug $ "put cut " <> sshow c

    receive = do
        c <- getCut cenv
        logg @T.Text Debug $ "got cut " <> sshow c
        addCutHashes db c

    origin = if useOrigin then Just p else Nothing

