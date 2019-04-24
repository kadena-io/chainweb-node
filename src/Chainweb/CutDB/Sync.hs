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
import Control.Lens (set)
import Control.Monad

import qualified Data.Text as T

import GHC.Generics

import Servant.Client

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal modules

import Chainweb.BlockHeader (BlockHeight)
import Chainweb.Cut (_cutHeight)
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
    -> BlockHeight
    -> IO CutHashes
getCut (CutClientEnv v env) h = runClientThrowM (cutGetClientLimit v (int h)) env

-- -------------------------------------------------------------------------- --
-- Sync Session

catchupStepSize :: BlockHeight
catchupStepSize = 1000

syncSession
    :: ChainwebVersion
    -> Bool
        -- ^ Whether to include the local peer as origin in outgoing cuts.
    -> PeerInfo
    -> CutDb cas
    -> P2pSession
syncSession v useOrigin p db logg env pinf = do
    race_
        (S.mapM_ send $ S.map (cutToCutHashes origin) $ cutStream db)
        (forever $ receive >> threadDelay 2000000 {- 2 seconds -})
            -- Usually we rely on blocks being pushed to us, but every 2
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
        -- Query cut that is at most 1000 blocks ahead
        h <- _cutHeight <$> _cut db
        c <- getCut cenv (h + min catchupStepSize (int farAheadThreshold - 1))
            -- Cf. documentation of 'fastAheadThreshold' for why this bound is
            -- needed

        logg @T.Text Info $ "received cut " <> sshow c
        addCutHashes db $ set cutOrigin (Just pinf) c

    origin = if useOrigin then Just p else Nothing

