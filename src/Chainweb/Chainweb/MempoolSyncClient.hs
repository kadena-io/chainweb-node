{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Chainweb.MempoolSyncClient
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Mempool Synchronization Client
--
module Chainweb.Chainweb.MempoolSyncClient
( runMempoolSyncClient
) where

import Chainweb.Time

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP

import Prelude hiding (log)

import System.LogLevel

-- internal modules

import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Logger
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import qualified Chainweb.Mempool.RestAPI.Client as MPC
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration
import P2P.Session
import P2P.Node

import qualified Servant.Client as Sv

-- -------------------------------------------------------------------------- --
-- Mempool sync.

-- | Synchronize the local mempool over the P2P network.
--
runMempoolSyncClient
    :: Logger logger
    => HTTP.Manager
        -- ^ HTTP connection pool
    -> MempoolP2pConfig
    -> PeerResources logger
    -> ChainResources logger
        -- ^ chain resources
    -> IO ()
runMempoolSyncClient mgr memP2pConfig peerRes chain = bracket create destroy go
  where
    create = do
        logg Debug "starting mempool p2p sync"
        p2pCreateNode v netId peer (logFunction syncLogger) peerDb mgr True $
            mempoolSyncP2pSession chain (_mempoolP2pConfigPollInterval memP2pConfig)
    go n = do
        -- Run P2P client node
        logg Debug "mempool sync p2p node initialized, starting session"
        p2pStartNode p2pConfig n

    destroy n = p2pStopNode n `finally` logg Debug "mempool sync p2p node stopped"

    v = _chainwebVersion chain
    peer = _peerResPeer peerRes
    p2pConfig = _peerResConfig peerRes
        & set p2pConfigMaxSessionCount (_mempoolP2pConfigMaxSessionCount memP2pConfig)
        & set p2pConfigSessionTimeout (_mempoolP2pConfigSessionTimeout memP2pConfig)
    peerDb = _peerResDb peerRes
    netId = MempoolNetwork $ _chainId chain

    logg = logFunctionText syncLogger
    syncLogger = setComponent "mempool-sync" $ _chainResLogger chain

mempoolSyncP2pSession
    :: ChainResources logger
    -> Seconds
    -> P2pSession
mempoolSyncP2pSession chain (Seconds pollInterval) logg0 env _ = do
    logg Debug "mempool sync session starting"
    Mempool.syncMempools' logg syncIntervalUs pool peerMempool
    logg Debug "mempool sync session finished"
    return True
  where
    peerMempool = MPC.toMempool v cid txcfg env

    -- FIXME Potentially dangerous down-cast.
    syncIntervalUs :: Int
    syncIntervalUs = int pollInterval * 500000

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    pool = _chainResMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    cid = _chainId chain
    v = _chainwebVersion chain
