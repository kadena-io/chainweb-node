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

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Network.HTTP.Client as HTTP

import Prelude hiding (log)

import System.IO.Unsafe
import System.LogLevel
import qualified System.Random.MWC as RNG
import qualified System.Random.MWC.Distributions as RNG

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

import P2P.Node
import P2P.Node.Configuration
import P2P.Session

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
            mempoolP2pSession memP2pConfig chain (_mempoolP2pConfigPollInterval memP2pConfig)
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


mempoolP2pSession
    :: MempoolP2pConfig
    -> ChainResources logger
    -> Seconds
    -> P2pSession
mempoolP2pSession mcfg chain pollInterval logg0 env peerInfo = do
    -- We'll gossip always, but we won't always sync. Run the gossip and sync
    -- sessions concurrently, and perform sync based on a dice roll.
    Async.concurrently_ gossipSession syncSession
    return True
  where
    gossipSession = do
        -- We won't propagate every transaction we receive; instead, we'll
        -- select a maximum gossip level based on an exponential distribution
        -- and only pass on transactions that came to us having fewer than that
        -- many hops. This biases the network towards low-latency sharing of
        -- new transactions.
        maxHops <- getGossipLevel mcfg
        mempoolGossipP2pSession maxHops pool peerMempool logg0 env peerInfo

    syncSession = shouldWeRunSync mcfg >>= flip when syncSession_
    syncSession_ =
        void $ mempoolSyncP2pSession chain pollInterval pool peerMempool logg0 env peerInfo

    -- TODO: remote version detection goes here
    peerMempool = MPC.toMempool v cid txcfg env
    pool = _chainResMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    cid = _chainId chain
    v = _chainwebVersion chain


mempoolGossipP2pSession
    :: Mempool.HopCount           -- ^ only send txs with this many hops or fewer.
    -> Mempool.MempoolBackend t   -- ^ our pool
    -> Mempool.MempoolBackend t   -- ^ remote pool
    -> P2pSession
mempoolGossipP2pSession maxHops pool peerMempool logg0 env _peerInfo = do
    logg Debug "mempool gossip session starting"
    void (Mempool.mempoolGetHighwaterMark pool >>= go) `finally`
        logg Debug "mempool gossip session finished"
    return True
  where
    -- Loops forever, sending any new transactions sent to the mempool to the remote peer.
    go hw = do
        hashref <- newIORef []
        newhw <- Mempool.mempoolGetPendingTransactions pool (Just hw) Mempool.MempoolBlocking $
                 \chunk -> modifyIORef' hashref (chunk:)
        hashes <- V.concat <$> readIORef hashref
        results <- Mempool.mempoolLookup pool hashes
        let txs = V.map (fmap (+1)) $
                  V.filter (\(_, hops) -> hops <= maxHops) $
                  V.mapMaybe toResult results
        -- TODO: make sure we are using new gossip protocol
        Mempool.mempoolInsert peerMempool Mempool.CheckedInsert txs
        go newhw

    toResult Mempool.Missing = Nothing
    toResult (Mempool.Pending t Nothing) = Just (t, Mempool.mAXIMUM_HOP_COUNT)
    toResult (Mempool.Pending t (Just x)) = Just (t, x)

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool gossip@", remote, "]:", m]

mempoolSyncP2pSession
    :: Show t
    => ChainResources logger
    -> Seconds
    -> Mempool.MempoolBackend t   -- ^ our pool
    -> Mempool.MempoolBackend t   -- ^ remote pool
    -> P2pSession
mempoolSyncP2pSession chain (Seconds pollInterval) pool peerMempool logg0 env peerInfo = do
    logg Debug "mempool sync session starting"
    Mempool.syncMempools' logg syncHistory peerInfo syncIntervalUs pool peerMempool
    logg Debug "mempool sync session finished"
    return True
  where
    -- FIXME: Potentially dangerous down-cast.
    -- FIXME: multiplication by 500000 instead of 1000000
    syncIntervalUs :: Int
    syncIntervalUs = int pollInterval * 500000

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    syncHistory = _chainResMempoolSyncHistory chain


------------------------------------------------------------------------------
-- RNG helper functions

mempoolGlobalRNG :: MVar RNG.GenIO
mempoolGlobalRNG = unsafePerformIO (RNG.createSystemRandom >>= newMVar)
{-# NOINLINE mempoolGlobalRNG #-}

withMempoolRNG :: (RNG.GenIO -> IO a) -> IO a
withMempoolRNG = withMVar mempoolGlobalRNG

diceRoll :: Double              -- ^ threshold between [0,1]
         -> IO Bool
diceRoll threshold = do
    sample <- withMempoolRNG (RNG.uniformRM (0, 1))
    return $! sample <= threshold

shouldWeRunSync :: MempoolP2pConfig -> IO Bool
shouldWeRunSync cfg = diceRoll thresh   -- FIXME: make configurable
  where
    thresh = view mempoolP2pConfigSyncProbability cfg

getGossipLevel :: MempoolP2pConfig -> IO Mempool.HopCount
getGossipLevel cfg = do
    x <- withMempoolRNG (RNG.truncatedExp scale limits)
    return $! max 0 (min (Mempool.mAXIMUM_HOP_COUNT - 1) (floor x))
  where
    scale = view mempoolP2pConfigGossipScaleFactor cfg
    limits = (0, fromIntegral Mempool.mAXIMUM_HOP_COUNT)
