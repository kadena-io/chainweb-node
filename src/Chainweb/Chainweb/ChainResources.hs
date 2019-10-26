{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb.ChainResources
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Allocate chainweb resources for individual chains
--
module Chainweb.Chainweb.ChainResources
( ChainResources(..)
, chainResBlockHeaderDb
, chainResPeer
, chainResMempool
, chainResLogger
, chainResPact
, withChainResources

-- * Mempool Sync
, runMempoolSyncClient
) where

import Chainweb.Time

import Control.Concurrent.MVar
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP

import Prelude hiding (log)

import System.LogLevel

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.PeerResources
import Chainweb.CutDB (CutDb)
import Chainweb.Graph
import Chainweb.Logger
import qualified Chainweb.Mempool.Consensus as MPCon
import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import qualified Chainweb.Mempool.RestAPI.Client as MPC
import Chainweb.NodeId
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB

import P2P.Node
import P2P.Node.Configuration
import P2P.Session

import qualified Servant.Client as Sv

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data ChainResources logger = ChainResources
    { _chainResPeer :: !(PeerResources logger)
    , _chainResBlockHeaderDb :: !BlockHeaderDb
    , _chainResLogger :: !logger
    , _chainResMempool :: !(MempoolBackend ChainwebTransaction)
    , _chainResPact :: PactExecutionService
    }

makeLenses ''ChainResources

instance HasChainwebVersion (ChainResources logger) where
    _chainwebVersion = _chainwebVersion . _chainResBlockHeaderDb
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph (ChainResources logger) where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainId (ChainResources logger) where
    _chainId = _chainId . _chainResBlockHeaderDb
    {-# INLINE _chainId #-}

-- | Intializes all local Chain resources, but doesn't start any networking.
--
withChainResources
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> RocksDb
    -> PeerResources logger
    -> logger
    -> (MVar PactExecutionService -> Mempool.InMemConfig ChainwebTransaction)
    -> MVar (CutDb cas)
    -> PayloadDb cas
    -> Bool
        -- ^ whether to prune the chain database
    -> Maybe FilePath
        -- ^ database directory for checkpointer
    -> Maybe NodeId
    -> Bool
        -- ^ reset database directory
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResources v cid rdb peer logger mempoolCfg0 cdbv payloadDb prune dbDir nodeid resetDb inner =
    withBlockHeaderDb rdb v cid $ \cdb -> do
      pexMv <- newEmptyMVar
      let mempoolCfg = mempoolCfg0 pexMv
      Mempool.withInMemoryMempool_ (setComponent "mempool" logger) mempoolCfg v $ \mempool -> do
        mpc <- MPCon.mkMempoolConsensus mempool cdb $ Just payloadDb
        withPactService v cid (setComponent "pact" logger) mpc cdbv cdb
                        payloadDb dbDir nodeid resetDb $ \requestQ -> do
            -- prune block header db
            when prune $ do
                logg Info "start pruning block header database"
                x <- pruneForks logger cdb (diam * 3) $ \_h _payloadInUse ->

                    -- FIXME At the time of writing his payload hashes are not
                    -- unique. The pruning algorithm can handle non-uniquness
                    -- between within a chain between forks, but not accross
                    -- chains. Also cas-deletion is sound for payload hashes if
                    -- outputs are unique for payload hashes.
                    --
                    -- Renable this code once pact
                    --
                    -- includes the parent hash into the coinbase hash,
                    -- includes the transaction hash into the respective output hash, and
                    -- guarantees that transaction hashes are unique.
                    --
                    -- unless payloadInUse
                    --     $ casDelete payloadDb (_blockPayloadHash h)
                    return ()
                logg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."
            let pex = pes requestQ
            putMVar pexMv pex

            -- run inner
            inner $ ChainResources
                { _chainResPeer = peer
                , _chainResBlockHeaderDb = cdb
                , _chainResLogger = logger
                , _chainResMempool = mempool
                , _chainResPact = pex
                }
  where
    logg = logFunctionText (setComponent "pact-tx-replay" logger)
    diam = diameter (_chainGraph v)
    pes requestQ = case v of
        Test{} -> emptyPactExecutionService
        TimedConsensus{} -> emptyPactExecutionService
        PowConsensus{} -> emptyPactExecutionService
        TimedCPM{} -> mkPactExecutionService requestQ
        FastTimedCPM{} -> mkPactExecutionService requestQ
        Development -> mkPactExecutionService requestQ
        Testnet02 -> mkPactExecutionService requestQ

-- -------------------------------------------------------------------------- --
-- Mempool sync.

-- | Synchronize the local mempool over the P2P network.
--
runMempoolSyncClient
    :: Logger logger
    => HTTP.Manager
        -- ^ HTTP connection pool
    -> MempoolP2pConfig
    -> ChainResources logger
        -- ^ chain resources
    -> IO ()
runMempoolSyncClient mgr memP2pConfig chain = bracket create destroy go
  where
    create = do
        logg Debug "starting mempool p2p sync"
        p2pCreateNode v netId peer (logFunction syncLogger) peerDb mgr $
            mempoolSyncP2pSession chain (_mempoolP2pConfigPollInterval memP2pConfig)
    go n = do
        -- Run P2P client node
        logg Debug "mempool sync p2p node initialized, starting session"
        p2pStartNode p2pConfig n

    destroy n = p2pStopNode n `finally` logg Debug "mempool sync p2p node stopped"

    v = _chainwebVersion chain
    peer = _peerResPeer $ _chainResPeer chain
    p2pConfig = _peerResConfig (_chainResPeer chain)
        & set p2pConfigMaxSessionCount (_mempoolP2pConfigMaxSessionCount memP2pConfig)
        & set p2pConfigSessionTimeout (_mempoolP2pConfigSessionTimeout memP2pConfig)
    peerDb = _peerResDb $ _chainResPeer chain
    netId = MempoolNetwork $ _chainId chain

    logg = logFunctionText syncLogger
    syncLogger = setComponent "mempool-sync" $ _chainResLogger chain

mempoolSyncP2pSession :: ChainResources logger -> Seconds -> P2pSession
mempoolSyncP2pSession chain (Seconds pollInterval) logg0 env _ = do
    logg Debug "mempool sync session starting"
    Mempool.syncMempools' logg syncIntervalUs pool peerMempool
    logg Debug "mempool sync session finished"
    return True
  where
    peerMempool = MPC.toMempool v cid txcfg gaslimit env

    -- FIXME Potentially dangerous down-cast.
    syncIntervalUs :: Int
    syncIntervalUs = int pollInterval * 1000000

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    pool = _chainResMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    gaslimit = Mempool.mempoolBlockGasLimit pool
    cid = _chainId chain
    v = _chainwebVersion chain
