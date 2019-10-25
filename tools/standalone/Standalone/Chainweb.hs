{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Module: Standalone.Chainweb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Standalone.Chainweb where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad

import Data.CAS
import Data.CAS.RocksDB
import Data.Foldable
import Data.Function
import Data.List (sortBy)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Network.Wai.Middleware.Throttle

import P2P.Node.Configuration
import P2P.Peer

import System.Clock
import System.LogLevel

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI (HeaderStream(..))
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Logger
import Chainweb.NodeId
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService
import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool

import Standalone.Utils

withChainwebStandalone
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> Maybe FilePath
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebStandalone c logger rocksDb dbDir resetDb inner =
    withPeerResources v (view configP2p conf) logger $ \logger' peer ->
      withChainwebInternalStandalone
        (set configP2p (_peerResConfig peer) conf)
        logger'
        peer
        rocksDb
        dbDir
        (Just (_configNodeId c))
        resetDb
        inner
  where
    v = _chainwebVersion c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    conf
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers <>~ bootstrapPeerInfos v $ c

withChainResourcesStandalone
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
      -- ^ whether to prune the database
    -> Maybe FilePath
      -- ^ database directory for checkpointer
    -> Maybe NodeId
    -> Bool
      -- ^ reset database directory
    -> (ChainResources logger -> IO a)
    -> IO a
withChainResourcesStandalone v cid rdb peer logger mempoolCfg0 cdbv payloadDb prune dbDir nodeid resetDb inner =
    withBlockHeaderDb rdb v cid $ \cdb -> do
        pexMv <- newEmptyMVar
        let mempoolCfg = mempoolCfg0 pexMv
        Mempool.withInMemoryMempool_ (setComponent "mempool" logger) mempoolCfg v $ \mempool -> do
            -- placing mempool access shim here
            -- putting a default here for now.
              let mpa = onlyCoinTransferMemPoolAccess cid 10
              withPactService' v cid (setComponent "pact" logger)
                    mpa cdbv cdb payloadDb dbDir nodeid resetDb $
                \requestQ -> do
                      -- prune blockheader db
                      when prune $ do
                          logg Info "start pruning block header database"
                          x <- pruneForks logger cdb (diam * 3) $ \_h _payloadInUse ->

                          -- FIXME At the time of writing his payload hashes are not
                          -- unique. The pruning algorithm can handle non-uniquness
                          -- between within a chain between forks, but not across
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
                          logg Info $
                            "finished pruning block header database. Deleted "
                            <> sshow x
                            <> " block headers."

                      -- replay pact
                      let pact = pes requestQ
                      putMVar pexMv pact

                      -- run inner
                      inner ChainResources
                          { _chainResPeer = peer
                          , _chainResBlockHeaderDb = cdb
                          , _chainResLogger = logger
                          , _chainResMempool = mempool
                          , _chainResPact = pact
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
        -- Testnet00 -> mkPactExecutionService requestQ
        -- Testnet01 -> mkPactExecutionService requestQ
        Testnet02 -> mkPactExecutionService requestQ

withChainwebInternalStandalone
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> RocksDb
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebInternalStandalone conf logger peer rocksDb dbDir nodeid resetDb inner = do
    initializePayloadDb v payloadDb
    cdbv <- newEmptyMVar
    concurrentWith
      -- initialize chains concurrently
      (\cid -> do
          let mcfg = validatingMempoolConfig cid v (_configBlockGasLimit conf)
          withChainResourcesStandalone v cid rocksDb peer (chainLogger cid)
                mcfg cdbv payloadDb prune dbDir nodeid resetDb)

      -- initialize global resources after all chain resources are
      -- initialized
      (\cs -> global (HM.fromList $ zip cidsList cs) cdbv)
      cidsList
  where
    prune = _configPruneChainDatabase conf
    cidsList = toList cids
    payloadDb = newPayloadDb rocksDb
    chainLogger cid = addLabel ("chain", toText cid) logger
    logg = logFunctionText logger

    -- initialize global resources
    global cs cdbv = do
        let webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            cutLogger = setComponent "cut" logger
            mgr = _peerResManager peer
        logg Info "start initializing cut resources"
        withCutResources cutConfig peer cutLogger
            rocksDb webchain payloadDb mgr pact $ \cuts -> do
                logg Info "finished initializing cut resources"
                let mLogger = setComponent "miner" logger
                    mConf = _configMiner conf
                    mCutDb = _cutResCutDb cuts

                    -- initialize throttler
                throttler <- initThrottler
                    (defaultThrottleSettings $ TimeSpec 4 0)
                    { throttleSettingsRate = int $ _configThrottleRate conf
                    , throttleSettingsPeriod = 1 / micro -- 1 second (measured in usec)
                    , throttleSettingsBurst = int $ _configThrottleRate conf
                    , throttleSettingsIsThrottled = const True
                    -- , throttleSettingsIsThrottled = \r -> any (flip elem (pathInfo r))
                    --     [ "cut"
                    --     , "header"
                    --     , "payload"
                    --     , "mempool"
                    --     , "peer"
                    --     ]
                    }

                void $! putMVar cdbv mCutDb

                logg Info "start synchronizing Pact DBs"
                synchronizePactDb cs mCutDb
                logg Info "finished synchronizing Pact DBs"

                withPactData cs cuts $ \pactData -> do
                    logg Info "start initializing miner resources"
                    withMiningCoordination mLogger (_configCoordinator conf) mCutDb $ \mc -> do
                        withMinerResources mLogger mConf mCutDb $ \m -> do
                            logg Info "finished initializing miner resources"
                            inner Chainweb
                                      { _chainwebHostAddress =
                                          _peerConfigAddr
                                          $ _p2pConfigPeer
                                          $ _configP2p conf
                                      , _chainwebChains = cs
                                      , _chainwebCutResources = cuts
                                      , _chainwebMiner = m
                                      , _chainwebCoordinator = mc
                                      , _chainwebHeaderStream =
                                          HeaderStream $ _configHeaderStream conf
                                      , _chainwebLogger = logger
                                      , _chainwebPeer = peer
                                      , _chainwebPayloadDb = payloadDb
                                      , _chainwebManager = mgr
                                      , _chainwebPactData = pactData
                                      , _chainwebThrottler = throttler
                                      , _chainwebConfig = conf
                                      }

    withPactData cs cuts m
        | _enableConfigEnabled (_configTransactionIndex conf) = do
            logg Info "Transaction index enabled"
            let l = sortBy (compare `on` fst) (HM.toList cs)
            m $ map (\(c, cr) -> (c, (cuts, cr))) l
        | otherwise = do
            logg Info "Transaction index disabled"
            m []
    v = _configChainwebVersion conf
    cids = chainIds v

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        , _cutDbConfigUseOrigin = _configIncludeOrigin conf
        }

    synchronizePactDb cs cutDb = do
        currentCut <- _cut cutDb
        mapM_ syncOne $ mergeCutResources $ _cutMap currentCut
      where
        mergeCutResources c =
            let f cid bh = (bh, fromJuste $ HM.lookup cid cs)
            in map snd $ HM.toList $ HM.mapWithKey f c
        syncOne (bh, cr) = do
            let pact = _chainResPact cr
            let logCr = logFunctionText $ _chainResLogger cr
            let hsh = _blockHash bh
            let h = _blockHeight bh
            logCr Info $ "pact db synchronizing to block "
                      <> T.pack (show (h, hsh))
            payload <- payloadWithOutputsToPayloadData
                       <$> casLookupM payloadDb (_blockPayloadHash bh)
            void $ _pactValidateBlock pact bh payload
            logCr Info "pact db synchronized"
