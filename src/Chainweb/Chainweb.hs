{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module provides the tools to initialize the different components of the
-- chainweb consensus and run a chainweb-consensus node.
--
-- The overall pattern is that for each component @C@ there is
--
-- 1.  a function @withC@ that manages the resources of the component and passes
--     a handle that encapsulates those resources to an inner computation, and
--
-- 2.  a function @runC@ that takes the resource handle of @C@ and starts the
    -- component.
--
-- This pattern allows to bootstrap components with mutual dependencies and
-- resolve timing and ordering constraints for the startup of services.
--
-- Logging functions are initialized in the enviroment and passed as call backs
-- to the components.
--
module Chainweb.Chainweb
(
-- * Chainweb Resources
  Chainweb(..)
, chainwebChains
, chainwebCutResources
, chainwebHostAddress
, chainwebMiner
, chainwebCoordinator
, chainwebLogger
, chainwebSocket
, chainwebPeer
, chainwebThrottler
, chainwebPutPeerThrottler
, chainwebMempoolThrottler
, chainwebConfig
, chainwebServiceSocket
, chainwebBackup
, StartedChainweb(..)
, ChainwebStatus(..)
, NowServing(..)

-- ** Mempool integration
, Mempool.pactTransactionConfig
, validatingMempoolConfig

, withChainweb
, runChainweb

-- * Throttler
, mkGenericThrottler
, mkPutPeerThrottler
, checkPathPrefix
, mkThrottler

, ThrottlingConfig(..)
, throttlingRate
, throttlingPeerRate
, defaultThrottlingConfig

-- * Cut Config
, CutConfig(..)
, cutFetchTimeout
, cutInitialBlockHeightLimit
, cutFastForwardBlockHeightLimit
, defaultCutConfig

) where

import Configuration.Utils hiding (Error, Lens', disabled)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (isPrefixOf)
import Data.List qualified as L
import Data.LogMessage (LogFunctionText)
import Data.Maybe
import Data.Text qualified as T

import GHC.Generics hiding (to)

import Network.HTTP.Client qualified as HTTP
import Network.HTTP2.Client qualified as HTTP2
import Network.Socket (Socket)
import Network.Wai
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS (WarpTLSException(..))
import Network.Wai.Middleware.RequestSizeLimit
import Network.Wai.Middleware.Throttle

import Prelude hiding (log)

import System.Clock
import System.LogLevel

-- internal modules

import Chainweb.Backup
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.Configuration
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Counter
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Mempool.InMem.ValidatingConfig
import Chainweb.Mempool.Mempool qualified as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.OpenAPIValidation qualified as OpenAPIValidation
import Chainweb.Pact.RestAPI.Server (PactServerData(..))
import Chainweb.Pact.Types (PactServiceConfig(..))
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.PayloadProvider
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Storage.Table.RocksDB
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.Version.Guards
import Chainweb.WebBlockHeaderDB

import P2P.Node.Configuration
import P2P.Node.PeerDB (PeerDb)
import P2P.Peer
import qualified Pact.Core.Gas as Pact
import qualified Chainweb.PayloadProvider.Pact.Genesis as Pact

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb logger = Chainweb
    { _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(ChainMap (ChainResources logger))
    , _chainwebCutResources :: !CutResources
    , _chainwebMiner :: !(Maybe (MinerResources logger))
    , _chainwebCoordinator :: !(Maybe (MiningCoordination logger))
    , _chainwebLogger :: !logger
    , _chainwebPeer :: !(PeerResources logger)
    , _chainwebManager :: !HTTP.Manager
    -- , _chainwebPactData :: ![(ChainId, PactServerData logger tbl)]
    , _chainwebThrottler :: !(Throttle Address)
    , _chainwebPutPeerThrottler :: !(Throttle Address)
    , _chainwebMempoolThrottler :: !(Throttle Address)
    , _chainwebConfig :: !ChainwebConfiguration
    , _chainwebServiceSocket :: !(Port, Socket)
    , _chainwebBackup :: !(BackupEnv logger)
    }

makeLenses ''Chainweb

chainwebSocket :: Getter (Chainweb logger) Socket
chainwebSocket = chainwebPeer . peerResSocket

-- Intializes all service API chainweb components but doesn't start any networking.
--
withChainweb
    :: forall logger
    . Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> FilePath
    -> FilePath
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withChainweb c logger rocksDb pactDbDir backupDir inner =
    withVersion (c ^. configChainwebVersion) $
        withPeerResources (view configP2p confWithBootstraps) logger $ \logger' peerRes -> do
            withSocket serviceApiPort serviceApiHost $ \serviceSock -> do
                let conf' = confWithBootstraps
                        & set configP2p (_peerResConfig peerRes)
                        & set (configServiceApi . serviceApiConfigPort) (fst serviceSock)
                withChainwebInternal
                    conf'
                    logger'
                    peerRes
                    serviceSock
                    rocksDb
                    pactDbDir
                    backupDir
                    inner
  where
    serviceApiPort = _serviceApiConfigPort $ _configServiceApi c
    serviceApiHost = _serviceApiConfigInterface $ _configServiceApi c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    confWithBootstraps
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers
            %~ (\x -> L.nub $ x <> _versionBootstraps (c ^. configChainwebVersion)) $ c

data StartedChainweb logger where
  StartedChainweb
    :: (Logger logger)
    => !(Chainweb logger)
    -> StartedChainweb logger
  Replayed :: !Cut -> !(Maybe Cut) -> StartedChainweb logger

data ChainwebStatus
    = ProcessStarted
    | PruningDatabases
    | InitializingChainResources
    | InitializingCutResources
    | InitialSyncInProgress
    | PactReplayInProgress
    | InitializingMinerResources
    | ChainwebStarted
    | ProcessDied !String
    | PactReplaySuccessful
    deriving (Generic, Eq, Ord, Show, NFData, ToJSON, FromJSON)

-- Intializes all service chainweb components but doesn't start any networking.
--
withChainwebInternal
    :: forall logger
    . Logger logger
    => HasVersion
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> (Port, Socket)
    -> RocksDb
    -> FilePath
    -> FilePath
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withChainwebInternal conf logger peerRes serviceSock rocksDb pactDbDir backupDir inner = do
    logFunctionJson logger Info InitializingChainResources
    txFailuresCounter <- newCounter @"txFailures"
    let monitorTxFailuresCounter =
            runForever (logFunctionText logger) "monitor txFailuresCounter" $ do
                approximateThreadDelay 60_000_000 {- 1 minute -}
                logFunctionCounter logger Info . (:[]) =<<
                    roll txFailuresCounter
    logg Debug "start initializing chain resources"
    logFunctionText logger Info $ "opening pact db in directory " <> sshow pactDbDir
    withAsync monitorTxFailuresCounter $ \_ ->
        concurrentWith
            -- initialize chains concurrently
            (\cid x -> do
                -- Initialize all chain resources, including payload providers
                runResourceT $ do
                    cr <- withChainResources
                        (chainLogger cid)
                        cid
                        rocksDb
                        (_peerResManager peerRes)
                        pactDbDir
                        (_peerResConfig peerRes)
                        myInfo
                        peerDb
                        (_configReorgLimit conf)
                        initialUnlimitedRewind
                        (_configPayloadProviders conf)
                    liftIO $ x cr
            )

            -- initialize global resources after all chain resources are initialized
            (\cs -> do
                logg Debug "finished initializing chain resources"
                global cs
            )
            (onChains [(cid, cid) | cid <- cidsList])
  where
    cids :: HS.HashSet ChainId
    cids = chainIds

    cidsList :: [ChainId]
    cidsList = toList cids

    mgr :: HTTP.Manager
    mgr = _peerResManager peerRes

    peer :: Peer
    peer = _peerResPeer peerRes

    myInfo :: PeerInfo
    myInfo = _peerInfo peer

    peerDb :: PeerDb
    peerDb = _peerResDb peerRes

    p2pConfig :: P2pConfiguration
    p2pConfig = _peerResConfig peerRes

    -- FIXME: make this configurable
    cutDbParams :: CutDbParams
    cutDbParams = (defaultCutDbParams $ _cutFetchTimeout cutConf)
        { _cutDbParamsLogLevel = Info
        , _cutDbParamsTelemetryLevel = Info
        , _cutDbParamsInitialHeightLimit = _cutInitialBlockHeightLimit cutConf
        , _cutDbParamsFastForwardHeightLimit = _cutFastForwardBlockHeightLimit cutConf
        , _cutDbParamsReadOnly = _configOnlySync conf || _configReadOnlyReplay conf
        }
      where
        cutConf = _configCuts conf

    initialUnlimitedRewind =
        isJust (_cutDbParamsInitialHeightLimit cutDbParams) ||
        isJust (_cutDbParamsInitialCutFile cutDbParams)

    -- Logger

    backupLogger :: logger
    backupLogger = addLabel ("component", "backup") logger

    chainLogger :: HasChainId c => c -> logger
    chainLogger cid = addLabel ("chain", toText (_chainId cid)) logger

    initLogger :: logger
    initLogger = setComponent "init" logger

    providerLogger :: HasPayloadProviderType p => p -> logger -> logger
    providerLogger p =
        addLabel ("provider", toText (_payloadProviderType p))

    logg :: LogFunctionText
    logg = logFunctionText initLogger

    -- Initialize global resources
    -- TODO: Can this be moved to a top-level function or broken down a bit to
    -- avoid excessive indentation?

    global
        :: ChainMap (ChainResources logger)
        -> IO ()
    global cs = runResourceT $ do
        let !webchain = mkWebBlockHeaderDb (fmap _chainResBlockHeaderDb cs)
            -- !pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            !providers = payloadProvidersForAllChains cs
            !cutLogger = setComponent "cut" logger

        liftIO $ logg Debug "start initializing cut resources"
        liftIO $ logFunctionJson logger Info InitializingCutResources

        cuts <- withCutResources cutLogger cutDbParams p2pConfig myInfo peerDb rocksDb webchain providers mgr
        liftIO $ do
            logg Debug "finished initializing cut resources"

            let !mLogger = setComponent "miner" logger
                !mConf = _configMining conf
                !mCutDb = _cutResCutDb cuts
                !throt  = _configThrottling conf

            -- initialize throttler
            throttler <- mkGenericThrottler $ _throttlingRate throt
            putPeerThrottler <- mkPutPeerThrottler $ _throttlingPeerRate throt
            mempoolThrottler <- mkMempoolThrottler $ _throttlingMempoolRate throt
            logg Debug "initialized throttlers"

            -- synchronize pact dbs with latest cut before we start the server
            -- and clients and begin mining.
            --
            -- This is a consistency check that validates the blocks in the
            -- current cut. If it fails an exception is raised. Also, if it
            -- takes long (for example, when doing a reset to a prior block
            -- height) we want this to happen before we go online.
            --
            let
                -- pactSyncChains =
                --     case _configSyncPactChains conf of
                --         Just syncChains
                --             | _configOnlySyncPact conf || _configReadOnlyReplay conf
                --             -> HM.filterWithKey (\k _ -> elem k syncChains) cs
                --         _ -> cs

            if _configReadOnlyReplay conf
              then do
                -- FIXME implement replay in payload provider
                error "Chainweb.Chainweb.withChainwebInternal: pact replay is not supported"
                -- logFunctionJson logger Info PactReplayInProgress
                -- -- note that we don't use the "initial cut" from cutdb because its height depends on initialBlockHeightLimit.
                -- highestCut <-
                --     unsafeMkCut v <$> readHighestCutHeaders v (logFunctionText logger) webchain (cutHashesTable rocksDb)
                -- lowerBoundCut <-
                --     tryLimitCut webchain (fromMaybe 0 $ _cutInitialBlockHeightLimit $ _configCuts conf) highestCut
                -- upperBoundCut <- forM (_cutFastForwardBlockHeightLimit $ _configCuts conf) $ \upperBound ->
                --     tryLimitCut webchain upperBound highestCut
                -- let
                --     replayOneChain :: (ChainResources logger, (BlockHeader, Maybe BlockHeader)) -> IO ()
                --     replayOneChain (cr, (l, u)) = do
                --         let chainPact = _chainResPact cr
                --         let logCr = logFunctionText
                --                 $ addLabel ("component", "pact")
                --                 $ addLabel ("sub-component", "init")
                --                 $ _chainResLogger cr
                --         void $ _pactReadOnlyReplay chainPact l u
                --         logCr Info "pact db synchronized"
                -- let bounds =
                --         HM.intersectionWith (,)
                --             pactSyncChains
                --             (HM.mapWithKey
                --                 (\cid bh ->
                --                     (bh, (HM.! cid) . _cutMap <$> upperBoundCut))
                --                 (_cutMap lowerBoundCut)
                --             )
                -- mapConcurrently_ replayOneChain bounds
                -- logg Info "finished fast forward replay"
                -- logFunctionJson logger Info PactReplaySuccessful
                -- inner $ Replayed lowerBoundCut upperBoundCut
              else
                if _configOnlySync conf
                  then do
                    error "Chainweb.Chainweb.withChainwebInternal: pact replay is not supported"
                    -- initialCut <- _cut mCutDb
                    -- logg Info "start synchronizing Pact DBs to initial cut"
                    -- logFunctionJson logger Info InitialSyncInProgress
                    -- synchronizePactDb pactSyncChains initialCut
                    -- logg Info "finished synchronizing Pact DBs to initial cut"
                    -- logFunctionJson logger Info PactReplayInProgress
                    -- logg Info "start replaying Pact DBs to fast forward cut"
                    -- fastForwardCutDb mCutDb
                    -- newCut <- _cut mCutDb
                    -- synchronizePactDb pactSyncChains newCut
                    -- logg Info "finished replaying Pact DBs to fast forward cut"
                    -- logFunctionJson logger Info PactReplaySuccessful
                    -- inner $ Replayed initialCut (Just newCut)
                  else do
                    initialCut <- _cut mCutDb

                    -- synchronize payload providers. this also initializes
                    -- mining.
                    synchronizeProviders webchain providers initialCut

                    -- FIXME: synchronize all payload providers
                    -- logg Info "start synchronizing Pact DBs to initial cut"
                    -- logFunctionJson logger Info InitialSyncInProgress
                    -- synchronizePactDb pactSyncChains initialCut
                    -- logg Info "finished synchronizing Pact DBs to initial cut"


                    -- withPactData cs cuts $ \pactData -> do
                    do
                        logg Debug "start initializing miner resources"
                        logFunctionJson logger Info InitializingMinerResources

                        withMiningCoordination mLogger mConf mCutDb $ \mc ->

                            -- Miner resources are used by the test-miner when in-node
                            -- mining is configured or by the mempool noop-miner (which
                            -- keeps the mempool updated) in production setups.
                            --
                            withMinerResources mLogger (_miningInNode mConf) cs mCutDb mc $ \m -> do
                                logFunctionJson logger Info ChainwebStarted
                                logg Debug "finished initializing miner resources"
                                let !haddr = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                                inner $ StartedChainweb Chainweb
                                    { _chainwebHostAddress = haddr
                                    , _chainwebChains = cs
                                    , _chainwebCutResources = cuts
                                    , _chainwebMiner = m
                                    , _chainwebCoordinator = mc
                                    , _chainwebLogger = logger
                                    , _chainwebPeer = peerRes
                                    , _chainwebManager = mgr
                                    -- , _chainwebPactData = pactData
                                    , _chainwebThrottler = throttler
                                    , _chainwebPutPeerThrottler = putPeerThrottler
                                    , _chainwebMempoolThrottler = mempoolThrottler
                                    , _chainwebConfig = conf
                                    , _chainwebServiceSocket = serviceSock
                                    , _chainwebBackup = BackupEnv
                                        { _backupRocksDb = rocksDb
                                        , _backupDir = backupDir
                                        , _backupPactDbDir = pactDbDir
                                        , _backupChainIds = cids
                                        , _backupLogger = backupLogger
                                        }
                                    }

    synchronizeProviders :: WebBlockHeaderDb -> ChainMap ConfiguredPayloadProvider -> Cut -> IO ()
    synchronizeProviders wbh providers c = do
        let startHeaders = HM.unionWith (\startHeader _genesisHeader -> startHeader)
                (_cutHeaders c)
                (imap (\cid () -> genesisBlockHeader cid) (HS.toMap chainIds))
        mapConcurrently_ syncOne startHeaders
      where
        syncOne hdr = forM_ (providers ^? atChain (_chainId hdr)) $ \case
            ConfiguredPayloadProvider provider -> do
                let loggr = (providerLogger provider(chainLogger hdr))
                logFunctionText loggr Info $
                    "sync payload provider to "
                        <> sshow (view blockHeight hdr)
                        <> ":" <> sshow (view blockHash hdr)
                finfo <- forkInfoForHeader wbh hdr Nothing
                logFunctionText loggr Debug $ "syncToBlock with fork info " <> sshow finfo
                r <- syncToBlock provider Nothing finfo `catch` \(e :: SomeException) -> do
                    logFunctionText loggr Warn $ "syncToBlock for " <> sshow finfo <> " failed with :" <> sshow e
                    throwM e
                unless (r == _forkInfoTargetState finfo) $ do
                    logFunctionText loggr Error $ "unexpectedResult"
                    error "Chainweb.Chainweb.synchronizeProviders: unexpected result state"
                    -- FIXME
            DisabledPayloadProvider -> do
                logFunctionText logger Info $
                    "payload provider disabled, not synced, on chain: " <> toText (_chainId hdr)

    -- synchronizePactDb :: HM.HashMap ChainId (ChainResources logger) -> Cut -> IO ()
    -- synchronizePactDb cs targetCut = do
    --     mapConcurrently_ syncOne $
    --         HM.intersectionWith (,) (_cutMap targetCut) cs
    --   where
    --     syncOne :: (BlockHeader, ChainResources logger) -> IO ()
    --     syncOne (bh, cr) = do
    --         let pact = _chainResPact cr
    --         let logCr = logFunctionText
    --                 $ addLabel ("component", "pact")
    --                 $ addLabel ("sub-component", "init")
    --                 $ _chainResLogger cr
    --         void $ _pactSyncToBlock pact bh
    --         logCr Debug "pact db synchronized"

    -- withPactData
    --     :: HM.HashMap ChainId (ChainResources logger)
    --     -> CutResources
    --     -> ([(ChainId, PactServerData logger tbl)] -> IO b)
    --     -> IO b
    -- withPactData cs cuts m = do
    --     let l = sortBy (compare `on` fst) (HM.toList cs)
    --     m $ l <&> fmap (\cr -> PactServerData
    --         { _pactServerDataCutDb = _cutResCutDb cuts
    --         , _pactServerDataMempool = _chainResMempool cr
    --         , _pactServerDataLogger = _chainResLogger cr
    --         , _pactServerDataPact = _chainResPact cr
    --         , _pactServerDataPayloadDb = _chainResPayloadDb cr
    --         })

-- -------------------------------------------------------------------------- --
-- Throttling

mkGenericThrottler :: Double -> IO (Throttle Address)
mkGenericThrottler rate = mkThrottler 30 rate (const True)

mkPutPeerThrottler :: Double -> IO (Throttle Address)
mkPutPeerThrottler rate = mkThrottler 30 rate $ \r ->
    elem "peer" (pathInfo r) && requestMethod r == "PUT"

mkMempoolThrottler :: Double -> IO (Throttle Address)
mkMempoolThrottler rate = mkThrottler 30 rate $ \r ->
    elem "mempool" (pathInfo r)

checkPathPrefix
    :: [T.Text]
        -- ^ the base rate granted to users of the endpoing
    -> Request
    -> Bool
checkPathPrefix endpoint r = endpoint `isPrefixOf` drop 3 (pathInfo r)

-- | The period is 1 second. Burst is 2*rate.
--
mkThrottler
    :: Double
        -- ^ expiration of a stall bucket in seconds
    -> Double
        -- ^ the base rate granted to users of the endpoint (requests per second)
    -> (Request -> Bool)
        -- ^ Predicate to select requests that are throttled
    -> IO (Throttle Address)
mkThrottler e rate c = initThrottler (defaultThrottleSettings $ TimeSpec (ceiling e) 0) -- expiration
    { throttleSettingsRate = rate -- number of allowed requests per period
    , throttleSettingsPeriod = 1_000_000 -- 1 second
    , throttleSettingsBurst = 4 * ceiling rate
    , throttleSettingsIsThrottled = c
    }

-- -------------------------------------------------------------------------- --
-- Run Chainweb

data NowServing = NowServing
    { _nowServingP2PAPI :: !Bool
    , _nowServingServiceAPI :: !Bool
    } deriving Eq

makeLenses ''NowServing

-- | Starts server and runs all network clients
--
runChainweb
    :: forall logger
    . Logger logger
    => HasVersion
    => Chainweb logger
    -> ((NowServing -> NowServing) -> IO ())
    -> IO ()
runChainweb cw nowServing = do
    logg Debug "start chainweb node"

    -- Create OpenAPI Validation Middlewars
    mkValidationMiddleware <- interleaveIO $
        OpenAPIValidation.mkValidationMiddleware (_chainwebLogger cw) (_chainwebManager cw)
    p2pValidationMiddleware <-
        if _p2pConfigValidateSpec (_configP2p $ _chainwebConfig cw)
        then do
            logg Warn $ "OpenAPI spec validation enabled on P2P API, make sure this is what you want"
            mkValidationMiddleware
        else return id
    serviceApiValidationMiddleware <-
        if _serviceApiConfigValidateSpec (_configServiceApi $ _chainwebConfig cw)
        then do
            logg Warn $ "OpenAPI spec validation enabled on service API, make sure this is what you want"
            mkValidationMiddleware
        else return id

    concurrentlies_

        -- 1. Start serving P2P Rest API
        [ (if tls then serve else servePlain)
            $ httpLog
            . throttle (_chainwebPutPeerThrottler cw)
            . throttle (_chainwebMempoolThrottler cw)
            . throttle (_chainwebThrottler cw)
            . p2pRequestSizeLimit
            . p2pValidationMiddleware

        -- 2. Start Clients (with a delay of 500ms)
        , threadDelay 500000 >> clients

        -- 3. Start serving local service API
        , threadDelay 500000 >> do
            serveServiceApi
                $ serviceHttpLog
                . serviceRequestSizeLimit
                . serviceApiValidationMiddleware
        ]

  where

    tls = _p2pConfigTls $ _configP2p $ _chainwebConfig cw

    clients :: IO ()
    clients = do
        concurrentlies_ $ concat
            [ miner
            , cutNetworks (_chainwebCutResources cw)
            , runP2pNodesOfAllChains (_chainwebChains cw)
            ]

    logg :: LogFunctionText
    logg = logFunctionText $ _chainwebLogger cw

    -- chains
    chains :: [(ChainId, ChainResources logger)]
    chains = itoList (_chainwebChains cw)

    chainVals :: [ChainResources logger]
    chainVals = map snd chains

    -- collect server resources
    proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
    proj f = chains & mapped . _2 %~ f

    peerDb = _peerResDb (_chainwebPeer cw)

    -- FIXME export the SomeServer instead of DBs?
    -- I.e. the handler would be created in the chain resource.
    -- Similar to how it is done with the payload provider APIs.
    --
    -- chainDbsToServe :: [(ChainId, BlockHeaderDb)]
    -- chainDbsToServe = proj _chainResBlockHeaderDb
    chainDbsToServe :: ChainMap BlockHeaderDb
    chainDbsToServe = _chainResBlockHeaderDb <$> _chainwebChains cw

    mempoolsToServe :: ChainMap (Mempool.MempoolBackend Pact.Transaction)
    -- mempoolsToServe = proj _chainResMempool
    mempoolsToServe = mempty

    pactDbsToServe :: [(ChainId, PactServerData logger tbl)]
    -- pactDbsToServe = _chainwebPactData cw
    pactDbsToServe = []

    -- As long as all Pact payload providers live within the consensus node, it
    -- probably makes sense to serve the P2P network for mempools through the
    -- consensus node API.
    -- TODO: only include the mempools for enabled payload providers
    --
    memP2pPeersToServe :: [(NetworkId, PeerDb)]
    -- memP2pToServe = (\(i, _) -> (MempoolNetwork i, peerDb)) <$> chains
    memP2pPeersToServe = []

    loggServerError msg (Just r) e =
        "HTTP server error (" <> msg <> "): " <> sshow e <> ". Request: " <> sshow r
    loggServerError msg Nothing e =
        "HTTP server error (" <> msg <> "): " <> sshow e

    logWarpException msg clientClosedConnectionsCounter r e
        | Just InsecureConnectionDenied <- fromException e =
            return ()
        | Just ClientClosedConnectionPrematurely <- fromException e =
            inc clientClosedConnectionsCounter
        -- this isn't really an error, this is a graceful close.
        -- see https://github.com/kazu-yamamoto/http2/issues/102
        | Just HTTP2.ConnectionIsClosed <- fromException e =
            return ()
        | otherwise =
            when (defaultShouldDisplayException e) $
                logg Debug $ loggServerError msg r e

    -- P2P Server

    serverSettings :: Counter "clientClosedConnections" -> Settings
    serverSettings clientClosedConnectionsCounter =
        peerServerSettings (_peerResPeer $ _chainwebPeer cw)
        & setOnException (logWarpException "P2P API" clientClosedConnectionsCounter)
        & setBeforeMainLoop (nowServing (nowServingP2PAPI .~ True))

    monitorConnectionsClosedByClient :: Counter "clientClosedConnections" -> IO ()
    monitorConnectionsClosedByClient clientClosedConnectionsCounter =
        runForever logg "ConnectionClosedByClient.counter" $ do
            approximateThreadDelay 60_000_000 {- 1 minute -}
            logFunctionCounter (_chainwebLogger cw) Info . (:[]) =<<
                roll clientClosedConnectionsCounter

    chainwebServerDbs :: ChainwebServerDbs
    chainwebServerDbs = ChainwebServerDbs
        { _chainwebServerCutDb = Just cutDb
        , _chainwebServerBlockHeaderDbs = chainDbsToServe
        , _chainwebServerMempools = mempoolsToServe
        -- , _chainwebServerPayloads = payloadsToServeOnP2pApi chains
        , _chainwebServerPayloads = ChainMap $ HM.fromList $ payloadsToServeOnP2pApi chains
        , _chainwebServerPeerDbs
            = (CutNetwork, cutPeerDb)
            : memP2pPeersToServe
            <> payloadP2pPeersToServe chains
        }

    serve :: Middleware -> IO ()
    serve mw = do

        clientClosedConnectionsCounter <- newCounter
        concurrently_
            (serveChainwebSocketTls
                (serverSettings clientClosedConnectionsCounter)
                (_peerCertificateChain $ _peerResPeer $ _chainwebPeer cw)
                (_peerKey $ _peerResPeer $ _chainwebPeer cw)
                (_peerResSocket $ _chainwebPeer cw)
                (_chainwebConfig cw)
                chainwebServerDbs
                mw)
            (monitorConnectionsClosedByClient clientClosedConnectionsCounter)

    -- serve without tls
    servePlain :: Middleware -> IO ()
    servePlain mw = do
        clientClosedConnectionsCounter <- newCounter
        concurrently_
            (serveChainwebSocket
                (serverSettings clientClosedConnectionsCounter)
                (_peerResSocket $ _chainwebPeer cw)
                (_chainwebConfig cw)
                chainwebServerDbs
                mw
            )
            (monitorConnectionsClosedByClient clientClosedConnectionsCounter)

    -- Request size limit for the service API
    --
    serviceRequestSizeLimit :: Middleware
    serviceRequestSizeLimit = requestSizeLimitMiddleware $
        setMaxLengthForRequest (\_req -> pure $ Just $ 2 * 1024 * 1024) -- 2MB
        defaultRequestSizeLimitSettings

    -- Request size limit for the P2P API
    --
    -- NOTE: this may need to have to be adjusted if the p2p limits for batch
    -- sizes or number of branch bound change. It may also need adjustment for
    -- other protocol changes, like additional HTTP request headers or changes
    -- in the mempool protocol.
    --
    -- FIXME: can we make this smaller and still let the mempool work?
    --
    p2pRequestSizeLimit :: Middleware
    p2pRequestSizeLimit = requestSizeLimitMiddleware $
        setMaxLengthForRequest (\_req -> pure $ Just $ 2 * 1024 * 1024) -- 2MB
        defaultRequestSizeLimitSettings

    httpLog :: Middleware
    httpLog = requestResponseLogger $ setComponent "http:p2p-api" (_chainwebLogger cw)

    -- Service API Server

    serviceApiServerSettings
        :: Counter "clientClosedConnections"
        -> Port -> HostPreference -> Settings
    serviceApiServerSettings clientClosedConnectionsCounter port interface = defaultSettings
        & setPort (int port)
        & setHost interface
        & setOnException
            (logWarpException "Service API" clientClosedConnectionsCounter)
        & setBeforeMainLoop (nowServing (nowServingServiceAPI .~ True))
        & setServerName "Chainweb Service API"

    serviceApiHost = _serviceApiConfigInterface $ _configServiceApi $ _chainwebConfig cw

    backupApiEnabled = _enableConfigEnabled $ _configBackupApi $ _configBackup $ _chainwebConfig cw

    serveServiceApi :: Middleware -> IO ()
    serveServiceApi mw = do
        clientClosedConnectionsCounter <- newCounter
        serveServiceApiSocket
            (serviceApiServerSettings clientClosedConnectionsCounter (fst $ _chainwebServiceSocket cw) serviceApiHost)
            (snd $ _chainwebServiceSocket cw)
            ChainwebServerDbs
                { _chainwebServerCutDb = Just cutDb
                , _chainwebServerBlockHeaderDbs = chainDbsToServe
                , _chainwebServerMempools = mempoolsToServe
                , _chainwebServerPayloads = ChainMap $ HM.fromList $ payloadsToServeOnServiceApi chains

                -- We do not want to serve peer APIs on the service API.
                -- If at all we could serve the GET endpoints.
                , _chainwebServerPeerDbs = []
                }
            (_chainwebCoordinator cw)
            (HeaderStream . _configHeaderStream $ _chainwebConfig cw)
            (_chainwebBackup cw <$ guard backupApiEnabled)
            (_serviceApiPayloadBatchLimit . _configServiceApi $ _chainwebConfig cw)
            mw

    serviceHttpLog :: Middleware
    serviceHttpLog = requestResponseLogger $ setComponent "http:service-api" (_chainwebLogger cw)

    -- HTTP Request Logger

    -- Cut DB and Miner

    cutDb :: CutDb
    cutDb = _cutResCutDb $ _chainwebCutResources cw

    cutPeerDb :: PeerDb
    cutPeerDb = _cutResPeerDb $ _chainwebCutResources cw

    miner :: [IO ()]
    miner = maybe [] (\m -> [ runMiner m ]) $ _chainwebMiner cw
