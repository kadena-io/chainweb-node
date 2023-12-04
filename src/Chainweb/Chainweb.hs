{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- * GC Configuration
  ChainDatabaseGcConfig(..)
, chainDatabaseGcToText
, chainDatabaseGcFromText

-- * Chainweb Resources
, Chainweb(..)
, chainwebChains
, chainwebCutResources
, chainwebHostAddress
, chainwebMiner
, chainwebCoordinator
, chainwebLogger
, chainwebSocket
, chainwebPeer
, chainwebPayloadDb
, chainwebPactData
, chainwebThrottler
, chainwebPutPeerThrottler
, chainwebMempoolThrottler
, chainwebConfig
, chainwebServiceSocket
, chainwebBackup
, StartedChainweb(..)
, ChainwebStatus(..)

-- ** Mempool integration
, ChainwebTransaction
, Mempool.chainwebTransactionConfig
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
, cutPruneChainDatabase
, cutFetchTimeout
, cutInitialBlockHeightLimit
, cutFastForwardBlockHeightLimit
, defaultCutConfig

) where

import Configuration.Utils hiding (Error, Lens', disabled)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, readMVar)
import Control.DeepSeq
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (fromException, throwM)

import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (isPrefixOf, sortBy)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.These (These(..))
import qualified Data.Vector as V

import GHC.Generics

import qualified Network.HTTP.Client as HTTP
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
import Chainweb.Chainweb.MempoolSyncClient
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Chainweb.PruneChainDatabase
import Chainweb.Counter
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.HostAddress
import Chainweb.Logger
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import qualified Chainweb.OpenAPIValidation as OpenAPIValidation
import Chainweb.Pact.RestAPI.Server (PactServerData(..))
import Chainweb.Pact.Service.Types (PactServiceConfig(..))
import Chainweb.Pact.Validations
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.Version.Guards
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table.RocksDB

import Data.LogMessage (LogFunctionText)

import P2P.Node.Configuration
import P2P.Node.PeerDB (PeerDb)
import P2P.Peer

import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb logger tbl = Chainweb
    { _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId (ChainResources logger))
    , _chainwebCutResources :: !(CutResources logger tbl)
    , _chainwebMiner :: !(Maybe (MinerResources logger tbl))
    , _chainwebCoordinator :: !(Maybe (MiningCoordination logger tbl))
    , _chainwebLogger :: !logger
    , _chainwebPeer :: !(PeerResources logger)
    , _chainwebPayloadDb :: !(PayloadDb tbl)
    , _chainwebManager :: !HTTP.Manager
    , _chainwebPactData :: ![(ChainId, PactServerData logger tbl)]
    , _chainwebThrottler :: !(Throttle Address)
    , _chainwebPutPeerThrottler :: !(Throttle Address)
    , _chainwebMempoolThrottler :: !(Throttle Address)
    , _chainwebConfig :: !ChainwebConfiguration
    , _chainwebServiceSocket :: !(Port, Socket)
    , _chainwebBackup :: !(BackupEnv logger)
    }

makeLenses ''Chainweb

chainwebSocket :: Getter (Chainweb logger t) Socket
chainwebSocket = chainwebPeer . peerResSocket

instance HasChainwebVersion (Chainweb logger t) where
    _chainwebVersion = _chainwebVersion . _chainwebCutResources
    {-# INLINE _chainwebVersion #-}

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
    -> Bool
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withChainweb c logger rocksDb pactDbDir backupDir resetDb inner =
    withPeerResources v (view configP2p confWithBootstraps) logger $ \logger' peer ->
        withSocket serviceApiPort serviceApiHost $ \serviceSock -> do
            let conf' = confWithBootstraps
                    & set configP2p (_peerResConfig peer)
                    & set (configServiceApi . serviceApiConfigPort) (fst serviceSock)
            withChainwebInternal
                conf'
                logger'
                peer
                serviceSock
                rocksDb
                pactDbDir
                backupDir
                resetDb
                inner
  where
    serviceApiPort = _serviceApiConfigPort $ _configServiceApi c
    serviceApiHost = _serviceApiConfigInterface $ _configServiceApi c

    v = _chainwebVersion c

    -- Here we inject the hard-coded bootstrap peer infos for the configured
    -- chainweb version into the configuration.
    confWithBootstraps
        | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
        | otherwise = configP2p . p2pConfigKnownPeers
            %~ (\x -> L.nub $ x <> _versionBootstraps v) $ c

-- TODO: The type InMempoolConfig contains parameters that should be
-- configurable as well as parameters that are determined by the chainweb
-- version or the chainweb protocol. These should be separated in to two
-- different types.

validatingMempoolConfig
    :: ChainId
    -> ChainwebVersion
    -> Mempool.GasLimit
    -> Mempool.GasPrice
    -> MVar PactExecutionService
    -> Mempool.InMemConfig ChainwebTransaction
validatingMempoolConfig cid v gl gp mv = Mempool.InMemConfig
    { Mempool._inmemTxCfg = txcfg
    , Mempool._inmemTxBlockSizeLimit = gl
    , Mempool._inmemTxMinGasPrice = gp
    , Mempool._inmemMaxRecentItems = maxRecentLog
    , Mempool._inmemPreInsertPureChecks = preInsertSingle
    , Mempool._inmemPreInsertBatchChecks = preInsertBatch
    , Mempool._inmemCurrentTxsSize = currentTxsSize
    }
  where
    txcfg = Mempool.chainwebTransactionConfig (maxBound :: PactParserVersion)
        -- The mempool doesn't provide a chain context to the codec which means
        -- that the latest version of the parser is used.

    maxRecentLog = 2048

    currentTxsSize = 1024 * 1024 -- ~16MB per mempool
        -- 1M items is is sufficient for supporing about 12 TPS per chain, which
        -- is about 360 tx per block. Larger TPS values would result in false
        -- negatives in the set.

    -- | Validation: Is this TX associated with the correct `ChainId`?
    --
    preInsertSingle :: ChainwebTransaction -> Either Mempool.InsertError ChainwebTransaction
    preInsertSingle tx = do
        let !pay = payloadObj . P._cmdPayload $ tx
            pcid = P._pmChainId $ P._pMeta pay
            sigs = P._cmdSigs tx
            ver  = P._pNetworkId pay
        if | not $ assertParseChainId pcid -> Left $ Mempool.InsertErrorOther "Unparsable ChainId"
           | not $ assertChainId cid pcid  -> Left Mempool.InsertErrorMetadataMismatch
           | not $ assertSigSize sigs      -> Left $ Mempool.InsertErrorOther "Too many signatures"
           | not $ assertNetworkId v ver   -> Left Mempool.InsertErrorMetadataMismatch
           | otherwise                     -> Right tx

    -- | Validation: All checks that should occur before a TX is inserted into
    -- the mempool. A rejection at this stage means that something is
    -- fundamentally wrong/illegal with the TX, and that it should be rejected
    -- completely and not gossiped to other peers.
    --
    -- We expect this to be called in two places: once when a new Pact
    -- Transaction is submitted via the @send@ endpoint, and once when a new TX
    -- is gossiped to us from a peer's mempool.
    --
    preInsertBatch
        :: V.Vector (T2 Mempool.TransactionHash ChainwebTransaction)
        -> IO (V.Vector (Either (T2 Mempool.TransactionHash Mempool.InsertError)
                                (T2 Mempool.TransactionHash ChainwebTransaction)))
    preInsertBatch txs = do
        pex <- readMVar mv
        rs <- _pactPreInsertCheck pex cid (V.map ssnd txs) >>= either throwM pure
        pure $ alignWithV f rs txs
      where
        f (These r (T2 h t)) = case r of
                                 Left e -> Left (T2 h e)
                                 Right _ -> Right (T2 h t)
        f (That (T2 h _)) = Left (T2 h $ Mempool.InsertErrorOther "preInsertBatch: align mismatch 0")
        f (This _) = Left (T2 (Mempool.TransactionHash "") (Mempool.InsertErrorOther "preInsertBatch: align mismatch 1"))


data StartedChainweb logger where
  StartedChainweb :: (CanReadablePayloadCas cas, Logger logger) => !(Chainweb logger cas) -> StartedChainweb logger
  Replayed :: !Cut -> !Cut -> StartedChainweb logger

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
    .  Logger logger
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> (Port, Socket)
    -> RocksDb
    -> FilePath
    -> FilePath
    -> Bool
    -> (StartedChainweb logger -> IO ())
    -> IO ()
withChainwebInternal conf logger peer serviceSock rocksDb pactDbDir backupDir resetDb inner = do

    unless (_configOnlySyncPact conf) $
        initializePayloadDb v payloadDb

    -- Garbage Collection
    -- performed before PayloadDb and BlockHeaderDb used by other components
    logFunctionJson logger Info PruningDatabases
    logg Info "start pruning databases"
    case _cutPruneChainDatabase (_configCuts conf) of
        GcNone -> return ()
        GcHeaders ->
            pruneAllChains (pruningLogger "headers") rocksDb v []
        GcHeadersChecked ->
            pruneAllChains (pruningLogger "headers-checked") rocksDb v [CheckPayloads, CheckFull]
        GcFull ->
            fullGc (pruningLogger "full") rocksDb v
    logg Info "finished pruning databases"
    logFunctionJson logger Info InitializingChainResources

    logg Info "start initializing chain resources"
    concurrentWith
        -- initialize chains concurrently
        (\cid x -> do
            let mcfg = validatingMempoolConfig cid v (_configBlockGasLimit conf) (_configMinGasPrice conf)
            -- NOTE: the gas limit may be set based on block height in future, so this approach may not be valid.
            let maxGasLimit = fromIntegral <$> maxBlockGasLimit v maxBound
            case maxGasLimit of
                Just maxGasLimit'
                    | _configBlockGasLimit conf > maxGasLimit' ->
                        logg Warn $ T.unwords
                            [ "configured block gas limit is greater than the"
                            , "maximum for this chain; the maximum will be used instead"
                            ]
                _ -> return ()
            withChainResources
                v
                cid
                rocksDb
                (chainLogger cid)
                mcfg
                payloadDb
                pactDbDir
                (pactConfig maxGasLimit)
                x
        )

        -- initialize global resources after all chain resources are initialized
        (\cs -> do
            logg Info "finished initializing chain resources"
            global (HM.fromList $ zip cidsList cs)
        )
        cidsList
  where
    pactConfig maxGasLimit = PactServiceConfig
      { _pactReorgLimit = _configReorgLimit conf
      , _pactLocalRewindDepthLimit = _configLocalRewindDepthLimit conf
      , _pactPreInsertCheckTimeout = _configPreInsertCheckTimeout conf
      , _pactQueueSize = _configPactQueueSize conf
      , _pactResetDb = resetDb
      , _pactAllowReadsInLocal = _configAllowReadsInLocal conf
      , _pactUnlimitedInitialRewind =
          isJust (_cutDbParamsInitialHeightLimit cutConfig) ||
          isJust (_cutDbParamsInitialCutFile cutConfig)
      , _pactBlockGasLimit = maybe id min maxGasLimit (_configBlockGasLimit conf)
      , _pactLogGas = _configLogGas conf
      , _pactModuleCacheLimit = _configModuleCacheLimit conf
      , _pactFullHistoryRequired = _configRosetta conf -- this could be OR'd with other things that require full history
      }

    pruningLogger :: T.Text -> logger
    pruningLogger l = addLabel ("sub-component", l)
        $ setComponent "database-pruning" logger

    cidsList :: [ChainId]
    cidsList = toList cids

    payloadDb :: PayloadDb RocksDbTable
    payloadDb = newPayloadDb rocksDb

    chainLogger :: ChainId -> logger
    chainLogger cid = addLabel ("chain", toText cid) logger

    initLogger :: logger
    initLogger = setComponent "init" logger

    logg :: LogFunctionText
    logg = logFunctionText initLogger

    -- Initialize global resources
    global
        :: HM.HashMap ChainId (ChainResources logger)
        -> IO ()
    global cs = do
        let !webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            !pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            !cutLogger = setComponent "cut" logger
            !mgr = _peerResManager peer

        logg Info "start initializing cut resources"
        logFunctionJson logger Info InitializingCutResources

        withCutResources cutConfig peer cutLogger rocksDb webchain payloadDb mgr pact $ \cuts -> do
            logg Info "finished initializing cut resources"

            let !mLogger = setComponent "miner" logger
                !mConf = _configMining conf
                !mCutDb = _cutResCutDb cuts
                !throt  = _configThrottling conf

            -- initialize throttler
            throttler <- mkGenericThrottler $ _throttlingRate throt
            putPeerThrottler <- mkPutPeerThrottler $ _throttlingPeerRate throt
            mempoolThrottler <- mkMempoolThrottler $ _throttlingMempoolRate throt
            logg Info "initialized throttlers"

            -- synchronize pact dbs with latest cut before we start the server
            -- and clients and begin mining.
            --
            -- This is a consistency check that validates the blocks in the
            -- current cut. If it fails an exception is raised. Also, if it
            -- takes long (for example, when doing a reset to a prior block
            -- height) we want this to happen before we go online.
            --
            let
                pactSyncChains =
                    case _configSyncPactChains conf of
                      Just syncChains | _configOnlySyncPact conf -> HM.filterWithKey (\k _ -> elem k syncChains) cs
                      _ -> cs
            logg Info "start synchronizing Pact DBs to initial cut"
            logFunctionJson logger Info InitialSyncInProgress
            initialCut <- _cut mCutDb
            synchronizePactDb pactSyncChains initialCut
            logg Info "finished synchronizing Pact DBs to initial cut"

            if _configOnlySyncPact conf
            then do
                logFunctionJson logger Info PactReplayInProgress
                logg Info "start replaying Pact DBs to fast forward cut"
                fastForwardCutDb mCutDb
                newCut <- _cut mCutDb
                synchronizePactDb pactSyncChains newCut
                logg Info "finished replaying Pact DBs to fast forward cut"
                logFunctionJson logger Info PactReplaySuccessful
                inner $ Replayed initialCut newCut
            else do
                withPactData cs cuts $ \pactData -> do
                    logg Info "start initializing miner resources"
                    logFunctionJson logger Info InitializingMinerResources

                    withMiningCoordination mLogger mConf mCutDb $ \mc ->

                        -- Miner resources are used by the test-miner when in-node
                        -- mining is configured or by the mempool noop-miner (which
                        -- keeps the mempool updated) in production setups.
                        --
                        withMinerResources mLogger (_miningInNode mConf) cs mCutDb mc $ \m -> do
                            logFunctionJson logger Info ChainwebStarted
                            logg Info "finished initializing miner resources"
                            let !haddr = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                            inner $ StartedChainweb Chainweb
                                { _chainwebHostAddress = haddr
                                , _chainwebChains = cs
                                , _chainwebCutResources = cuts
                                , _chainwebMiner = m
                                , _chainwebCoordinator = mc
                                , _chainwebLogger = logger
                                , _chainwebPeer = peer
                                , _chainwebPayloadDb = view cutDbPayloadDb $ _cutResCutDb cuts
                                , _chainwebManager = mgr
                                , _chainwebPactData = pactData
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

    withPactData
        :: HM.HashMap ChainId (ChainResources logger)
        -> CutResources logger tbl
        -> ([(ChainId, PactServerData logger tbl)] -> IO b)
        -> IO b
    withPactData cs cuts m = do
        let l = sortBy (compare `on` fst) (HM.toList cs)
        m $ l <&> fmap (\cr -> PactServerData
            { _pactServerDataCutDb = _cutResCutDb cuts
            , _pactServerDataMempool = _chainResMempool cr
            , _pactServerDataLogger = _chainResLogger cr
            , _pactServerDataPact = _chainResPact cr
            })

    v = _configChainwebVersion conf
    cids = chainIds v
    backupLogger = addLabel ("component", "backup") logger

    -- FIXME: make this configurable
    cutConfig :: CutDbParams
    cutConfig = (defaultCutDbParams v $ _cutFetchTimeout cutConf)
        { _cutDbParamsLogLevel = Info
        , _cutDbParamsTelemetryLevel = Info
        , _cutDbParamsInitialHeightLimit = _cutInitialBlockHeightLimit cutConf
        , _cutDbParamsFastForwardHeightLimit = _cutFastForwardBlockHeightLimit cutConf
        , _cutDbParamsReadOnly = _configOnlySyncPact conf
        }
      where
        cutConf = _configCuts conf

    synchronizePactDb :: HM.HashMap ChainId (ChainResources logger) -> Cut -> IO ()
    synchronizePactDb cs targetCut = do
        mapConcurrently_ syncOne $
            HM.intersectionWith (,) (_cutMap targetCut) cs
      where
        syncOne :: (BlockHeader, ChainResources logger) -> IO ()
        syncOne (bh, cr) = do
            let pact = _chainResPact cr
            let logCr = logFunctionText
                    $ addLabel ("component", "pact")
                    $ addLabel ("sub-component", "init")
                    $ _chainResLogger cr
            let hsh = _blockHash bh
            let h = _blockHeight bh
            logCr Info $ "pact db synchronizing to block "
                <> T.pack (show (h, hsh))
            void $ _pactSyncToBlock pact bh
            logCr Info "pact db synchronized"

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

-- | Starts server and runs all network clients
--
runChainweb
    :: forall logger tbl
    . Logger logger
    => CanReadablePayloadCas tbl
    => Chainweb logger tbl
    -> IO ()
runChainweb cw = do
    logg Info "start chainweb node"
    mkValidationMiddleware <- interleaveIO $
        OpenAPIValidation.mkValidationMiddleware (_chainwebLogger cw) (_chainwebVersion cw) (_chainwebManager cw)
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

        -- 1. Start serving Rest API
        [ (if tls then serve else servePlain)
            $ httpLog
            . throttle (_chainwebPutPeerThrottler cw)
            . throttle (_chainwebMempoolThrottler cw)
            . throttle (_chainwebThrottler cw)
            . p2pRequestSizeLimit
            . p2pValidationMiddleware

        -- 2. Start Clients (with a delay of 500ms)
        , threadDelay 500000 >> clients

        -- 3. Start serving local API
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
        mpClients <- mempoolSyncClients
        concurrentlies_ $ concat
            [ miner
            , cutNetworks mgr (_chainwebCutResources cw)
            , mpClients
            ]

    logg :: LogFunctionText
    logg = logFunctionText $ _chainwebLogger cw

    -- chains
    chains :: [(ChainId, ChainResources logger)]
    chains = HM.toList (_chainwebChains cw)

    chainVals :: [ChainResources logger]
    chainVals = map snd chains

    -- collect server resources
    proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
    proj f = chains & mapped . _2 %~ f

    chainDbsToServe :: [(ChainId, BlockHeaderDb)]
    chainDbsToServe = proj _chainResBlockHeaderDb

    mempoolsToServe :: [(ChainId, Mempool.MempoolBackend ChainwebTransaction)]
    mempoolsToServe = proj _chainResMempool

    peerDb = _peerResDb (_chainwebPeer cw)

    memP2pToServe :: [(NetworkId, PeerDb)]
    memP2pToServe = (\(i, _) -> (MempoolNetwork i, peerDb)) <$> chains

    payloadDbsToServe :: [(ChainId, PayloadDb tbl)]
    payloadDbsToServe = itoList (view chainwebPayloadDb cw <$ _chainwebChains cw)

    pactDbsToServe :: [(ChainId, PactServerData logger tbl)]
    pactDbsToServe = _chainwebPactData cw

    -- P2P Server

    serverSettings :: Counter "clientClosedConnections" -> Settings
    serverSettings closedConnectionsCounter = setOnException
        (\r e -> if
            | Just InsecureConnectionDenied <- fromException e ->
                return ()
            | Just ClientClosedConnectionPrematurely <- fromException e ->
                inc closedConnectionsCounter
            | otherwise ->
                when (defaultShouldDisplayException e) $
                    logg Warn $ loggServerError r e
        ) $ peerServerSettings (_peerResPeer $ _chainwebPeer cw)

    monitorConnectionsClosedByClient :: Counter "clientClosedConnections" -> IO ()
    monitorConnectionsClosedByClient clientClosedConnectionsCounter =
        runForever logg "ConnectionClosedByClient.counter" $ do
            approximateThreadDelay 60000000 {- 1 minute -}
            logFunctionCounter (_chainwebLogger cw) Info . (:[]) =<<
                roll clientClosedConnectionsCounter

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
                ChainwebServerDbs
                    { _chainwebServerCutDb = Just cutDb
                    , _chainwebServerBlockHeaderDbs = chainDbsToServe
                    , _chainwebServerMempools = mempoolsToServe
                    , _chainwebServerPayloadDbs = payloadDbsToServe
                    , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : memP2pToServe
                    }
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
                ChainwebServerDbs
                    { _chainwebServerCutDb = Just cutDb
                    , _chainwebServerBlockHeaderDbs = chainDbsToServe
                    , _chainwebServerMempools = mempoolsToServe
                    , _chainwebServerPayloadDbs = payloadDbsToServe
                    , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : memP2pToServe
                    }
                mw)
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

    loggServerError (Just r) e = "HTTP server error: " <> sshow e <> ". Request: " <> sshow r
    loggServerError Nothing e = "HTTP server error: " <> sshow e

    -- Service API Server

    serviceApiServerSettings :: Port -> HostPreference -> Settings
    serviceApiServerSettings port interface = defaultSettings
        & setPort (int port)
        & setHost interface
        & setOnException
            (\r e -> when (defaultShouldDisplayException e) (logg Warn $ loggServiceApiServerError r e))

    serviceApiHost = _serviceApiConfigInterface $ _configServiceApi $ _chainwebConfig cw

    backupApiEnabled = _enableConfigEnabled $ _configBackupApi $ _configBackup $ _chainwebConfig cw

    serveServiceApi :: Middleware -> IO ()
    serveServiceApi = serveServiceApiSocket
        (serviceApiServerSettings (fst $ _chainwebServiceSocket cw) serviceApiHost)
        (snd $ _chainwebServiceSocket cw)
        (_chainwebVersion cw)
        ChainwebServerDbs
            { _chainwebServerCutDb = Just cutDb
            , _chainwebServerBlockHeaderDbs = chainDbsToServe
            , _chainwebServerMempools = mempoolsToServe
            , _chainwebServerPayloadDbs = payloadDbsToServe
            , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : memP2pToServe
            }
        pactDbsToServe
        (_chainwebCoordinator cw)
        (HeaderStream . _configHeaderStream $ _chainwebConfig cw)
        (Rosetta . _configRosetta $ _chainwebConfig cw)
        (_chainwebBackup cw <$ guard backupApiEnabled)
        (_serviceApiPayloadBatchLimit . _configServiceApi $ _chainwebConfig cw)

    serviceHttpLog :: Middleware
    serviceHttpLog = requestResponseLogger $ setComponent "http:service-api" (_chainwebLogger cw)

    loggServiceApiServerError (Just r) e = "HTTP service API server error: " <> sshow e <> ". Request: " <> sshow r
    loggServiceApiServerError Nothing e = "HTTP service API server error: " <> sshow e

    -- HTTP Request Logger

    -- Cut DB and Miner

    cutDb :: CutDb tbl
    cutDb = _cutResCutDb $ _chainwebCutResources cw

    cutPeerDb :: PeerDb
    cutPeerDb = _peerResDb $ _cutResPeer $ _chainwebCutResources cw

    miner :: [IO ()]
    miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw

    -- Configure Clients

    mgr :: HTTP.Manager
    mgr = view chainwebManager cw

    -- Mempool

    mempoolP2pConfig :: EnableConfig MempoolP2pConfig
    mempoolP2pConfig = _configMempoolP2p $ _chainwebConfig cw

    -- | Decide whether to enable the mempool sync clients.
    --
    mempoolSyncClients :: IO [IO ()]
    mempoolSyncClients = case enabledConfig mempoolP2pConfig of
        Nothing -> disabled
        Just c
            | cw ^. chainwebVersion . versionDefaults . disableMempoolSync -> disabled
            | otherwise -> enabled c
      where
        disabled = do
            logg Info "Mempool p2p sync disabled"
            return []
        enabled conf = do
            logg Info "Mempool p2p sync enabled"
            return $ map (runMempoolSyncClient mgr conf (_chainwebPeer cw)) chainVals
