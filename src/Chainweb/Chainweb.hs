{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- * Pact Configuration
  TransactionIndexConfig(..)
, defaultTransactionIndexConfig
, pTransactionIndexConfig

-- * Chainweb Configuration
, ChainwebConfiguration(..)
, configNodeId
, configChainwebVersion
, configMining
, configHeaderStream
, configReintroTxs
, configP2p
, configTransactionIndex
, configBlockGasLimit
, configThrottling
, defaultChainwebConfiguration
, pChainwebConfiguration
, validateChainwebConfiguration

-- * Chainweb Resources
, Chainweb(..)
, chainwebChains
, chainwebCutResources
, chainwebHostAddress
, chainwebMiner
, chainwebCoordinator
, chainwebHeaderStream
, chainwebLogger
, chainwebSocket
, chainwebPeer
, chainwebPayloadDb
, chainwebPactData
, chainwebThrottler
, chainwebMiningThrottler
, chainwebPutPeerThrottler
, chainwebLocalThrottler
, chainwebConfig

-- ** Mempool integration
, ChainwebTransaction
, Mempool.chainwebTransactionConfig
, validatingMempoolConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

-- * Throttler
, mkGenericThrottler
, mkMiningThrottler
, mkPutPeerThrottler
, mkLocalThrottler
, checkPathPrefix
, mkThrottler

, ThrottlingConfig(..)
, throttlingRate
, throttlingLocalRate
, throttlingMiningRate
, throttlingPeerRate
, defaultThrottlingConfig

-- * Cut Config
, CutConfig(..)
, cutIncludeOrigin
, cutPruneChainDatabase
, cutFetchTimeout
, cutInitialCutHeightLimit
, defaultCutConfig
) where

import Configuration.Utils hiding (Error, Lens', disabled, (<.>))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Error.Util (note)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (throwM)

import Data.Align (alignWith)
import Data.CAS (casLookupM)
import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (isPrefixOf, sortBy)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.These (These(..))
import Data.Tuple.Strict (T2(..))
import qualified Data.Vector as V

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Throttle

import Numeric.Natural (Natural)

import Prelude hiding (log)

import System.Clock
import System.LogLevel

-- internal modules

import qualified Pact.Types.ChainId as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI (HeaderStream(..))
import Chainweb.ChainId
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Config
import Chainweb.NodeId
import Chainweb.Pact.RestAPI.Server (PactServerData)
import Chainweb.Pact.Utils (fromPactChainId)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.CAS.RocksDB
import Data.LogMessage (LogFunctionText)

import P2P.Node.Configuration
import P2P.Node.PeerDB (PeerDb)
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- TransactionIndexConfig

data TransactionIndexConfig = TransactionIndexConfig
    deriving (Show, Eq, Generic)

makeLenses ''TransactionIndexConfig

defaultTransactionIndexConfig :: TransactionIndexConfig
defaultTransactionIndexConfig = TransactionIndexConfig

instance ToJSON TransactionIndexConfig where
    toJSON _ = object []

instance FromJSON (TransactionIndexConfig -> TransactionIndexConfig) where
    parseJSON = withObject "TransactionIndexConfig" $ const (return id)

pTransactionIndexConfig :: MParser TransactionIndexConfig
pTransactionIndexConfig = pure id

-- -------------------------------------------------------------------------- --
-- Throttling Configuration

data ThrottlingConfig = ThrottlingConfig
    { _throttlingRate :: !Double
    , _throttlingMiningRate :: !Double
        -- ^ The rate should be sufficient to make at least on call per cut. We
        -- expect an cut to arrive every few seconds.
        --
        -- Default is 10 per second.
    , _throttlingPeerRate :: !Double
        -- ^ This should throttle aggressively. This endpoint does an expensive
        -- check of the client. And we want to keep bad actors out of the
        -- system. There should be no need for a client to call this endpoint on
        -- the same node more often than at most few times peer minute.
        --
        -- Default is 1 per second
    , _throttlingLocalRate :: !Double
    }
    deriving stock (Eq, Show)

makeLenses ''ThrottlingConfig

defaultThrottlingConfig :: ThrottlingConfig
defaultThrottlingConfig = ThrottlingConfig
    { _throttlingRate = 50 -- per second
    , _throttlingMiningRate = 5 --  per second
    , _throttlingPeerRate = 11 -- per second, one for each p2p network
    , _throttlingLocalRate = 0.1  -- per 10 seconds
    }

instance ToJSON ThrottlingConfig where
    toJSON o = object
        [ "global" .= _throttlingRate o
        , "mining" .= _throttlingMiningRate o
        , "putPeer" .= _throttlingPeerRate o
        , "local" .= _throttlingLocalRate o
        ]

instance FromJSON (ThrottlingConfig -> ThrottlingConfig) where
    parseJSON = withObject "ThrottlingConfig" $ \o -> id
        <$< throttlingRate ..: "global" % o
        <*< throttlingMiningRate ..: "mining" % o
        <*< throttlingPeerRate ..: "putPeer" % o
        <*< throttlingLocalRate ..: "local" % o

--

data CutConfig = CutConfig
    { _cutIncludeOrigin :: !Bool
    , _cutPruneChainDatabase :: !Bool
    , _cutFetchTimeout :: !Int
    , _cutInitialCutHeightLimit :: !(Maybe BlockHeight)
    } deriving (Eq, Show)

makeLenses ''CutConfig

instance ToJSON CutConfig where
    toJSON o = object
        [ "pruneChainDatabase" .= _cutPruneChainDatabase o
        , "fetchTimeout" .= _cutFetchTimeout o
        , "initialCutHeightLimit" .= _cutInitialCutHeightLimit o ]

instance FromJSON (CutConfig -> CutConfig) where
    parseJSON = withObject "CutConfig" $ \o -> id
        <$< cutIncludeOrigin ..: "includeOrigin" % o
        <*< cutPruneChainDatabase ..: "pruneChainDatabase" % o
        <*< cutFetchTimeout ..: "fetchTimeout" % o
        <*< cutInitialCutHeightLimit ..: "initialCutHeightLimit" % o

defaultCutConfig :: CutConfig
defaultCutConfig = CutConfig
    { _cutIncludeOrigin = True
    , _cutPruneChainDatabase = True
    , _cutFetchTimeout = 3_000_000
    , _cutInitialCutHeightLimit = Nothing }

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configNodeId :: !NodeId
    , _configCuts :: !CutConfig
    , _configMining :: !MiningConfig
    , _configHeaderStream :: !Bool
    , _configReintroTxs :: !Bool
    , _configP2p :: !P2pConfiguration
    , _configTransactionIndex :: !(EnableConfig TransactionIndexConfig)
    , _configThrottling :: !ThrottlingConfig
    , _configMempoolP2p :: !(EnableConfig MempoolP2pConfig)
    , _configBlockGasLimit :: !Mempool.GasLimit
    , _configPactQueueSize :: !Natural
    } deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ChainwebConfiguration where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration l
validateChainwebConfiguration c = do
    validateMinerConfig (_configMining c)

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configNodeId = NodeId 0 -- FIXME
    , _configCuts = defaultCutConfig
    , _configMining = defaultMining
    , _configHeaderStream = False
    , _configReintroTxs = True
    , _configP2p = defaultP2pConfiguration
    , _configTransactionIndex = defaultEnableConfig defaultTransactionIndexConfig
    , _configThrottling = defaultThrottlingConfig
    , _configMempoolP2p = defaultEnableConfig defaultMempoolP2pConfig
    , _configBlockGasLimit = 6000
    , _configPactQueueSize = 2000
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "cuts" .= _configCuts o
        , "mining" .= _configMining o
        , "headerStream" .= _configHeaderStream o
        , "reintroTxs" .= _configReintroTxs o
        , "p2p" .= _configP2p o
        , "transactionIndex" .= _configTransactionIndex o
        , "throttling" .= _configThrottling o
        , "mempoolP2p" .= _configMempoolP2p o
        , "gasLimitOfBlock" .= _configBlockGasLimit o
        , "pactQueueSize" .= _configPactQueueSize o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configCuts %.: "cuts" % o
        <*< configMining %.: "mining" % o
        <*< configHeaderStream ..: "headerStream" % o
        <*< configReintroTxs ..: "reintroTxs" % o
        <*< configP2p %.: "p2p" % o
        <*< configTransactionIndex %.: "transactionIndex" % o
        <*< configThrottling %.: "throttling" % o
        <*< configMempoolP2p %.: "mempoolP2p" % o
        <*< configBlockGasLimit ..: "gasLimitOfBlock" % o
        <*< configPactQueueSize ..: "pactQueueSize" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion .:: textOption
        % long "chainweb-version"
        <> short 'v'
        <> help "the chainweb version that this node is using"
    <*< configNodeId .:: textOption
        % long "node-id"
        <> short 'i'
        <> help "unique id of the node that is used as miner id in new blocks"
    <*< configHeaderStream .:: boolOption_
        % long "header-stream"
        <> help "whether to enable an endpoint for streaming block updates"
    <*< configReintroTxs .:: enableDisableFlag
        % long "tx-reintro"
        <> help "whether to enable transaction reintroduction from losing forks"
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configTransactionIndex %::
        pEnableConfig "transaction-index" pTransactionIndexConfig
    <*< configMempoolP2p %::
        pEnableConfig "mempool-p2p" pMempoolP2pConfig
    <*< configBlockGasLimit .:: jsonOption
        % long "block-gas-limit"
        <> help "the sum of all transaction gas fees in a block must not exceed this number"
    <*< configPactQueueSize .:: jsonOption
        % long "pact-queue-size"
        <> help "max size of pact internal queue"

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb logger cas = Chainweb
    { _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId (ChainResources logger))
    , _chainwebCutResources :: !(CutResources logger cas)
    , _chainwebMiner :: !(Maybe (MinerResources logger cas))
    , _chainwebCoordinator :: !(Maybe (MiningCoordination logger cas))
    , _chainwebHeaderStream :: !HeaderStream
    , _chainwebLogger :: !logger
    , _chainwebPeer :: !(PeerResources logger)
    , _chainwebPayloadDb :: !(PayloadDb cas)
    , _chainwebManager :: !HTTP.Manager
    , _chainwebPactData :: [(ChainId, PactServerData logger cas)]
    , _chainwebThrottler :: !(Throttle Address)
    , _chainwebMiningThrottler :: !(Throttle Address)
    , _chainwebPutPeerThrottler :: !(Throttle Address)
    , _chainwebLocalThrottler :: !(Throttle Address)
    , _chainwebConfig :: !ChainwebConfiguration
    }

makeLenses ''Chainweb

chainwebSocket :: Getter (Chainweb logger cas) Socket
chainwebSocket = chainwebPeer . peerResSocket

instance HasChainwebVersion (Chainweb logger cas) where
    _chainwebVersion = _chainwebVersion . _chainwebCutResources
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph (Chainweb logger cas) where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> Maybe FilePath
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainweb c logger rocksDb dbDir resetDb inner =
    withPeerResources v (view configP2p conf) logger $ \logger' peer ->
        withChainwebInternal
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
    conf | _p2pConfigIgnoreBootstrapNodes (_configP2p c) = c
         | otherwise = configP2p . p2pConfigKnownPeers <>~ bootstrapPeerInfos v $ c

-- TODO: The type InMempoolConfig contains parameters that should be
-- configurable as well as parameters that are determined by the chainweb
-- version or the chainweb protocol. These should be separated in to two
-- different types.

validatingMempoolConfig
    :: ChainId
    -> ChainwebVersion
    -> Mempool.GasLimit
    -> MVar PactExecutionService
    -> Mempool.InMemConfig ChainwebTransaction
validatingMempoolConfig cid v gl mv = Mempool.InMemConfig
    { Mempool._inmemTxCfg = txcfg
    , Mempool._inmemTxBlockSizeLimit = gl
    , Mempool._inmemMaxRecentItems = maxRecentLog
    , Mempool._inmemPreInsertPureChecks = preInsertSingle
    , Mempool._inmemPreInsertBatchChecks = preInsertBatch
    }
  where
    txcfg = Mempool.chainwebTransactionConfig
    maxRecentLog = 2048

    preInsertSingle :: ChainwebTransaction -> Either Mempool.InsertError ChainwebTransaction
    preInsertSingle tx = checkMetadata tx

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
        pure $ alignWith f rs txs
      where
        f (These r (T2 h t)) = case r of
                                 Left e -> Left (T2 h e)
                                 Right _ -> Right (T2 h t)
        f (That (T2 h _)) = Left (T2 h $ Mempool.InsertErrorOther "preInsertBatch: align mismatch 0")
        f (This _) = Left (T2 (Mempool.TransactionHash "") (Mempool.InsertErrorOther "preInsertBatch: align mismatch 1"))

    -- | Validation: Is this TX associated with the correct `ChainId`?
    --
    checkMetadata :: ChainwebTransaction -> Either Mempool.InsertError ChainwebTransaction
    checkMetadata tx = do
        let !pay = payloadObj . P._cmdPayload $ tx
            pcid = P._pmChainId $ P._pMeta pay
            sigs = length (P._cmdSigs tx)
            ver  = P._pNetworkId pay >>= fromText @ChainwebVersion . P._networkId
        tcid <- note (Mempool.InsertErrorOther "Unparsable ChainId") $ fromPactChainId pcid
        if | tcid /= cid   -> Left Mempool.InsertErrorMetadataMismatch
           | sigs > 100    -> Left $ Mempool.InsertErrorOther "Too many signatures"
           | ver /= Just v -> Left Mempool.InsertErrorMetadataMismatch
           | otherwise     -> Right tx

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainwebInternal
    :: forall logger a
    .  Logger logger
    => ChainwebConfiguration
    -> logger
    -> PeerResources logger
    -> RocksDb
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebInternal conf logger peer rocksDb dbDir nodeid resetDb inner = do
    initializePayloadDb v payloadDb
    concurrentWith
        -- initialize chains concurrently
        (\cid -> do
            let mcfg = validatingMempoolConfig cid v (_configBlockGasLimit conf)
            withChainResources v cid rocksDb peer (chainLogger cid)
                     mcfg payloadDb prune dbDir nodeid
                     resetDb (_configPactQueueSize conf))

        -- initialize global resources after all chain resources are initialized
        (\cs -> global (HM.fromList $ zip cidsList cs))
        cidsList
  where
    prune :: Bool
    prune = _cutPruneChainDatabase $ _configCuts conf

    cidsList :: [ChainId]
    cidsList = toList cids

    payloadDb :: PayloadDb RocksDbCas
    payloadDb = newPayloadDb rocksDb

    chainLogger :: ChainId -> logger
    chainLogger cid = addLabel ("chain", toText cid) logger

    logg :: LogFunctionText
    logg = logFunctionText logger

    -- Initialize global resources
    global
        :: HM.HashMap ChainId (ChainResources logger)
        -> IO a
    global cs = do
        let !webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            !pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            !cutLogger = setComponent "cut" logger
            !mgr = _peerResManager peer

        logg Info "start initializing cut resources"

        withCutResources cutConfig peer cutLogger rocksDb webchain payloadDb mgr pact $ \cuts -> do
            logg Info "finished initializing cut resources"

            let !mLogger = setComponent "miner" logger
                !mConf = _configMining conf
                !mCutDb = _cutResCutDb cuts
                !throt  = _configThrottling conf

            -- initialize throttler
            throttler <- mkGenericThrottler $ _throttlingRate throt
            miningThrottler <- mkMiningThrottler $ _throttlingMiningRate throt
            putPeerThrottler <- mkPutPeerThrottler $ _throttlingPeerRate throt
            localThrottler <- mkLocalThrottler $ _throttlingLocalRate throt

            -- synchronize pact dbs with latest cut before we start the server
            -- and clients and begin mining.
            --
            -- This is a consistency check that validates the blocks in the
            -- current cut. If it fails in exception is raised. Also, if it
            -- takes long (why would it?) we want this to happen before we go
            -- online.
            --
            logg Info "start synchronizing Pact DBs"
            synchronizePactDb cs mCutDb
            logg Info "finished synchronizing Pact DBs"

            withPactData cs cuts $ \pactData -> do
                logg Info "start initializing miner resources"
                withMiningCoordination mLogger (_miningCoordination mConf) mCutDb $ \mc ->
                    withMinerResources mLogger (_miningInNode mConf) cs mCutDb $ \m -> do
                        logg Info "finished initializing miner resources"
                        let !haddr = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                        inner Chainweb
                            { _chainwebHostAddress = haddr
                            , _chainwebChains = cs
                            , _chainwebCutResources = cuts
                            , _chainwebMiner = m
                            , _chainwebCoordinator = mc
                            , _chainwebHeaderStream = HeaderStream $ _configHeaderStream conf
                            , _chainwebLogger = logger
                            , _chainwebPeer = peer
                            , _chainwebPayloadDb = payloadDb
                            , _chainwebManager = mgr
                            , _chainwebPactData = pactData
                            , _chainwebThrottler = throttler
                            , _chainwebMiningThrottler = miningThrottler
                            , _chainwebPutPeerThrottler = putPeerThrottler
                            , _chainwebLocalThrottler = localThrottler
                            , _chainwebConfig = conf
                            }

    withPactData
        :: HM.HashMap ChainId (ChainResources logger)
        -> CutResources logger cas
        -> ([(ChainId, (CutResources logger cas, ChainResources logger))] -> IO b)
        -> IO b
    withPactData cs cuts m
        | _enableConfigEnabled (_configTransactionIndex conf) = do
              -- TODO: delete this knob
              logg Info "Transaction index enabled"
              let l = sortBy (compare `on` fst) (HM.toList cs)
              m $ map (\(c, cr) -> (c, (cuts, cr))) l
        | otherwise = do
              logg Info "Transaction index disabled"
              m []

    v = _configChainwebVersion conf
    cids = chainIds v

    -- FIXME: make this configurable
    cutConfig :: CutDbParams
    cutConfig = (defaultCutDbParams v $ _cutFetchTimeout cutConf)
        { _cutDbParamsLogLevel = Info
        , _cutDbParamsTelemetryLevel = Info
        , _cutDbParamsUseOrigin = _cutIncludeOrigin cutConf
        , _cutDbParamsInitialHeightLimit = _cutInitialCutHeightLimit $ cutConf }
      where
        cutConf = _configCuts conf

    synchronizePactDb :: HM.HashMap ChainId (ChainResources logger) -> CutDb cas -> IO ()
    synchronizePactDb cs cutDb = do
        currentCut <- _cut cutDb
        mapConcurrently_ syncOne $ mergeCutResources $ _cutMap currentCut
      where
        mergeCutResources :: HM.HashMap ChainId b -> [(b, ChainResources logger)]
        mergeCutResources c =
            let f cid bh = (bh, fromJuste $ HM.lookup cid cs)
            in map snd $ HM.toList $ HM.mapWithKey f c

        syncOne :: (BlockHeader, ChainResources logger) -> IO ()
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

-- -------------------------------------------------------------------------- --
-- Throttling

mkGenericThrottler :: Double -> IO (Throttle Address)
mkGenericThrottler rate = mkThrottler 5 rate (const True)

mkMiningThrottler :: Double -> IO (Throttle Address)
mkMiningThrottler rate = mkThrottler 5 rate (checkPathPrefix ["mining", "work"])

mkPutPeerThrottler :: Double -> IO (Throttle Address)
mkPutPeerThrottler rate = mkThrottler 5 rate $ \r ->
    elem "peer" (pathInfo r) && requestMethod r == "PUT"

mkLocalThrottler :: Double -> IO (Throttle Address)
mkLocalThrottler rate = mkThrottler 5 rate (checkPathPrefix path)
  where
    path = ["pact", "api", "v1", "local"]

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
    , throttleSettingsBurst = 2 * ceiling rate
    , throttleSettingsIsThrottled = c
    }

-- -------------------------------------------------------------------------- --
-- Run Chainweb

-- | Starts server and runs all network clients
--
runChainweb
    :: forall logger cas
    . Logger logger
    => PayloadCas cas
    => Chainweb logger cas
    -> IO ()
runChainweb cw = do
    logg Info "start chainweb node"
    concurrently_
        -- 1. Start serving Rest API
        (serve
            $ httpLog
            . throttle (_chainwebPutPeerThrottler cw)
            . throttle (_chainwebMiningThrottler cw)
            . throttle (_chainwebLocalThrottler cw)
            . throttle (_chainwebThrottler cw)
        )
        -- 2. Start Clients (with a delay of 500ms)
        (threadDelay 500000 >> clients)
  where
    clients :: IO ()
    clients = do
        mpClients <- mempoolSyncClients
        mapConcurrently_ id $ concat
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
    proj f = flip map chains $ \(k, ch) -> (k, f ch)

    chainDbsToServe :: [(ChainId, BlockHeaderDb)]
    chainDbsToServe = proj _chainResBlockHeaderDb

    mempoolsToServe :: [(ChainId, Mempool.MempoolBackend ChainwebTransaction)]
    mempoolsToServe = proj _chainResMempool

    chainP2pToServe :: [(NetworkId, PeerDb)]
    chainP2pToServe =
        bimap ChainNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

    memP2pToServe :: [(NetworkId, PeerDb)]
    memP2pToServe =
        bimap MempoolNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

    payloadDbsToServe :: [(ChainId, PayloadDb cas)]
    payloadDbsToServe = itoList (view chainwebPayloadDb cw <$ _chainwebChains cw)

    pactDbsToServe :: [(ChainId, PactServerData logger cas)]
    pactDbsToServe = _chainwebPactData cw

    serverSettings :: Settings
    serverSettings = setOnException
        (\r e -> when (defaultShouldDisplayException e) (logg Warn $ loggServerError r e))
        $ peerServerSettings (_peerResPeer $ _chainwebPeer cw)

    serve :: Middleware -> IO ()
    serve = serveChainwebSocketTls
        serverSettings
        (_peerCertificateChain $ _peerResPeer $ _chainwebPeer cw)
        (_peerKey $ _peerResPeer $ _chainwebPeer cw)
        (_peerResSocket $ _chainwebPeer cw)
        (_chainwebVersion cw)
        ChainwebServerDbs
            { _chainwebServerCutDb = Just cutDb
            , _chainwebServerBlockHeaderDbs = chainDbsToServe
            , _chainwebServerMempools = mempoolsToServe
            , _chainwebServerPayloadDbs = payloadDbsToServe
            , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : chainP2pToServe <> memP2pToServe
            , _chainwebServerPactDbs = pactDbsToServe
            }
        (_chainwebCoordinator cw)
        (_chainwebHeaderStream cw)

    -- HTTP Request Logger

    httpLog :: Middleware
    httpLog = requestResponseLogger $ setComponent "http" (_chainwebLogger cw)

    loggServerError (Just r) e = "HTTP server error: " <> sshow e <> ". Request: " <> sshow r
    loggServerError Nothing e = "HTTP server error: " <> sshow e

    -- Cut DB and Miner

    cutDb :: CutDb cas
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
        Just c -> case _chainwebVersion cw of
            Test{} -> disabled
            TimedConsensus{} -> disabled
            PowConsensus{} -> disabled
            TimedCPM{} -> enabled c
            FastTimedCPM{} -> enabled c
            Development -> enabled c
            Testnet04 -> enabled c
            Mainnet01 -> enabled c
      where
        disabled = do
            logg Info "Mempool p2p sync disabled"
            return []
        enabled conf = do
            logg Info "Mempool p2p sync enabled"
            return $ map (runMempoolSyncClient mgr conf) chainVals
