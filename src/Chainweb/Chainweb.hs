{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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

-- * Configuration
, ChainwebConfiguration(..)
, configNodeId
, configChainwebVersion
, configMiner
, configCoordinator
, configHeaderStream
, configReintroTxs
, configP2p
, configTransactionIndex
, configBlockGasLimit
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
, chainwebConfig

-- ** Mempool integration
, ChainwebTransaction
, Mempool.chainwebTransactionConfig
, validatingMempoolConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Configuration.Utils hiding (Error, Lens', disabled, (<.>))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Error.Util (note)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch (throwM)

import Data.Align (alignWith)
import qualified Data.ByteString.Short as SB
import Data.CAS (casLookupM)
import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
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

import Numeric.Natural

import Prelude hiding (log)

import System.Clock
import System.LogLevel

-- internal modules

import qualified Pact.Types.ChainId as P
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P

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
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configNodeId :: !NodeId
    , _configMiner :: !(EnableConfig MinerConfig)
    , _configCoordinator :: !Bool
    , _configHeaderStream :: !Bool
    , _configReintroTxs :: !Bool
    , _configP2p :: !P2pConfiguration
    , _configTransactionIndex :: !(EnableConfig TransactionIndexConfig)
    , _configIncludeOrigin :: !Bool
    , _configThrottleRate :: !Natural
    , _configMempoolP2p :: !(EnableConfig MempoolP2pConfig)
    , _configPruneChainDatabase :: !Bool
    , _configBlockGasLimit :: !Mempool.GasLimit
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ChainwebConfiguration where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

validateChainwebConfiguration :: ConfigValidation ChainwebConfiguration l
validateChainwebConfiguration c = do
    validateEnableConfig validateMinerConfig (_configMiner c)

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configNodeId = NodeId 0 -- FIXME
    , _configMiner = EnableConfig False defaultMinerConfig
    , _configCoordinator = False
    , _configHeaderStream = False
    , _configReintroTxs = True
    , _configP2p = defaultP2pConfiguration
    , _configTransactionIndex = defaultEnableConfig defaultTransactionIndexConfig
    , _configIncludeOrigin = True
    , _configThrottleRate = 1000
    , _configMempoolP2p = defaultEnableConfig defaultMempoolP2pConfig
    , _configPruneChainDatabase = True
    , _configBlockGasLimit = 100000
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "miner" .= _configMiner o
        , "miningCoordination" .= _configCoordinator o
        , "headerStream" .= _configHeaderStream o
        , "reintroTxs" .= _configReintroTxs o
        , "p2p" .= _configP2p o
        , "transactionIndex" .= _configTransactionIndex o
        , "includeOrigin" .= _configIncludeOrigin o
        , "throttleRate" .= _configThrottleRate o
        , "mempoolP2p" .= _configMempoolP2p o
        , "pruneChainDatabase" .= _configPruneChainDatabase o
        , "blockGasLimit" .= _configBlockGasLimit o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configCoordinator ..: "miningCoordination" % o
        <*< configHeaderStream ..: "headerStream" % o
        <*< configReintroTxs ..: "reintroTxs" % o
        <*< configP2p %.: "p2p" % o
        <*< configTransactionIndex %.: "transactionIndex" % o
        <*< configIncludeOrigin ..: "includeOrigin" % o
        <*< configThrottleRate ..: "throttleRate" % o
        <*< configMempoolP2p %.: "mempoolP2p" % o
        <*< configPruneChainDatabase ..: "pruneChainDatabase" % o
        <*< configBlockGasLimit ..: "blockGasLimit" % o

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
    <*< configMiner %:: pEnableConfig "mining" pMinerConfig
    <*< configCoordinator .:: boolOption_
        % long "mining-coordination"
        <> help "whether to enable external requests for mining work"
    <*< configHeaderStream .:: boolOption_
        % long "header-stream"
        <> help "whether to enable an endpoint for streaming block updates"
    <*< configReintroTxs .:: enableDisableFlag
        % long "tx-reintro"
        <> help "whether to enable transaction reintroduction from losing forks"
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configTransactionIndex %::
        pEnableConfig "transaction-index" pTransactionIndexConfig
    <*< configIncludeOrigin .:: enableDisableFlag
        % long "include-origin"
        <> help "whether to include the local peer as origin when publishing cut hashes"
    <*< configThrottleRate .:: option auto
        % long "throttle-rate"
        <> help "how many requests per second are accepted from another node before it is being throttled"
    <*< configMempoolP2p %::
        pEnableConfig "mempool-p2p" pMempoolP2pConfig
    <*< configPruneChainDatabase .:: enableDisableFlag
        % long "prune-chain-database"
        <> help "prune the chain database for all chains on startup"
    <*< configBlockGasLimit .:: jsonOption
        % long "block-gas-limit"
        <> help "the sum of all transaction gas fees in a block must not exceed this number"

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

    toDupeResult :: Maybe a -> Maybe Mempool.InsertError
    toDupeResult = fmap (const Mempool.InsertErrorDuplicate)

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
        let hashes = V.map (toPactHash . sfst) txs
        pex <- readMVar mv
        rs <- _pactLookup pex (Left cid) hashes >>= either throwM pure
        pure $ alignWith f rs txs
      where
        f (These r (T2 h t)) = maybe (Right (T2 h t)) (Left . T2 h) $ toDupeResult r
        f (That (T2 h _)) = Left (T2 h $ Mempool.InsertErrorOther "preInsertBatch: align mismatch 0")
        f (This _) = Left (T2 (Mempool.TransactionHash "") (Mempool.InsertErrorOther "preInsertBatch: align mismatch 1"))

    toPactHash :: Mempool.TransactionHash -> P.TypedHash h
    toPactHash (Mempool.TransactionHash h) = P.TypedHash $ SB.fromShort h

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
    cdbv <- newEmptyMVar
    concurrentWith
        -- initialize chains concurrently
        (\cid -> do
            let mcfg = validatingMempoolConfig cid v (_configBlockGasLimit conf)
            withChainResources v cid rocksDb peer (chainLogger cid)
                     mcfg cdbv payloadDb prune dbDir nodeid
                     resetDb)

        -- initialize global resources after all chain resources are initialized
        (\cs -> global (HM.fromList $ zip cidsList cs) cdbv)
        cidsList
  where
    prune :: Bool
    prune = _configPruneChainDatabase conf

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
        -> MVar (CutDb RocksDbCas)
        -> IO a
    global cs cdbv = do
        let !webchain = mkWebBlockHeaderDb v (HM.map _chainResBlockHeaderDb cs)
            !pact = mkWebPactExecutionService (HM.map _chainResPact cs)
            !cutLogger = setComponent "cut" logger
            !mgr = _peerResManager peer

        logg Info "start initializing cut resources"

        withCutResources cutConfig peer cutLogger rocksDb webchain payloadDb mgr pact $ \cuts -> do
            logg Info "finished initializing cut resources"

            let !mLogger = setComponent "miner" logger
                !mConf = _configMiner conf
                !mCutDb = _cutResCutDb cuts

            -- initialize throttler
            throttler <- initThrottler (defaultThrottleSettings $ TimeSpec 4 0)
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

            -- update the cutdb mvar used by pact service with cutdb
            void $! putMVar cdbv mCutDb

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
                withMiningCoordination mLogger (_configCoordinator conf) mCutDb $ \mc ->
                    withMinerResources mLogger mConf mCutDb $ \m -> do
                        logg Info "finished initializing miner resources"
                        inner Chainweb
                            { _chainwebHostAddress = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
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
    cutConfig :: CutDbConfig
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        , _cutDbConfigUseOrigin = _configIncludeOrigin conf
        }

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
        (serve (throttle (_chainwebThrottler cw) . httpLog))
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

    -- | KILLSWITCH: The logic here involving `txSilenceEndDate` here is to be
    -- removed in a future version of Chain. This disables the Mempool API
    -- entirely during the TX blackout period.
    --
    mempoolsToServe
        :: ChainwebVersion
        -> [(ChainId, Mempool.MempoolBackend ChainwebTransaction)]
    mempoolsToServe v = case txSilenceEndDate v of
        Just _ -> []
        _ -> proj _chainResMempool

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
        (\r e -> when (defaultShouldDisplayException e) (logg Error $ loggServerError r e))
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
            , _chainwebServerMempools = mempoolsToServe (_chainwebVersion cw)
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

    -- Decide whether to enable the mempool sync clients
    -- | KILLSWITCH: Reenable the mempool sync for Mainnet.
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
            Testnet02 -> enabled c
            Mainnet01 -> disabled
      where
        disabled = do
            logg Info "Mempool p2p sync disabled"
            return []
        enabled conf = do
            logg Info "Mempool p2p sync enabled"
            return $ map (runMempoolSyncClient mgr conf) chainVals
