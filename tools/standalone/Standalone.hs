{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Standalone where

import Configuration.Utils hiding (Error, disabled)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed

import Data.ByteString (ByteString)
import Data.CAS
import Data.CAS.RocksDB
import Data.Default
import Data.Foldable
import Data.Function
import Data.List
import Data.LogMessage
import Data.PQueue
import Data.Text (Text)
import Data.Typeable
import qualified  Data.HashMap.Strict as HM
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SB
import qualified Data.TaskMap as TM
import qualified Data.Text as T
import qualified Data.Vector as Vector

import GHC.Stats
import GHC.Generics

-- import Network.Wai
-- import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Throttle
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Numeric.Natural

import P2P.Peer
import P2P.Node.Configuration

import PkgInfo

import qualified Streaming.Prelude as S

import System.Clock
import System.Directory
import qualified System.Logger as L
import System.LogLevel

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

-- pact imports

import Pact.ApiReq
import qualified Pact.Types.ChainId as PactChain
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Util (toB16Text)

-- chainweb imports
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Counter
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Logging.Amberdata
import Chainweb.Logging.Config
import Chainweb.Logging.Miner
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
-- import Chainweb.RestAPI
-- import Chainweb.RestAPI.NetworkID
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Transaction
import Chainweb.Utils
-- import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService
import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import qualified Chainweb.Pact.BloomCache as Bloom

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

defaultMemPoolAccess :: ChainId -> Int -> MemPoolAccess
defaultMemPoolAccess cid blocksize  = MemPoolAccess
    { mpaGetBlock = \height _hash _prevBlock ->
        makeBlock height cid blocksize ("(+ 1 2)", Nothing)
    , mpaSetLastHeader = const $ return ()
    , mpaProcessFork = const $ return ()
    }
  where
    makeBlock
        :: BlockHeight
        -> ChainId
        -> Int
        -> (Text, Maybe Value)
        -> IO (Vector.Vector ChainwebTransaction)
    makeBlock height cidd n = Vector.replicateM n . go
        where
          go (c, d) = do
              let dd = mergeObjects (toList d)
                  pm = def
                    & set pmSender "sender00"
                    & set pmGasLimit 100
                    & set pmGasPrice 0.1
                    & set pmChainId (PactChain.ChainId (chainIdToText cidd))
                  msg = Exec (ExecMsg c dd)
                  -- TODO: This might need to be something more fleshed out.
                  nonce = T.pack $ show height
              ks <- testKeyPairs
              cmd <- mkCommand ks pm nonce msg
              case verifyCommand cmd of
                ProcSucc t -> return $ fmap (k t) (SB.toShort <$> cmd)
                ProcFail e -> throwM $ userError e

          k t bs = PayloadWithText bs (_cmdPayload t)

    -- | Merge a list of JSON Objects together. Note: this will yield an empty
    -- object in the case that there are no objects in the list of values.
    --
    mergeObjects :: [Value] -> Value
    mergeObjects = Object . HM.unions . foldr unwrap []
      where
        unwrap (Object o) = (:) o
        unwrap _ = id

-- MOVE TO UTILS

testKeyPairs :: IO [SomeKeyPair]
testKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    mkKeyPairs [apiKP]


-- | note this is "sender00"'s key
someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ getByteString
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ getByteString
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

-- MOVE TO UTILS


withChainResourcesStandalone
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> ChainId
    -> RocksDb
    -> PeerResources logger
    -> logger
    -> Mempool.InMemConfig ChainwebTransaction
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
withChainResourcesStandalone v cid rdb peer logger mempoolCfg cdbv payloadDb prune dbDir nodeid resetDb inner =
    withBlockHeaderDb rdb v cid $ \cdb ->
        Mempool.withInMemoryMempool mempoolCfg rdb $ \mempool -> do
            -- placing mempool access shim here
            -- putting a default here for now.
              let mpa = defaultMemPoolAccess cid 1
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
        TimedCPM{} -> mkPactExecutionService' requestQ
        Development -> mkPactExecutionService' requestQ
        -- Testnet00 -> mkPactExecutionService' requestQ
        -- Testnet01 -> mkPactExecutionService' requestQ
        Testnet02 -> mkPactExecutionService' requestQ

mkPactExecutionService' :: TQueue RequestMsg -> PactExecutionService
mkPactExecutionService' q = emptyPactExecutionService
  { _pactValidateBlock = \h pd -> do
      mv <- validateBlock h pd q
      r <- takeMVar mv
      case r of
          (Right !pdo) -> return pdo
          Left e -> throwM e
  , _pactNewBlock = \m h -> do
      mv <- newBlock m h q
      r <- takeMVar mv
      case r of
          (Right !pdo) -> return pdo
          Left e -> throwM e
  }

-- TODO: The type InMempoolConfig contains parameters that should be
-- configurable as well as parameters that are determined by the chainweb
-- version or the chainweb protocol. These should be separated in to two
-- different types.
--
mempoolConfig :: Bool -> Mempool.InMemConfig ChainwebTransaction
mempoolConfig enableReIntro = Mempool.InMemConfig
    Mempool.chainwebTransactionConfig
    blockGasLimit
    mempoolReapInterval
    maxRecentLog
    enableReIntro
  where
    blockGasLimit = 100000               -- TODO: policy decision
    mempoolReapInterval = 60 * 20 * 1000000   -- 20 mins
    maxRecentLog = 2048                   -- store 2k recent transaction hashes


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
      (\cid -> withChainResourcesStandalone v cid rocksDb peer (chainLogger cid)
                mempoolConf cdbv payloadDb prune dbDir nodeid resetDb)

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
    enableTxsReintro = _configReintroTxs conf
    mempoolConf = mempoolConfig enableTxsReintro

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
                    withMinerResources mLogger mConf cwnid mCutDb $ \m -> do
                        logg Info "finished initializing miner resources"
                        inner Chainweb
                                  { _chainwebHostAddress =
                                      _peerConfigAddr
                                      $ _p2pConfigPeer
                                      $ _configP2p conf
                                  , _chainwebChains = cs
                                  , _chainwebCutResources = cuts
                                  , _chainwebNodeId = cwnid
                                  , _chainwebMiner = m
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
                bdbs = map (\(c, cr) -> (c, _chainResBlockHeaderDb cr)) l
            Bloom.withCache (cuts ^. cutsCutDb) bdbs $ \bloom ->
               m $ map (\(c, cr) -> (c, (cuts, cr, bloom))) l
        | otherwise = do
            logg Info "Transaction index disabled"
            m []
    v = _configChainwebVersion conf
    cids = chainIds v
    cwnid = _configNodeId conf

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


-- -------------------------------------------------------------------------- --
-- Monitors

-- | Run a monitor function with a logger forever. If the monitor function exist
-- or fails the event is logged and the function is restarted.
--
-- In order to prevent the function to spin in case of a persistent failure
-- cause, only 10 immediate restart are allowed. After that restart is throttled
-- to at most one restart every 10 seconds.
--
runMonitorLoop :: Logger logger => T.Text -> logger -> IO () -> IO ()
runMonitorLoop label logger = runForeverThrottled
    (logFunction logger)
    label
    10 -- 10 bursts in case of failure
    (10 * mega) -- allow restart every 10 seconds in case of failure

runCutMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runCutMonitor logger db = L.withLoggerLabel ("component", "cut-monitor") logger $ \l ->
    runMonitorLoop "ChainwebNode.runCutMonitor" l $ do
        logFunctionText l Info $ "Initialized Cut Monitor"
        S.mapM_ (logFunctionJson l Info)
            $ S.map (cutToCutHashes Nothing)
            $ cutStream db

{-
runAmberdataBlockMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runAmberdataBlockMonitor logger db
    = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l ->
        runMonitorLoop "Chainweb.Logging.amberdataBlockMonitor" l (amberdataBlockMonitor l db)

-}

-- type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

-- This instances are OK, since this is the "Main" module of an application
--
deriving instance Generic GCDetails
deriving instance NFData GCDetails
deriving instance ToJSON GCDetails

deriving instance Generic RTSStats
deriving instance NFData RTSStats
deriving instance ToJSON RTSStats

runRtsMonitor :: Logger logger => logger -> IO ()
runRtsMonitor logger = L.withLoggerLabel ("component", "rts-monitor") logger go
  where
    go l = getRTSStatsEnabled >>= \case
        False -> do
            logFunctionText l Warn "RTS Stats isn't enabled. Run with '+RTS -T' to enable it."
        True -> do
            logFunctionText l Info $ "Initialized RTS Monitor"
            runMonitorLoop "Chainweb.Node.runRtsMonitor" l $ do
                stats <- getRTSStats
                logFunctionText l Info $ "got stats"
                logFunctionJson logger Info stats
                logFunctionText l Info $ "logged stats"
                threadDelay 60000000 {- 1 minute -}

data QueueStats = QueueStats
    { _queueStatsCutQueueSize :: !Natural
    , _queueStatsBlockHeaderQueueSize :: !Natural
    , _queueStatsBlockHeaderTaskMapSize :: !Natural
    , _queueStatsPayloadQueueSize :: !Natural
    , _queueStatsPayloadTaskMapSize :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, ToJSON)

runQueueMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runQueueMonitor logger cutDb = L.withLoggerLabel ("component", "queue-monitor") logger go
  where
    go l = do
        logFunctionText l Info $ "Initialized Queue Monitor"
        runMonitorLoop "ChainwebNode.runQueueMonitor" l $ do
            stats <- QueueStats
                <$> cutDbQueueSize cutDb
                <*> pQueueSize (_webBlockHeaderStoreQueue $ view cutDbWebBlockHeaderStore cutDb)
                <*> (int <$> TM.size (_webBlockHeaderStoreMemo $ view cutDbWebBlockHeaderStore cutDb))
                <*> pQueueSize (_webBlockPayloadStoreQueue $ view cutDbPayloadStore cutDb)
                <*> (int <$> TM.size (_webBlockPayloadStoreMemo $ view cutDbPayloadStore cutDb))

            logFunctionText l Info $ "got stats"
            logFunctionJson logger Info stats
            logFunctionText l Info $ "logged stats"
            threadDelay 60000000 {- 1 minute -}

node :: Logger logger => StandaloneConfiguration -> logger -> IO ()
node conf logger = do
    rocksDbDir <- getRocksDbDir
    when (_nodeConfigResetChainDbs conf) $ destroyRocksDb rocksDbDir
    withRocksDb rocksDbDir $ \rocksDb -> do
        logFunctionText logger Info $ "opened rocksdb in directory" <> sshow rocksDbDir
        withChainwebStandalone cwConf logger rocksDb (_nodeConfigDatabaseDirectory conf) (_nodeConfigResetChainDbs conf) $ \cw -> mapConcurrently_ id
            [ runChainweb' cw
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            -- , runAmberdataBlockMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf
    nodeText = T.unpack (toText (_configNodeId cwConf))
    v = _configChainwebVersion cwConf
    getRocksDbDir = case _nodeConfigDatabaseDirectory conf of
      Nothing -> getXdgDirectory XdgData
            $ "chainweb-standalone-node/" <> sshow v <> "/" <> nodeText <> "/rocksDb"
      Just d -> return d

-- | Starts server and runs all network clients
--
runChainweb'
    :: forall logger cas
    . Logger logger
    => PayloadCas cas
    => Chainweb logger cas
    -> IO ()
runChainweb' cw = do
    logg Info "start chainweb node"

    -- miner
    withAsync miner wait

    -- forever (threadDelay 1000000)

{-
    -- 1. Start serving Rest API
    --
    withAsync (serve $ throttle (_chainwebThrottler cw) . httpLog) $ \_server ->
        wait _server
        -- forever (threadDelay 1000000)
        {-
        logg Info "started server"

        -- 2. Start Clients
        --
        mpClients <- mempoolSyncClients
        let clients = concat
              [ miner
              , cutNetworks mgr (_chainwebCutResources cw)
              , mpClients
              ]
        mapConcurrently_ id clients
        wait server
        -}
-}
  where
    logg = logFunctionText $ _chainwebLogger cw
    miner = maybe go (\m -> runMiner (_chainwebVersion cw) m) $ _chainwebMiner cw
        where
          go = do
            logg Warn "No miner configured. Starting consensus without mining."
            forever (threadDelay 1000000)
    -- miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw
{-

    -- chains
    _chains = HM.toList (_chainwebChains cw)
    _chainVals = map snd _chains

    -- collect server resources
    proj :: forall a . (ChainResources logger -> a) -> [(ChainId, a)]
    proj f = flip map _chains $ \(k, ch) -> (k, f ch)

    chainDbsToServe = proj _chainResBlockHeaderDb
    mempoolsToServe = proj _chainResMempool
    chainP2pToServe = bimap ChainNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)
    memP2pToServe = bimap MempoolNetwork (_peerResDb . _chainResPeer) <$> itoList (_chainwebChains cw)

    payloadDbsToServe = itoList $ const (view chainwebPayloadDb cw) <$> _chainwebChains cw
    pactDbsToServe = _chainwebPactData cw

    serverSettings
        = setOnException
            (\r e -> when (defaultShouldDisplayException e) (logg Error $ loggServerError r e))
        $ peerServerSettings (_peerResPeer $ _chainwebPeer cw)

    -- serve = return undefined

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

    -- HTTP Request Logger

    httpLog :: Middleware
    httpLog = requestResponseLogger $ setComponent "http" (_chainwebLogger cw)

    loggServerError (Just r) e = "HTTP server error: " <> sshow e <> ". Request: " <> sshow r
    loggServerError Nothing e = "HTTP server error: " <> sshow e

    -- Cut DB and Miner

    cutDb = _cutResCutDb $ _chainwebCutResources cw
    cutPeerDb = _peerResDb $ _cutResPeer $ _chainwebCutResources cw

    _miner = maybe [] (\m -> [ runMiner (_chainwebVersion cw) m ]) $ _chainwebMiner cw

    -- Configure Clients

    mgr = _chainwebManager cw


    -- Mempool

    _mempoolP2pConfig = _configMempoolP2p $ _chainwebConfig cw

    -- Decide whether to enable the mempool sync clients
    _mempoolSyncClients = case enabledConfig _mempoolP2pConfig of
      Nothing -> disabled
      Just c -> case _chainwebVersion cw of
        Test{} -> disabled
        TimedConsensus{} -> disabled
        PowConsensus{} -> disabled
        TimedCPM{} -> enabled c
        Development -> enabled c
        -- Testnet00 -> enabled c
        -- Testnet01 -> enabled c
        Testnet02 -> enabled c
      where
        disabled = do
          logg Info "Mempool p2p sync disabled"
          return []
        enabled conf = do
          logg Info "Mempool p2p sync enabled"
          return $ map (runMempoolSyncClient mgr conf) _chainVals
-}


data StandaloneConfiguration = StandaloneConfiguration
  { _nodeConfigChainweb :: !ChainwebConfiguration
  , _nodeConfigLog :: !LogConfig
  , _nodeConfigDatabaseDirectory :: !(Maybe FilePath)
  , _nodeConfigResetChainDbs :: !Bool
  }
  deriving (Show, Eq, Generic)

makeLenses ''StandaloneConfiguration

defaultStandaloneConfiguration :: ChainwebVersion -> StandaloneConfiguration
defaultStandaloneConfiguration v = StandaloneConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigDatabaseDirectory = Nothing
    , _nodeConfigResetChainDbs = False
    }

instance ToJSON StandaloneConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        , "databaseDirectory" .= _nodeConfigDatabaseDirectory o
        , "resetChainDatabases" .= _nodeConfigResetChainDbs o
        ]

instance FromJSON (StandaloneConfiguration -> StandaloneConfiguration) where
    parseJSON = withObject "StandaloneConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o
        <*< nodeConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< nodeConfigResetChainDbs ..: "resetChainDatabases" % o

pStandaloneConfiguration :: MParser StandaloneConfiguration
pStandaloneConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: pLogConfig
    <*< nodeConfigDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> help "directory where the databases are persisted"
    <*< nodeConfigResetChainDbs .:: enableDisableFlag
        % long "reset-chain-databases"
        <> help "Reset the chain databases for all chains on startup"


withNodeLogger
    :: LogConfig
    -> ChainwebVersion
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logConfig v f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    _mgrHttps <- liftIO $ HTTPS.newTlsManager

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr (_logConfigBackend logConfig)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    {-p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig-}
    rtsBackend <- managed
        $ mkTelemetryLogger @RTSStats mgr teleLogConfig
    counterBackend <- managed $ configureHandler
        (withJsonHandleBackend @CounterLog "connectioncounters" mgr)
        teleLogConfig
    {-newBlockAmberdataBackend <- managed $ mkAmberdataLogger mgrHttps amberdataConfig-}
    newBlockBackend <- managed
        $ mkTelemetryLogger @NewMinedBlock mgr teleLogConfig
    {- requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig -}
    queueStatsBackend <- managed
        $ mkTelemetryLogger @QueueStats mgr teleLogConfig
    {-reintroBackend <- managed
        $ mkTelemetryLogger @ReintroducedTxsLog mgr teleLogConfig-}
    traceBackend <- managed
        $ mkTelemetryLogger @Trace mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logHandler monitorBackend
            -- , logHandler p2pInfoBackend
            , logHandler rtsBackend
            , logHandler counterBackend
            -- , logHandler newBlockAmberdataBackend
            , logHandler newBlockBackend
            -- , logHandler requestLogBackend
            , logHandler queueStatsBackend
            -- , logHandler reintroBackend
            , logHandler traceBackend
            ] baseBackend

    liftIO $ f
        $ maybe id (\x -> addLabel ("cluster", toText x)) (_logConfigClusterId logConfig)
        $ addLabel ("chainwebVersion", sshow v)
        $ logger
  where
    teleLogConfig = _logConfigTelemetryBackend logConfig
    _amberdataConfig = _logConfigAmberdataBackend logConfig

mkAmberdataLogger
    :: HTTP.Manager
    -> Maybe AmberdataConfig
    -> (Backend (JsonLog AmberdataBlock) -> IO b)
    -> IO b
mkAmberdataLogger _ Nothing inner = inner (const $ return ())
mkAmberdataLogger mgr (Just config) inner = withAmberDataBlocksBackend mgr config inner

mkTelemetryLogger
    :: forall a b
    . Typeable a
    => ToJSON a
    => HTTP.Manager
    -> EnableConfig BackendConfig
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
mkTelemetryLogger mgr = configureHandler
    $ withJsonHandleBackend @(JsonLog a) (sshow $ typeRep $ Proxy @a) mgr

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo StandaloneConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pStandaloneConfiguration
    (defaultStandaloneConfiguration Testnet02)

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    withNodeLogger (_nodeConfigLog conf) v $ node conf
