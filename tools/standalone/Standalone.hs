{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Standalone where

import Configuration.Utils hiding (Error)

import Control.Concurrent.Async
import Control.Concurrent
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Managed

import Data.CAS
import Data.CAS.RocksDB
import Data.Foldable
import Data.Function
import Data.List
import Data.LogMessage
import Data.PQueue
import qualified  Data.HashMap.Strict as HM
import qualified Data.TaskMap as TM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable

import GHC.Stats
import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import Numeric.Natural

import P2P.Peer
import P2P.Node.Configuration

import PkgInfo

import Standalone.Utils

import qualified Streaming.Prelude as S

import System.Directory
import qualified System.Logger as L
import System.LogLevel

import Utils.Logging
import Utils.Logging.Config
import Utils.Logging.Trace

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.MinerResources
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
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Utils
import Chainweb.Utils.RequestLog
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService
import qualified Chainweb.Mempool.Consensus as MPCon
import qualified Chainweb.Mempool.InMem as Mempool
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Pact.BloomCache as Bloom

defaultV :: ChainwebVersion
defaultV = TimedCPM petersonChainGraph

-- Let's create a benchmark that incorporates unfurled components
-- (that may also be configured differently to isolate certain
-- behaviours) of the chainweb system (e.g. Mempool, Pact Service). We
-- are trying to investigate which portion of this system leads to
-- memory/time leaks or any other stability issues.

unfurledChainweb :: Maybe Int -> ChainwebVersion -> IO ()
unfurledChainweb _limit v =
  withAll v $ \sqlenv0s -> do
    let pactIO bhdb pdb =
          testWebPactExecutionService v Nothing
          (return bhdb) (return pdb)
          (return mempty) sqlenv0s
    withTempRocksDb "standalone" $ \rdb ->
        -- this version of withTestCutDb is slightly different because
        -- it can mine for an unbounded number of blocks (by providing
        -- Nothing instead of Just [some number])
        withTestCutDb rdb v 20 pactIO logg' $ \cutDB -> do
            _cdb <- newMVar cutDB
            return ()
    return ()

logg' :: LogMessage a => LogLevel -> a -> IO ()
logg' l
  | l <= Warn = T.putStrLn . logText
  | otherwise = const $ return ()


-- initChainweb :: ChainwebVersion -> IO ()
-- initChainweb =
--     withTempRocksDb "standalone" $ \rocks ->
--     withBlockHeaderDb rocks

{-
withChainwebInternal'
  :: Logger logger
  => ChainwebConfiguration
  -> logger
  -> RocksDb
  -> Maybe FilePath
  -> Maybe NodeId
  -> Bool
  -> (Chainweb logger RocksDbCas -> IO a)
  -> IO a
withChainwebInternal' = undefined
-}


-- let's see if this works

withChainwebNoNetwork
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> Maybe FilePath
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebNoNetwork c logger rocksDb dbDir resetDb inner =
  withChainwebInternalNoNetwork
    c
    logger
    rocksDb
    dbDir
    (Just (_configNodeId c))
    resetDb
    inner

withChainResources'
  :: Logger logger
  => PayloadCas cas
  => ChainwebVersion
  -> ChainId
  -> RocksDb
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
withChainResources' v cid rdb logger mempoolCfg cdbv payloadDb prune dbDir nodeid resetDb inner =
  withBlockHeaderDb rdb v cid $ \cdb ->
    -- we'll need to place a shim here
    Mempool.withInMemoryMempool mempoolCfg rdb $ \mempool -> do
        mpc <- MPCon.mkMempoolConsensus reIntroEnabled mempool cdb $ Just payloadDb
        withPactService v cid (setComponent "pact" logger) mpc cdbv cdb payloadDb dbDir nodeid resetDb $
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
                    logg Info $ "finished pruning block header database. Deleted " <> sshow x <> " block headers."

                -- replay pact
                let pact = pes mempool requestQ

                -- run inner
                inner ChainResources
                    { _chainResPeer = undefined
                    , _chainResBlockHeaderDb = cdb
                    , _chainResLogger = logger
                    , _chainResMempool = mempool
                    , _chainResPact = pact
                    }
  where
    logg = logFunctionText (setComponent "pact-tx-replay" logger)
    diam = diameter (_chainGraph v)
    reIntroEnabled = Mempool._inmemEnableReIntro mempoolCfg
    pes mempool requestQ = case v of
        Test{} -> emptyPactExecutionService
        TimedConsensus{} -> emptyPactExecutionService
        PowConsensus{} -> emptyPactExecutionService
        TimedCPM{} -> mkPactExecutionService mempool requestQ
        Development -> mkPactExecutionService mempool requestQ
        Testnet00 -> mkPactExecutionService mempool requestQ
        Testnet01 -> mkPactExecutionService mempool requestQ
        Testnet02 -> mkPactExecutionService mempool requestQ

-- TODO: might need a different version of mkPactExecutionService that
-- factors out mempool.


withChainwebInternalNoNetwork
    :: Logger logger
    => ChainwebConfiguration
    -> logger
    -> RocksDb
    -> Maybe FilePath
    -> Maybe NodeId
    -> Bool
    -> (Chainweb logger RocksDbCas -> IO a)
    -> IO a
withChainwebInternalNoNetwork conf logger rocksDb dbDir nodeid resetDb inner = do
    initializePayloadDb v payloadDb
    cdbv <- newEmptyMVar
    concurrentWith
      -- initialize chains concurrently
      (\cid -> withChainResources' v cid rocksDb (chainLogger cid)
               mempoolquestionmark cdbv payloadDb prune dbDir nodeid resetDb)

      -- initialize global resources after all chain resources are
      -- initialized
      (\cs -> global (HM.fromList $ zip cidsList cs) cdbv)
      cidsList
  where
    mempoolquestionmark = undefined
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
          -- mgr = _peer
      logg Info "start initializing cut resources"
      withCutResources' cutConfig cutLogger rocksDb webchain payloadDb pact $ \cuts -> do
        logg Info "finished initializing cut resources"
        let mLogger = setComponent "miner" logger
            mConf = _configMiner conf
            mCutDb = _cutResCutDb cuts

        -- no throttling necessary

        void $! putMVar cdbv mCutDb

        logg Info "start synchronizing Pact DBs"
        synchronizePactDb cs mCutDb
        logg Info "finished synchronizing Pact DBs"

        withPactData cs cuts $ \pactData -> do
            logg Info "start initializing miner resources"
            withMinerResources mLogger mConf cwnid mCutDb $ \m -> do
              logg Info "finished initializing miner resources"
              inner Chainweb
                        { _chainwebHostAddress = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                        , _chainwebChains = cs
                        , _chainwebCutResources = cuts
                        , _chainwebNodeId = cwnid
                        , _chainwebMiner = m
                        , _chainwebLogger = logger
                        -- , _chainwebPeer = peer
                        , _chainwebPeer = undefined
                        , _chainwebPayloadDb = payloadDb
                        -- , _chainwebManager = mgr
                        , _chainwebManager = undefined
                        , _chainwebPactData = pactData
                        -- , _chainwebThrottler = throttler
                        , _chainwebThrottler = undefined
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
    _enableTxsReintro = _configReintroTxs conf
    _mempoolConf = _mempoolConfig _enableTxsReintro

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


withCutResources' :: a
withCutResources' = undefined

_mempoolConfig :: a -> b
_mempoolConfig _enableReIntro = undefined

-- -- TODO: The type InMempoolConfig contains parameters that should be
-- -- configurable as well as parameters that are determined by the chainweb
-- -- version or the chainweb protocol. These should be separated in to two
-- -- different types.
-- --
-- mempoolConfig :: Bool -> Mempool.InMemConfig ChainwebTransaction
-- mempoolConfig enableReIntro = Mempool.InMemConfig
--     Mempool.chainwebTransactionConfig
--     blockGasLimit
--     mempoolReapInterval
--     maxRecentLog
--     enableReIntro
--   where
--     blockGasLimit = 100000               -- TODO: policy decision
--     mempoolReapInterval = 60 * 20 * 1000000   -- 20 mins
--     maxRecentLog = 2048                   -- store 2k recent transaction hashes



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

runAmberdataBlockMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runAmberdataBlockMonitor logger db
    = L.withLoggerLabel ("component", "amberdata-block-monitor") logger $ \l ->
        runMonitorLoop "Chainweb.Logging.amberdataBlockMonitor" l (amberdataBlockMonitor l db)

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
        withChainweb cwConf logger rocksDb (_nodeConfigDatabaseDirectory conf) (_nodeConfigResetChainDbs conf) $ \cw -> mapConcurrently_ id
            [ runChainweb cw
            , runCutMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runAmberdataBlockMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runQueueMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw)
            , runRtsMonitor (_chainwebLogger cw)
            ]
  where
    cwConf = _nodeConfigChainweb conf
    nodeText = T.unpack (toText (_configNodeId cwConf))
    v = _configChainwebVersion cwConf
    getRocksDbDir = case _nodeConfigDatabaseDirectory conf of
      Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> nodeText <> "/rocksDb"
      Just d -> return d


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
    requestLogBackend <- managed
        $ mkTelemetryLogger @RequestResponseLog mgr teleLogConfig
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
            , logHandler requestLogBackend
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
    (defaultStandaloneConfiguration Testnet01)


main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _nodeConfigChainweb conf
    withNodeLogger (_nodeConfigLog conf) v $ node conf
