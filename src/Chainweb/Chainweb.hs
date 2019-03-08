{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
-- * Configuration
  ChainwebConfiguration(..)
, configNodeId
, configChainwebVersion
, configMiner
, configP2p
, configChainDbDirPath
, defaultChainwebConfiguration
, pChainwebConfiguration

-- * Peer Resources
, allocatePeer
, withPeer
, peerServerSettings

-- * Cut Resources
, Cuts(..)
, cutsChainwebVersion
, cutsCutConfig
, cutsP2pConfig
, cutsPeer
, cutsCutDb
, cutsPeerDb
, cutsLogFun

, withCuts
, runCutNetworkCutSync
, runCutNetworkHeaderSync
, runCutNetworkPayloadSync

-- * Single Chain Resources
, Chain(..)
, chainChainId
, chainChainwebVersion
, chainP2pConfig
, chainPeer
, chainBlockHeaderDb
, chainPeerDb
, chainLogFun
, chainSyncDepth
, chainMempool

, withChain
, runChainSyncClient
, runMempoolSyncClient

-- * Chainweb Logging Functions
, ChainwebLogFunctions(..)
, chainwebNodeLogFun
, chainwebMinerLogFun
, chainwebCutLogFun
, chainwebChainLogFuns

-- * Chainweb Resources
, Chainweb(..)
, chainwebChainwebVersion
, chainwebChains
, chainwebCuts
, chainwebNodeId
, chainwebHostAddress
, chainwebMiner
, chainwebLogFun
, chainwebSocket
, chainwebPeer
, chainwebPayloadDb

-- ** Mempool integration
, ChainwebTransaction
, chainwebTransactionConfig

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Configuration.Utils hiding (Lens', (<.>))

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (SomeAsyncException)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef
import Data.IxSet.Typed (getEQ, getOne)
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Prelude hiding (log)

import qualified Network.HTTP.Client as HTTP
import Network.Socket (Socket, close)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setHost, setPort)

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.LogLevel
import System.Path

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.CutDB
import qualified Chainweb.CutDB.Sync as C
import Chainweb.Graph
import Chainweb.HostAddress
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Mempool.Mempool (MempoolBackend)
import qualified Chainweb.Mempool.Mempool as Mempool
import qualified Chainweb.Mempool.RestAPI.Client as MPC
import Chainweb.Miner.Config
import Chainweb.Miner.POW
import Chainweb.Miner.Test
import Chainweb.NodeId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Transaction
import Chainweb.TreeDB.Persist
import Chainweb.TreeDB.RemoteDB
import Chainweb.TreeDB.Sync
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB

import Data.CAS.HashMap hiding (toList)
import Data.LogMessage

import Network.X509.SelfSigned

import P2P.Node
import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Peer
import P2P.Session
import P2P.TaskQueue

import qualified Servant.Client as Sv

-- -------------------------------------------------------------------------- --
-- PRELIMINARY TESTING

-- | FAKE pact execution service
--
pact :: PactExectutionService
pact = PactExectutionService $ \_ d -> return
    $ payloadWithOutputs d $ getFakeOutput <$> _payloadDataTransactions d
  where
    getFakeOutput (Transaction txBytes) = TransactionOutput txBytes

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configNodeId :: !NodeId
    , _configMiner :: !MinerConfig
    , _configP2p :: !P2pConfiguration
    , _configChainDbDirPath :: !(Maybe FilePath)
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

instance HasChainwebVersion ChainwebConfiguration where
    _chainwebVersion = _configChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ChainwebConfiguration where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

defaultChainwebConfiguration :: ChainwebVersion -> ChainwebConfiguration
defaultChainwebConfiguration v = ChainwebConfiguration
    { _configChainwebVersion = v
    , _configNodeId = NodeId 0 -- FIXME
    , _configMiner = defaultMinerConfig
    , _configP2p = defaultP2pConfiguration v
    , _configChainDbDirPath = Nothing
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "nodeId" .= _configNodeId o
        , "miner" .= _configMiner o
        , "p2p" .= _configP2p o
        , "chainDbDirPath" .= _configChainDbDirPath o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configNodeId ..: "nodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configP2p %.: "p2p" % o
        <*< configChainDbDirPath ..: "chainDbDirPath" % o

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
    <*< configMiner %:: pMinerConfig
    <*< configP2p %:: pP2pConfiguration Nothing
    <*< configChainDbDirPath .:: fmap Just % textOption
        % long "chain-db-dir"
        <> help "directory where chain databases are persisted"

-- -------------------------------------------------------------------------- --
-- Allocate Peer Resources

-- | Allocate Peer resources. All P2P networks of a chainweb node share the a
-- single Peer and the associated underlying network resources.
--
-- The following resources are allocated:
--
-- * Resolve port and allocating a socket for the port,
-- * Generate a new certifcate, if none is provided in the configuration, and
-- * adjust the P2PConfig with the new values.
--
allocatePeer :: PeerConfig -> IO (PeerConfig, Socket, Peer)
allocatePeer conf = do
    (p, sock) <- bindPortTcp (_peerConfigPort conf) (_peerConfigInterface conf)
    let conf' = set peerConfigPort p conf
    peer <- unsafeCreatePeer conf'
    return (conf', sock, peer)

peerServerSettings :: Peer -> Settings
peerServerSettings peer
    = setPort (int . _hostAddressPort . _peerAddr $ _peerInfo peer)
    . setHost (_peerInterface peer)
    $ defaultSettings

withPeer :: PeerConfig -> ((PeerConfig, Socket, Peer) -> IO a) -> IO a
withPeer conf = bracket (allocatePeer conf) (\(_, sock, _) -> close sock)

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data Cuts = Cuts
    { _cutsChainwebVersion :: !ChainwebVersion
    , _cutsCutConfig :: !CutDbConfig
    , _cutsP2pConfig :: !P2pConfiguration
    , _cutsPeer :: !Peer
    , _cutsCutDb :: !CutDb
    , _cutsPeerDb :: !PeerDb
    , _cutsLogFun :: !ALogFunction
    , _cutsHeaderStore :: !WebBlockHeaderStore
    , _cutsPayloadStore :: !(WebBlockPayloadStore HashMapCas)
    }

makeLenses ''Cuts

withCuts
    :: ChainwebVersion
    -> CutDbConfig
    -> P2pConfiguration
    -> Peer
    -> PeerDb
    -> ALogFunction
    -> WebBlockHeaderDb
    -> HTTP.Manager
    -> (Cuts -> IO a)
    -> IO a
withCuts v cutDbConfig p2pConfig peer peerDb logfun webchain mgr f = do

    -- initialize blockheader store
    headerStore <- newWebBlockHeaderStore mgr webchain (_getLogFunction logfun)

    -- initialize payload store
    payloadStore <- newWebPayloadStore v mgr pact (_getLogFunction logfun)

    withCutDb cutDbConfig (_getLogFunction logfun) headerStore payloadStore $ \cutDb ->
        f $ Cuts v cutDbConfig p2pConfig peer cutDb peerDb logfun headerStore payloadStore

cutNetworks :: HTTP.Manager -> Cuts -> [IO ()]
cutNetworks mgr cuts =
    [ runCutNetworkCutSync mgr cuts
    , runCutNetworkHeaderSync mgr cuts
    , runCutNetworkPayloadSync mgr cuts
    ]

-- | P2P Network for pushing Cuts
--
runCutNetworkCutSync :: HTTP.Manager -> Cuts -> IO ()
runCutNetworkCutSync mgr cuts = mkCutNetworkSync mgr cuts "cut sync"
    $ C.syncSession (_cutsChainwebVersion cuts) (_peerInfo $ _cutsPeer cuts) (_cutsCutDb cuts)

-- | P2P Network for Block Headers
--
runCutNetworkHeaderSync :: HTTP.Manager -> Cuts -> IO ()
runCutNetworkHeaderSync mgr cuts = mkCutNetworkSync mgr cuts "block header sync"
    $ session attemptsLimit $ _webBlockHeaderStoreQueue $ view cutsHeaderStore cuts
  where
    attemptsLimit = 10

-- | P2P Network for Block Payloads
--
runCutNetworkPayloadSync :: HTTP.Manager -> Cuts -> IO ()
runCutNetworkPayloadSync mgr cuts = mkCutNetworkSync mgr cuts "block payload sync"
    $ session attemptsLimit $ _webBlockPayloadStoreQueue $ view cutsPayloadStore cuts
  where
    attemptsLimit = 10

-- | P2P Network for Block Payloads
--
-- This uses the 'CutNetwork' for syncing peers. The network doesn't restrict
-- the API network endpoints that are used in the client sessions.
--
mkCutNetworkSync
    :: HTTP.Manager
    -> Cuts
    -> T.Text
    -> P2pSession
    -> IO ()
mkCutNetworkSync mgr cuts label s = bracket create destroy $ \n ->
    p2pStartNode (_cutsP2pConfig cuts) n
  where
    v = _cutsChainwebVersion cuts
    peer = _cutsPeer cuts
    logfun = _cutsLogFun cuts
    peerDb = _cutsPeerDb cuts

    create = do
        n <- p2pCreateNode v CutNetwork peer (_getLogFunction logfun) peerDb mgr s
        logg Info $ label <> ": initialized"
        return n

    destroy n = do
        p2pStopNode n
        logg Info $ label <> ": stopped"

    logg = alogFunction @T.Text (_cutsLogFun cuts)

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data Chain = Chain
    { _chainChainId :: !ChainId
    , _chainChainwebVersion :: !ChainwebVersion
    , _chainP2pConfig :: !P2pConfiguration
    , _chainPeer :: !Peer
    , _chainBlockHeaderDb :: !BlockHeaderDb
    , _chainPeerDb :: !PeerDb
    , _chainLogFun :: !ALogFunction
    , _chainSyncDepth :: !Depth
    , _chainMempool :: !(MempoolBackend ChainwebTransaction)
    }

makeLenses ''Chain

instance HasChainwebVersion Chain where
    _chainwebVersion = _chainChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph Chain where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

-- Intializes all local Chain resources, but doesn't start any networking.
--
withChain
    :: ChainwebVersion
    -> ChainId
    -> P2pConfiguration
    -> Peer
    -> PeerDb
    -> (Maybe FilePath)
    -> ALogFunction
    -> Mempool.InMemConfig ChainwebTransaction
    -> (Chain -> IO a)
    -> IO a
withChain v cid p2pConfig peer peerDb chainDbDir logfun mempoolCfg inner =
    Mempool.withInMemoryMempool mempoolCfg $ \mempool ->
    withBlockHeaderDb v cid $ \cdb -> do
        chainDbDirPath <- traverse (makeAbsolute . fromFilePath) chainDbDir
        withPersistedDb cid chainDbDirPath cdb $
            inner $ Chain
                { _chainChainId = cid
                , _chainChainwebVersion = v
                , _chainP2pConfig = p2pConfig
                , _chainPeer = peer
                , _chainBlockHeaderDb = cdb
                , _chainPeerDb = peerDb
                , _chainLogFun = logfun
                , _chainSyncDepth = syncDepth (_chainGraph v)
                , _chainMempool = mempool
                }

withPersistedDb
    :: ChainId
    -> Maybe (Path Absolute)
    -> BlockHeaderDb
    -> IO a
    -> IO a
withPersistedDb _ Nothing _ = id
withPersistedDb cid (Just dir) db = bracket_ load (persist path db)
  where
    path = dir </> fragment "chain" <.> FileExt (T.unpack (toText cid))
    load = do
        createDirectoryIfMissing True (toFilePath dir)
        whenM (doesFileExist $ toFilePath path) (restore path db)

-- | Synchronize the local block database over the P2P network.
--
runChainSyncClient
    :: HTTP.Manager
        -- ^ HTTP connection pool
    -> Chain
        -- ^ chain resources
    -> IO ()
runChainSyncClient mgr chain = bracket create destroy go
  where
    netId = ChainNetwork (_chainChainId chain)
    syncLogg = alogFunction @T.Text (_chainLogFun chain)
    create = p2pCreateNode
        (_chainChainwebVersion chain)
        netId
        (_chainPeer chain)
        (_getLogFunction $ _chainLogFun chain)
        (_chainPeerDb chain)
        mgr
        (chainSyncP2pSession (_chainSyncDepth chain) (_chainBlockHeaderDb chain))

    go n = do
        -- Run P2P client node
        syncLogg Info "initialized"
        p2pStartNode (_chainP2pConfig chain) n

    destroy n = p2pStopNode n `finally` syncLogg Info "stopped"

chainSyncP2pSession :: BlockHeaderTreeDb db => Depth -> db -> P2pSession
chainSyncP2pSession depth db logg env = do
    peer <- PeerTree <$> remoteDb db logg env
    chainwebSyncSession db peer depth logg

syncDepth :: ChainGraph -> Depth
syncDepth g = Depth (2 * diameter g)
{-# NOINLINE syncDepth #-}


-- -------------------------------------------------------------------------- --
-- Mempool sync.
-- | Synchronize the local mempool over the P2P network.
--
runMempoolSyncClient
    :: HTTP.Manager
        -- ^ HTTP connection pool
    -> Chain
        -- ^ chain resources
    -> IO ()
runMempoolSyncClient mgr chain = bracket create destroy go
  where
    create = do
        log Debug "starting mempool p2p sync"
        p2pCreateNode v netId peer (_getLogFunction $ _chainLogFun chain) peerDb mgr $
            mempoolSyncP2pSession chain
    go n = do
        -- Run P2P client node
        log Info "mempool sync p2p node initialized, starting session"
        p2pStartNode p2pConfig n

    destroy n = p2pStopNode n `finally` log Info "mempool sync p2p node stopped"

    v = _chainChainwebVersion chain
    peer = _chainPeer chain
    p2pConfig = _chainP2pConfig chain
    peerDb = _chainPeerDb chain
    netId = ChainNetwork $ _chainChainId chain
    log = alogFunction @T.Text (_chainLogFun chain)


mempoolSyncP2pSession :: Chain -> P2pSession
mempoolSyncP2pSession chain logg0 env = go
  where
    go = flip catches [ Handler asyncHandler , Handler errorHandler ] $ do
             logg Debug "mempool sync session starting"
             Mempool.syncMempools pool peerMempool
             logg Debug "mempool sync session succeeded"
             return False

    remote = T.pack $ Sv.showBaseUrl $ Sv.baseUrl env
    logg d m = logg0 d $ T.concat ["[mempool sync@", remote, "]:", m]

    asyncHandler (_ :: SomeAsyncException) = do
        logg Debug "mempool sync session cancelled"
        return False

    errorHandler (e :: SomeException) = do
        logg Debug ("mempool sync session failed: " <> sshow e)
        throwM e

    peerMempool = MPC.toMempool v cid txcfg gaslimit env
    pool = _chainMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    gaslimit = Mempool.mempoolBlockGasLimit pool
    cid = _chainChainId chain
    v = _chainChainwebVersion chain


-- -------------------------------------------------------------------------- --
-- Miner

data Miner = Miner
    { _minerLogFun :: !ALogFunction
    , _minerNodeId :: !NodeId
    , _minerCutDb :: !CutDb
    , _minerWebBlockHeaderDb :: !WebBlockHeaderDb
    , _minerWebPayloadDb :: !(PayloadDb HashMapCas)
    , _minerConfig :: !MinerConfig
    }

withMiner
    :: ALogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb
    -> WebBlockHeaderDb
    -> PayloadDb HashMapCas
    -> (Miner -> IO a)
    -> IO a
withMiner logFun conf nid cutDb webDb payloadDb inner = inner $ Miner
    { _minerLogFun = logFun
    , _minerNodeId = nid
    , _minerCutDb = cutDb
    , _minerWebBlockHeaderDb = webDb
    , _minerWebPayloadDb = payloadDb
    , _minerConfig = conf
    }

runMiner :: ChainwebVersion -> Miner -> IO ()
runMiner v m = (chooseMiner v)
    (_getLogFunction $ _minerLogFun m)
    (_minerConfig m)
    (_minerNodeId m)
    (_minerCutDb m)
    (_minerWebBlockHeaderDb m)
    (_minerWebPayloadDb m)
  where
    chooseMiner
        :: ChainwebVersion
        -> LogFunction
        -> MinerConfig
        -> NodeId
        -> CutDb
        -> WebBlockHeaderDb
        -> PayloadDb HashMapCas
        -> IO ()
    chooseMiner Test{} = testMiner
    chooseMiner TestWithTime{} = testMiner
    chooseMiner TestWithPow{} = powMiner
    chooseMiner Simulation{} = testMiner
    chooseMiner Testnet00 = powMiner

-- -------------------------------------------------------------------------- --
-- Chainweb Log Functions

data ChainwebLogFunctions = ChainwebLogFunctions
    { _chainwebNodeLogFun :: !ALogFunction
    , _chainwebMinerLogFun :: !ALogFunction
    , _chainwebCutLogFun :: !ALogFunction
    , _chainwebChainLogFuns :: !(HM.HashMap ChainId ALogFunction)
    }

makeLenses ''ChainwebLogFunctions

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb = Chainweb
    { _chainwebChainwebVersion :: !ChainwebVersion
    , _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId Chain)
    , _chainwebCuts :: !Cuts
    , _chainwebNodeId :: !NodeId
    , _chainwebMiner :: !Miner
    , _chainwebLogFun :: !ALogFunction
    , _chainwebSocket :: !Socket
    , _chainwebPeer :: !Peer
    , _chainwebPayloadDb :: !(PayloadDb HashMapCas)
    , _chainwebManager :: !HTTP.Manager
    }

makeLenses ''Chainweb

instance HasChainwebVersion Chainweb where
    _chainwebVersion = _chainwebChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph Chainweb where
    _chainGraph = _chainGraph . _chainwebVersion
    {-# INLINE _chainGraph #-}

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: ChainwebConfiguration
    -> ChainwebLogFunctions
    -> (Chainweb -> IO a)
    -> IO a
withChainweb conf logFuns inner = withPeer (view confLens conf) $ \(c, sock, peer) ->
    withChainwebInternal (set confLens c conf) logFuns sock peer inner
  where
    confLens :: Lens' ChainwebConfiguration PeerConfig
    confLens = configP2p . p2pConfigPeer

mempoolConfig :: Mempool.InMemConfig ChainwebTransaction
mempoolConfig = Mempool.InMemConfig
    chainwebTransactionConfig
    blockGasLimit
    mempoolReapInterval
  where
    blockGasLimit = 1000000                 -- TODO: policy decision
    mempoolReapInterval = 60 * 20 * 1000000   -- 20 mins

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainwebInternal
    :: ChainwebConfiguration
    -> ChainwebLogFunctions
    -> Socket
    -> Peer
    -> (Chainweb -> IO a)
    -> IO a
withChainwebInternal conf logFuns socket peer inner = do
    let nids = HS.map ChainNetwork cids `HS.union` HS.singleton CutNetwork
    withPeerDb nids p2pConf $ \peerDb -> do
        payloadDb <- emptyPayloadDb
        initializePayloadDb v payloadDb
        go peerDb payloadDb mempty (toList cids)
  where
    -- Initialize chain resources
    go peerDb payloadDb cs (cid : t) =
        case HM.lookup cid (_chainwebChainLogFuns logFuns) of
            Nothing -> error $ T.unpack
                $ "Failed to initialize chainweb node: missing log function for chain " <> toText cid
            Just logfun ->
                withChain v cid p2pConf peer peerDb chainDbDir logfun mempoolConfig $ \c ->
                    go peerDb payloadDb (HM.insert cid c cs) t

    -- Initialize global resources
    go peerDb payloadDb cs [] = do
        let webchain = mkWebBlockHeaderDb v (HM.map _chainBlockHeaderDb cs)
        withConnectionManger (_chainwebNodeLogFun logFuns) peer peerDb $ \mgr ->
            withCuts v cutConfig p2pConf peer peerDb (_chainwebCutLogFun logFuns) webchain mgr $ \cuts ->
                withMiner
                    (_chainwebMinerLogFun logFuns)
                    (_configMiner conf)
                    cwnid
                    (_cutsCutDb cuts)
                    webchain
                    (_webBlockPayloadStoreCas $ _cutsPayloadStore cuts)
                    $ \m -> inner Chainweb
                        { _chainwebChainwebVersion = v
                        , _chainwebHostAddress = _peerConfigAddr $ _p2pConfigPeer $ _configP2p conf
                        , _chainwebChains = cs
                        , _chainwebCuts = cuts
                        , _chainwebNodeId = cwnid
                        , _chainwebMiner = m
                        , _chainwebLogFun = _chainwebNodeLogFun logFuns
                        , _chainwebSocket = socket
                        , _chainwebPeer = peer
                        , _chainwebPayloadDb = payloadDb
                        , _chainwebManager = mgr
                        }

    v = _configChainwebVersion conf
    graph = _chainGraph v
    cids = chainIds_ graph
    cwnid = _configNodeId conf
    p2pConf = _configP2p conf
    chainDbDir = _configChainDbDirPath conf

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        }

-- Starts server and runs all network clients
--
runChainweb :: Chainweb -> IO ()
runChainweb cw = do
    logfun Info "start chainweb node"

    let cutDb = _cutsCutDb $ _chainwebCuts cw
        cutPeerDb = _cutsPeerDb $ _chainwebCuts cw

    -- Startup sequnce:
    --
    -- 1. Start serving Rest API
    -- 2. Start Clients
    --

    -- collect server resources
    let chains = HM.toList (_chainwebChains cw)
        chainVals = map snd chains
        proj :: forall a . (Chain -> a) -> [(ChainId, a)]
        proj f = flip map chains $ \(k, ch) -> (k, f ch)
        chainDbsToServe = proj _chainBlockHeaderDb
        mempoolsToServe = proj _chainMempool
        chainP2pToServe = bimap ChainNetwork _chainPeerDb <$> itoList (_chainwebChains cw)

        payloadDbsToServe = itoList $ const (view chainwebPayloadDb cw) <$> _chainwebChains cw

        serverSettings = peerServerSettings (_chainwebPeer cw)
        serve = serveChainwebSocketTls
            serverSettings
            (_peerCertificate $ _chainwebPeer cw)
            (_peerKey $ _chainwebPeer cw)
            (_chainwebSocket cw)
            (_chainwebChainwebVersion cw)
            ChainwebServerDbs
                { _chainwebServerCutDb = Just cutDb
                , _chainwebServerBlockHeaderDbs = chainDbsToServe
                , _chainwebServerMempools = mempoolsToServe
                , _chainwebServerPayloadDbs = payloadDbsToServe
                , _chainwebServerPeerDbs = (CutNetwork, cutPeerDb) : chainP2pToServe
                }

    -- 1. start server
    --
    withAsync serve $ \server -> do
        logfun Info "started server"

        -- Configure Clients
        --
        let mgr = view chainwebManager cw

        -- 2. Run Clients
        --
        let clients :: [IO ()]
            clients = concat
                [ [runMiner (_chainwebVersion cw) (_chainwebMiner cw)]
                -- FIXME: should we start mining with some delay, so
                -- that the block header base is up to date?
                , cutNetworks mgr (_chainwebCuts cw)
                , map (runChainSyncClient mgr) chainVals
                , map (runMempoolSyncClient mgr) chainVals
                ]

        mapConcurrently_ id clients
        wait server
  where
    logfun = alogFunction @T.Text (_chainwebLogFun cw)

withConnectionManger :: ALogFunction -> Peer -> PeerDb -> (HTTP.Manager -> IO a) -> IO a
withConnectionManger logfun peer peerDb runInner = do
    let cred = unsafeMakeCredential (_peerCertificate peer) (_peerKey peer)
    settings <- certificateCacheManagerSettings
        (TlsSecure True certCacheLookup)
        (Just cred)

    connCountRef <- newIORef (0 :: Int)
    reqCountRef <- newIORef (0 :: Int)
    mgr <- HTTP.newManager settings
        { HTTP.managerConnCount = 5
            -- keep only 5 connections alive
        , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 1000000
            -- timeout connection attempts after 1 sec instead of 30 sec default
        , HTTP.managerIdleConnectionCount = 512
            -- total number of connections to keep alive. 512 is the default

        -- Debugging

        , HTTP.managerTlsConnection = do
            mk <- HTTP.managerTlsConnection settings
            return $ \a b c -> do
                atomicModifyIORef' connCountRef $ (,()) . succ
                mk a b c

        , HTTP.managerModifyRequest = \req -> do
            atomicModifyIORef' reqCountRef $ (,()) . succ
            HTTP.managerModifyRequest settings req
        }

    let logClientConnections = forever $ do
            threadDelay 5000000
            connCount <- readIORef connCountRef
            reqCount <- readIORef reqCountRef
            alogFunction @(JsonLog Value) logfun Debug $ JsonLog $ object
                [ "clientConnectionCount" .= connCount
                , "clientRequestCount" .= reqCount
                ]

    withAsync logClientConnections $ const $ runInner mgr

  where

    certCacheLookup :: ServiceID -> IO (Maybe Fingerprint)
    certCacheLookup si = do
        ha <- serviceIdToHostAddress si
        pe <- getOne . getEQ ha <$> peerDbSnapshot peerDb
        return $ pe >>= fmap peerIdToFingerprint . _peerId . _peerEntryInfo

    serviceIdToHostAddress (h, p) = readHostAddressBytes $ B8.pack h <> ":" <> p
