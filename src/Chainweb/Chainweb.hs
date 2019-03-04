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
, runCutsSyncClient

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
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Utils
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

import qualified Servant.Client as Sv

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
    -> (Cuts -> IO a)
    -> IO a
withCuts v cutDbConfig p2pConfig peer peerDb logfun webchain f =
    withCutDb cutDbConfig (_getLogFunction logfun) webchain $ \cutDb ->
        f $ Cuts v cutDbConfig p2pConfig peer cutDb peerDb logfun

runCutsSyncClient
    :: HTTP.Manager
    -> Cuts
    -> IO ()
runCutsSyncClient mgr cuts = bracket create destroy go
  where
    v = _cutsChainwebVersion cuts
    syncLogg = alogFunction @T.Text (_cutsLogFun cuts)
    create = p2pCreateNode
        v
        CutNetwork
        (_cutsPeer cuts)
        (_getLogFunction $ _cutsLogFun cuts)
        (_cutsPeerDb cuts)
        mgr
        (C.syncSession v (_cutsCutDb cuts))
    go n = do
        syncLogg Info "initialized"
        p2pStartNode (_cutsP2pConfig cuts) n
    destroy n = do
        p2pStopNode n
        syncLogg Info "stopped"

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

-- TODO: unify this with pact transaction type
type ChainwebTransaction = Mempool.MockTx

chainwebTransactionConfig :: Mempool.TransactionConfig ChainwebTransaction
chainwebTransactionConfig = Mempool.TransactionConfig Mempool.mockCodec
    mockHash
    Mempool.chainwebTestHashMeta
    Mempool.mockFees
    Mempool.mockSize
    Mempool.mockMeta
    (const $ return True)
  where
    mockHash = Mempool.chainwebTestHasher . codecEncode Mempool.mockCodec

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
    chainSyncSession db peer depth logg

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

    peerMempool = MPC.toMempool v cid txcfg bslimit env
    pool = _chainMempool chain
    txcfg = Mempool.mempoolTxConfig pool
    bslimit = Mempool.mempoolBlockSizeLimit pool
    cid = _chainChainId chain
    v = _chainChainwebVersion chain


-- -------------------------------------------------------------------------- --
-- Miner

data Miner = Miner
    { _minerLogFun :: !ALogFunction
    , _minerNodeId :: !NodeId
    , _minerCutDb :: !CutDb
    , _minerWebBlockHeaderDb :: !WebBlockHeaderDb
    , _minerConfig :: !MinerConfig
    }

withMiner
    :: ALogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb
    -> WebBlockHeaderDb
    -> (Miner -> IO a)
    -> IO a
withMiner logFun conf nid cutDb webDb inner = inner $ Miner
    { _minerLogFun = logFun
    , _minerNodeId = nid
    , _minerCutDb = cutDb
    , _minerWebBlockHeaderDb = webDb
    , _minerConfig = conf
    }

runMiner :: ChainwebVersion -> Miner -> IO ()
runMiner v m = (chooseMiner v)
    (_getLogFunction $ _minerLogFun m)
    (_minerConfig m)
    (_minerNodeId m)
    (_minerCutDb m)
    (_minerWebBlockHeaderDb m)
  where
    chooseMiner
        :: ChainwebVersion
        -> LogFunction
        -> MinerConfig
        -> NodeId
        -> CutDb
        -> WebBlockHeaderDb
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

mempoolConfig :: Mempool.InMemConfig Mempool.MockTx
mempoolConfig = Mempool.InMemConfig
    chainwebTransactionConfig
    Mempool.mockBlocksizeLimit
    mempoolReapInterval
  where
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
        withCuts v cutConfig p2pConf peer peerDb (_chainwebCutLogFun logFuns) webchain $ \cuts ->
            withMiner (_chainwebMinerLogFun logFuns) (_configMiner conf) cwnid (_cutsCutDb cuts) webchain $ \m ->
                inner Chainweb
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

        payloadDbsToServe = itoList $ const (_chainwebPayloadDb cw) <$> _chainwebChains cw

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
        -- FIXME: make sure the manager is configure properly for our
        -- usage scenario
        --
        let cred = unsafeMakeCredential
                (_peerCertificate $ _chainwebPeer cw)
                (_peerKey $ _chainwebPeer cw)
        settings <- certificateCacheManagerSettings
            (TlsSecure True (certCacheLookup cutPeerDb))
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
                alogFunction @(JsonLog Value) (_chainwebLogFun cw) Debug $ JsonLog $ object
                    [ "clientConnectionCount" .= connCount
                    , "clientRequestCount" .= reqCount
                    ]

        -- 2. Run Clients
        --
        let clients :: [Concurrently ()]
            clients = concatMap (map Concurrently)
                        [ [logClientConnections]
                        , [runMiner (_chainwebVersion cw) (_chainwebMiner cw)]
                        -- FIXME: should we start mining with some delay, so
                        -- that the block header base is up to date?
                        , [runCutsSyncClient mgr (_chainwebCuts cw)]
                        , map (runChainSyncClient mgr) chainVals
                        , map (runMempoolSyncClient mgr) chainVals
                        ]

        void $ runConcurrently $ mconcat clients
        wait server
  where
    logfun = alogFunction @T.Text (_chainwebLogFun cw)

    serviceIdToHostAddress (h, p) = readHostAddressBytes $ B8.pack h <> ":" <> p

    certCacheLookup :: PeerDb -> ServiceID -> IO (Maybe Fingerprint)
    certCacheLookup peerDb si = do
        ha <- serviceIdToHostAddress si
        pe <- getOne . getEQ ha <$> peerDbSnapshot peerDb
        return $ pe >>= fmap peerIdToFingerprint . _peerId . _peerEntryInfo

