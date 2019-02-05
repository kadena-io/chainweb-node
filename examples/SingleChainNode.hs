{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Configuration.Utils hiding (Error, (<.>))

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import Data.Bifunctor (first)
import Data.Function
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Word

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import qualified Streaming.Prelude as S

import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Mempool.InMem (InMemConfig(..), withInMemoryMempool)
import Chainweb.Node.SingleChainMiner
import Chainweb.NodeId
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB (remoteDb)
import Chainweb.TreeDB.Sync
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

import P2P.Node
import P2P.Node.Configuration
import P2P.Node.PeerDB
import P2P.Peer
import P2P.Session

import Paths_chainweb

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data P2pNodeConfig = P2pNodeConfig
    { _maxSessionCount :: !Natural
    , _maxPeerCount :: !Natural
    , _sessionTimeoutSeconds :: !Natural
    , _meanSessionSeconds :: !Natural
    , _meanBlockTimeSeconds :: !Natural
    , _nodeChainId :: !ChainId
    , _nodePeerId :: !(Maybe PeerId)
    , _nodePort :: !Word16
    , _telemetryPort :: !Word16
    , _remotePeerId :: !(Maybe PeerId)
    , _remotePeerAddress :: !(Maybe HostAddress)
    , _logConfig :: !L.LogConfig
    , _sessionsLoggerConfig :: !(EnableConfig JsonLoggerConfig)
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''P2pNodeConfig

defaultP2pNodeConfig :: P2pNodeConfig
defaultP2pNodeConfig = P2pNodeConfig
    { _maxSessionCount =  6
    , _maxPeerCount = 50
    , _sessionTimeoutSeconds = 40
    , _meanSessionSeconds = 20
    , _meanBlockTimeSeconds = 10
    , _nodeChainId = testChainId 0
    , _nodePeerId = Nothing
    , _nodePort = 45000
    , _telemetryPort = 8000
    , _remotePeerAddress = Nothing
    , _remotePeerId = Nothing
    , _logConfig = L.defaultLogConfig
        & L.logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _sessionsLoggerConfig = EnableConfig True defaultJsonLoggerConfig
    }

instance ToJSON P2pNodeConfig where
    toJSON o = object
        [ "maxSessionCount" .= _maxSessionCount o
        , "maxPeerCount" .= _maxPeerCount o
        , "sessionTimoutSeconds" .= _sessionTimeoutSeconds o
        , "meanSessionSeconds" .= _meanSessionSeconds o
        , "meanBlockTimeSeconds" .= _meanBlockTimeSeconds o
        , "nodeChainId" .= _nodeChainId o
        , "nodePeerId" .= _nodePeerId o
        , "nodePort" .= _nodePort o
        , "telemetryPort" .= _telemetryPort o
        , "remotePeerAddress" .= _remotePeerAddress o
        , "remotePeerId" .= _remotePeerId o
        , "logConfig" .= _logConfig o
        , "sessionsLoggerConfig" .= _sessionsLoggerConfig o
        ]

instance FromJSON (P2pNodeConfig -> P2pNodeConfig) where
    parseJSON = withObject "P2pNodeConfig" $ \o -> id
        <$< maxSessionCount ..: "maxSessionCount" % o
        <*< maxPeerCount ..: "maxPeerCount" % o
        <*< sessionTimeoutSeconds ..: "sessionTimeoutSeconds" % o
        <*< meanSessionSeconds ..: "meanSessionSeconds" % o
        <*< meanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< nodeChainId ..: "nodeChainId" % o
        <*< nodePeerId ..: "nodePeerId" % o
        <*< nodePort ..: "nodePort" % o
        <*< telemetryPort ..: "telemetryPort" % o
        <*< remotePeerAddress ..: "remotePeerAddress" % o
        <*< remotePeerId ..: "remotePeerId" % o
        <*< logConfig %.: "logConfig" % o
        <*< sessionsLoggerConfig %.: "sessionsLoggerConfig" % o

pP2pNodeConfig :: MParser P2pNodeConfig
pP2pNodeConfig = id
    <$< maxSessionCount .:: option auto
        % long "max-session-count"
        <> short 'm'
        <> help "maximum number of sessions that are active at any time"
    <*< maxPeerCount .:: option auto
        % long "max-peer-count"
        <> short 'p'
        <> help "maximum number of entries in the peer database"
    <*< sessionTimeoutSeconds .:: option auto
        % long "session-timeout"
        <> short 's'
        <> help "timeout for sessions in seconds"
    <*< meanSessionSeconds .:: option auto
        % long "mean-session-time"
        <> short 't'
        <> help "mean time of a session in seconds"
    <*< meanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "mean time for mining a block seconds"
    <*< nodeChainId .:: option auto
        % long "chainid"
        <> short 'c'
        <> help "the chain id that the node runs on"
    <*< nodePeerId .:: option (Just <$> eitherReader (first show . peerIdFromText . T.pack))
        % long "peerid"
        <> short 'i'
        <> help "the peer id for this node"
    <*< remotePeerId .:: option (Just <$> eitherReader (first show . peerIdFromText . T.pack))
        % long "remote-peerid"
        <> short 'r'
        <> help "the peer id of an initial remote node to connect to"
    <*< remotePeerAddress .:: option (Just <$> eitherReader (first show . hostAddressFromText . T.pack))
        % long "remote-address"
        <> short 'a'
        <> help "the address of an initial remote node to connect to"
    <*< nodePort .:: option auto
        % long "port"
        <> short 'p'
        <> help "the TCP port that the node uses"
    <*< telemetryPort .:: option auto
        % long "telemetry-port"
        <> short 't'
        <> help "the TCP port used to serve telemetry"
    <*< logConfig %:: L.pLogConfig
    <*< sessionsLoggerConfig %::
        pEnableConfig "sessions-logger" % pJsonLoggerConfig (Just "sessions-")

-- | How deep in the past from the current highest block that we wish to sync.
--
-- This is a single chain, therefore a singleton graph of diameter 1, but we'd
-- still like Sync to check a little deeper into the past. This will have to be
-- changed once we have real multi-chain mining, and the graph is made a
-- Peterson graph.
--
syncDepth :: Depth
syncDepth = Depth 6

-- -------------------------------------------------------------------------- --
-- Main

mainInfo :: ProgramInfo P2pNodeConfig
mainInfo = programInfo "ChainwebNode" pP2pNodeConfig defaultP2pNodeConfig

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
    staticDir <- (<> "/examples/static-html") <$> getDataDir
    withExampleLogger (fromIntegral $ _telemetryPort config)
        (_logConfig config)
        (_sessionsLoggerConfig config)
        staticDir
        (runNodeWithConfig config)

peerIdToChainNodeId :: HasChainId cid => cid -> UUID.UUID -> ChainNodeId
peerIdToChainNodeId cid uuid = ChainNodeId (_chainId cid) (int a * 2^(32 :: Integer) + int b)
  where
    (a,b,_,_) = UUID.toWords uuid

runNodeWithConfig :: P2pNodeConfig -> Logger -> IO ()
runNodeWithConfig conf logger =
    L.withLoggerLabel ("node", "init") logger $ \logger' -> do
        let logfun = loggerFunText logger'
        uid <- UUID.nextRandom
        logfun Info $ sshow uid
        node cid logger conf p2pConfig (peerIdToChainNodeId cid uid)
  where
    cid = _nodeChainId conf

    -- P2P node configuration
    --
    p2pConfig = (defaultP2pConfiguration Test)
        { _p2pConfigMaxSessionCount = _maxSessionCount conf
        , _p2pConfigMaxPeerCount = _maxPeerCount conf
        , _p2pConfigSessionTimeout = fromIntegral $ _sessionTimeoutSeconds conf
        , _p2pConfigKnownPeers = case (_remotePeerId conf, _remotePeerAddress conf) of
            (Just pid, Just addr) -> [PeerInfo (Just pid) addr]
            _ -> []
        }

-- -------------------------------------------------------------------------- --
-- P2P Client Sessions

chainDbSyncSession :: BlockHeaderDb -> P2pSession
chainDbSyncSession db logFun env = do
    peer <- PeerTree <$> remoteDb db logFun env
    try (syncSession db peer syncDepth logFun) >>= \case
      Left (e :: SomeException) -> do
        logg Warn $ "Session failed: " <> sshow e
        return False
      Right a -> do
        logg Warn "Session succeeded"
        return a
  where
    logg :: LogFunctionText
    logg = logFun

-- -------------------------------------------------------------------------- --
-- Test Node

node
    :: ChainId
    -> Logger
    -> P2pNodeConfig
    -> P2pConfiguration
    -> ChainNodeId
    -> IO ()
node cid logger conf p2pConfig nid =
    L.withLoggerLabel ("node", toText nid) logger $ \logger' -> do

        let logfun = loggerFunText logger'
        logfun Info $ "Start test node"

        (c, sock, peer) <- allocatePeer $ _p2pConfigPeer p2pConfig
        let p2pConfig' = set p2pConfigPeer c p2pConfig
        let settings = peerServerSettings peer
        let serve cdb pdb mempool = serveSingleChainSocket settings sock Test
                [(cid, cdb, mempool)]
                [(ChainNetwork cid, pdb)]

        withBlockHeaderDb Test singletonChainGraph cid $ \cdb ->
            withPeerDb (HS.singleton $ ChainNetwork cid) p2pConfig' $ \pdb ->
            withInMemoryMempool mempoolConfig $ \mempool ->
            withAsync (serve cdb pdb mempool) $ \server -> do
                logfun Info "started server"
                runConcurrently
                    $ Concurrently (miner logger' conf nid cdb)
                    <> Concurrently (syncer cid logger' p2pConfig' peer cdb pdb)
                    <> Concurrently (monitor logger' cdb)
                wait server
  where
    blockSizeLimit = 100000     -- fixme
    reaperInterval = 60 * 60 * 1000000   -- one hour
    mempoolConfig = InMemConfig chainwebTransactionConfig blockSizeLimit reaperInterval

-- -------------------------------------------------------------------------- --
-- Syncer

-- | Synchronized the local block database copy over the P2P network.
--
syncer
    :: ChainId
    -> Logger
    -> P2pConfiguration
    -> Peer
    -> BlockHeaderDb
    -> PeerDb
    -> IO ()
syncer cid logger conf peer cdb pdb =
    L.withLoggerLabel ("component", "syncer") logger $ \syncLogger -> do
        let syncLogg = loggerFunText syncLogger

        -- Create P2P client node
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        n <- L.withLoggerLabel ("component", "syncer/p2p") logger $ \sessionLogger ->
            p2pCreateNode Test nid peer (loggerFun sessionLogger) pdb mgr (chainDbSyncSession cdb)

        -- Run P2P client node
        syncLogg Info "initialized syncer"
        p2pStartNode conf n `finally` do
            p2pStopNode n
            syncLogg Info "stopped syncer"

  where
    nid = ChainNetwork cid

-- -------------------------------------------------------------------------- --
-- Miner

-- | A miner creates new blocks headers on the top of the longest branch in
-- the chain database with a mean rate of meanBlockTimeSeconds. Mind blocks
-- are added to the database.
--
-- For testing the difficulty is trivial, so that the target is 'maxBound' and
-- each nonce if accepted. Block creation is delayed through through
-- 'threadDelay' with an geometric distribution.
--
miner :: Logger -> P2pNodeConfig -> ChainNodeId -> BlockHeaderDb -> IO ()
miner logger conf nid db = singleChainMiner logger conf' nid db
    where
        conf' = SingleChainMinerConfig
            { _configMeanBlockTimeSeconds = _meanBlockTimeSeconds conf
            , _configChainId = _nodeChainId conf
            }

-- -------------------------------------------------------------------------- --
-- Monitor

data Stats = Stats
    { _chainHeight :: !Natural
    , _branchCount :: !Natural
    , _branchHeightHistogram :: ![Natural] -- not yet implemented
    , _blockHeaderCount :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, NFData)

instance Semigroup Stats where
    a <> b = Stats
        { _chainHeight = (max `on` _chainHeight) a b
        , _branchCount = (max `on` _branchCount) a b
        , _branchHeightHistogram = (zipWith (+) `on` _branchHeightHistogram) a b
        , _blockHeaderCount = ((+) `on` _blockHeaderCount) a b
        }

instance Monoid Stats where
    mempty = Stats 0 0 [] 0
    mappend = (<>)

-- | Collects statistics about local block database copy
--
monitor :: Logger -> BlockHeaderDb -> IO ()
monitor logger db =
    L.withLoggerLabel ("component", "monitor") logger $ \logger' -> do
        let logg = loggerFun logger'
        logg Info $ TextLog "Initialized Monitor"
        void $ allEntries db Nothing
            & S.foldM_ (\stat _ -> go (loggerFun logger') stat) mempty return
  where
    go logg stat = do
        bs <- S.length_ $ leafEntries db Nothing Nothing Nothing Nothing
        mh <- maxHeader db

        let stat' = stat <> Stats
                { _chainHeight = rank mh
                , _branchCount = int bs
                , _branchHeightHistogram = []
                , _blockHeaderCount = 1
                }

        void $ logg Info $ JsonLog stat'
        return stat'
