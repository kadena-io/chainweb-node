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
{-# LANGUAGE TypeFamilies #-}

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

import Configuration.Utils hiding (Error, (<.>), Lens')

import Control.Concurrent
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import Data.Function
import qualified Data.HashSet as HS
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import qualified Streaming.Prelude as SP

import System.FilePath
import qualified System.Logger as L
import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Graph
import Chainweb.Node.SingleChainMiner
import Chainweb.NodeId
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.Test.P2P.Peer.BootstrapConfig (bootstrapPeerConfig)
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

import Utils.Gexf
import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data P2pExampleConfig = P2pExampleConfig
    { _numberOfNodes :: !Natural
    , _maxSessionCount :: !Natural
    , _maxPeerCount :: !Natural
    , _sessionTimeoutSeconds :: !Natural
    , _meanSessionSeconds :: !Natural
    , _meanBlockTimeSeconds :: !Natural
    , _exampleChainId :: !ChainId
    , _logConfig :: !L.LogConfig
    , _sessionsLoggerConfig :: !(EnableConfig JsonLoggerConfig)
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''P2pExampleConfig

defaultP2pExampleConfig :: P2pExampleConfig
defaultP2pExampleConfig = P2pExampleConfig
    { _numberOfNodes = 10
    , _maxSessionCount =  6
    , _maxPeerCount = 50
    , _sessionTimeoutSeconds = 40
    , _meanSessionSeconds = 20
    , _meanBlockTimeSeconds = 10
    , _exampleChainId = testChainId 0
    , _logConfig = L.defaultLogConfig
        & L.logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _sessionsLoggerConfig = EnableConfig True defaultJsonLoggerConfig
    }

instance ToJSON P2pExampleConfig where
    toJSON o = object
        [ "numberOfNodes" .= _numberOfNodes o
        , "maxSessionCount" .= _maxSessionCount o
        , "maxPeerCount" .= _maxPeerCount o
        , "sessionTimoutSeconds" .= _sessionTimeoutSeconds o
        , "meanSessionSeconds" .= _meanSessionSeconds o
        , "meanBlockTimeSeconds" .= _meanBlockTimeSeconds o
        , "exampleChainId" .= _exampleChainId o
        , "logConfig" .= _logConfig o
        , "sessionsLoggerConfig" .= _sessionsLoggerConfig o
        ]

instance FromJSON (P2pExampleConfig -> P2pExampleConfig) where
    parseJSON = withObject "P2pExampleConfig" $ \o -> id
        <$< numberOfNodes ..: "numberOfNodes" % o
        <*< maxSessionCount ..: "maxSessionCount" % o
        <*< maxPeerCount ..: "maxPeerCount" % o
        <*< sessionTimeoutSeconds ..: "sessionTimeoutSeconds" % o
        <*< meanSessionSeconds ..: "meanSessionSeconds" % o
        <*< meanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< exampleChainId ..: "exampleChainId" % o
        <*< logConfig %.: "logConfig" % o
        <*< sessionsLoggerConfig %.: "sessionsLoggerConfig" % o

pP2pExampleConfig :: MParser P2pExampleConfig
pP2pExampleConfig = id
    <$< numberOfNodes .:: option auto
        % long "number-of-nodes"
        <> short 'n'
        <> help "number of nodes to run in the example"
    <*< maxSessionCount .:: option auto
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
    <*< exampleChainId .:: option auto
        % long "chainid"
        <> short 'c'
        <> help "the chain id that is used in the example"
    <*< logConfig %:: L.pLogConfig
    <*< sessionsLoggerConfig %::
        pEnableConfig "sessions-logger" % pJsonLoggerConfig (Just "sessions-")

-- | How deep in the past from the current highest block that we wish to sync.
--
-- This is a single chain, therefore a singleton graph of diameter 1, but we'd
-- still like Sync to check a little deeper into the past.
--
syncDepth :: Depth
syncDepth = Depth 6

-- -------------------------------------------------------------------------- --
-- Main

mainInfo :: ProgramInfo P2pExampleConfig
mainInfo = programInfo "P2P Example" pP2pExampleConfig defaultP2pExampleConfig

main :: IO ()
main = runWithConfiguration mainInfo $ \config -> do
    staticDir <- (<> "/examples/static-html") <$> getDataDir
    withExampleLogger 8000
        (_logConfig config)
        (_sessionsLoggerConfig config)
        staticDir
        (example config)

-- -------------------------------------------------------------------------- --
-- Example

example :: P2pExampleConfig -> Logger -> IO ()
example conf logger =
    withAsync (node cid t logger conf bootstrapConfig bootstrapChainNodeId)
        $ \bootstrap -> do
            mapConcurrently_ (node cid t logger conf p2pConfig) nodeIds
            wait bootstrap

  where
    cid = _exampleChainId conf
    t = _meanSessionSeconds conf

    -- P2P node configuration
    --
    p2pConfig = (defaultP2pConfiguration Test)
        { _p2pConfigMaxSessionCount = _maxSessionCount conf
        , _p2pConfigMaxPeerCount = _maxPeerCount conf
        , _p2pConfigSessionTimeout = fromIntegral $ _sessionTimeoutSeconds conf
        }

    -- Configuration for bootstrap node
    --
    bootstrapConfig = set p2pConfigPeer (head $ bootstrapPeerConfig Test) p2pConfig
    bootstrapChainNodeId = ChainNodeId cid 0

    -- Other nodes
    --
    nodeIds = ChainNodeId cid <$> [1 .. int (_numberOfNodes conf) - 1]

-- -------------------------------------------------------------------------- --
-- Example P2P Client Sessions

timer :: Natural -> IO ()
timer t = do
    gen <- MWC.createSystemRandom
    timeout <- MWC.geometric1 (1 / (fromIntegral t * 1000000)) gen
    threadDelay timeout

chainDbSyncSession :: BlockHeaderTreeDb db => Natural -> db -> P2pSession
chainDbSyncSession t db logFun env = do
    peer <- PeerTree <$> remoteDb db env
    withAsync (timer t) $ \timerAsync ->
      withAsync (syncSession db peer syncDepth logFun) $ \sessionAsync ->
        waitEitherCatchCancel timerAsync sessionAsync >>= \case
            Left (Left e) -> do
                logg Info $ "session timer failed " <> sshow e
                return False
            Left (Right ()) -> do
                logg Info "session killed by timer"
                return False
            Right (Left e) -> do
                logg Warn $ "Session failed: " <> sshow e
                return False
            Right (Right a) -> do
                logg Warn "Session succeeded"
                return a
  where
    logg :: LogFunctionText
    logg = logFun

-- -------------------------------------------------------------------------- --
-- Test Node

node
    :: ChainId
    -> Natural
    -> Logger
    -> P2pExampleConfig
    -> P2pConfiguration
    -> ChainNodeId
    -> IO ()
node cid t logger conf p2pConfig nid =
    L.withLoggerLabel ("node", toText nid) logger $ \logger' -> do

        let logfun = loggerFunText logger'
        logfun Info "start test node"

        (c, sock, peer) <- allocatePeer $ _p2pConfigPeer p2pConfig
        let p2pConfig' = set p2pConfigPeer c p2pConfig
        let settings = peerServerSettings peer
        let serve cdb pdb = serveSingleChainSocket settings sock Test
                [(cid, cdb)]
                [(ChainNetwork cid, pdb)]

        withBlockHeaderDbGexf Test singletonChainGraph cid nid $ \cdb ->
            withPeerDb (HS.singleton $ ChainNetwork cid) p2pConfig' $ \pdb ->
                withAsync (serve cdb pdb) $ \server -> do
                    logfun Info "started server"
                    runConcurrently
                        $ Concurrently (singleChainMiner logger' minerConfig nid cdb)
                        <> Concurrently (syncer cid logger' p2pConfig' peer cdb pdb t)
                        <> Concurrently (monitor logger' cdb)
                    wait server
  where
    minerConfig = SingleChainMinerConfig
        (_numberOfNodes conf * _meanBlockTimeSeconds conf)
            -- We multiply these together, since this is now the mean time per node.
        cid

withBlockHeaderDbGexf
    :: ChainwebVersion
    -> ChainGraph
    -> ChainId
    -> ChainNodeId
    -> (BlockHeaderDb -> IO b)
    -> IO b
withBlockHeaderDbGexf v graph cid nid f =
    withBlockHeaderDb v graph cid $ \db -> f db `finally` do
        l <- SP.toList_ $ entries db Nothing Nothing Nothing Nothing
        B8.writeFile ("headersgraph" <.> nidPath <.> "tmp.gexf") $ blockHeaders2gexf l
  where
    nidPath = T.unpack . T.replace "/" "." $ toText nid

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
    -> Natural
    -> IO ()
syncer cid logger conf peer cdb pdb t =
    L.withLoggerLabel ("component", "syncer") logger $ \syncLogger -> do
        let syncLogg = loggerFunText syncLogger

        -- Create P2P client node
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        n <- L.withLoggerLabel ("component", "syncer/p2p") logger $ \sessionLogger ->
            p2pCreateNode Test nid peer (loggerFun sessionLogger) pdb mgr (chainDbSyncSession t cdb)

        -- Run P2P client node
        syncLogg Info "initialized syncer"
        p2pStartNode conf n `finally` do
            p2pStopNode n
            syncLogg Info "stopped syncer"

  where
    nid = ChainNetwork cid

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
            & SP.foldM_ (\stat _ -> go (loggerFun logger') stat) mempty return
  where
    go logg stat = do
        bs <- SP.length_ $ leafEntries db Nothing Nothing Nothing Nothing
        mh <- maxHeader db

        let stat' = stat <> Stats
                { _chainHeight = rank mh
                , _branchCount = int bs
                , _branchHeightHistogram = []
                , _blockHeaderCount = 1
                }

        void $ logg Info $ JsonLog stat'
        return stat'
