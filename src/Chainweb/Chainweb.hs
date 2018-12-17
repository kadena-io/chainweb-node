{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Chainweb
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Chainweb
(
-- * Cut Resources
  Cuts(..)
, cutsChainwebVersion
, cutsCutConfig
, cutsP2pConfig
, cutsCutDb
, cutsPeerDb
, cutsLogger

, withCuts
, runCutsSyncClient

-- * Single Chain Resources
, Chain(..)
, chainChainId
, chainChainwebVersion
, chainChainwebGraph
, chainP2pConfig
, chainBlockHeaderDb
, chainPeerDb
, chainLogger

, withChain
, runChainSyncClient

-- * Chainweb Resources
, Chainweb(..)
, chainwebVersion
, chainwebGraph
, chainwebChains
, chainwebCuts
, chainwebChainwebNodeId
, withChainweb
, runChainweb

-- * Miner
, runMiner

-- * Monitor
, type CutLog
, runMonitor

-- Node
, node
, main
) where

import Configuration.Utils

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch

import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Reflection

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP

import P2P.Node.PeerDB

import qualified Streaming.Prelude as S

import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import qualified Chainweb.CutDB.Sync as C
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Miner.Test
import Chainweb.NodeId
import Chainweb.RestAPI
import Chainweb.RestAPI.NetworkID
import Chainweb.TreeDB.RemoteDB
import Chainweb.TreeDB.Sync
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebChainDB

import Data.DiGraph
import Data.LogMessage

import P2P.Node
import P2P.Node.Configuration
import P2P.Session

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data Cuts = Cuts
    { _cutsChainwebVersion :: !ChainwebVersion
    , _cutsCutConfig :: !CutDbConfig
    , _cutsP2pConfig :: !P2pConfiguration
    , _cutsCutDb :: !CutDb
    , _cutsPeerDb :: !PeerDb
    , _cutsLogger :: !Logger
    }

makeLenses ''Cuts

withCuts
    :: ChainwebVersion
    -> CutDbConfig
    -> P2pConfiguration
    -> Logger
    -> WebChainDb
    -> (Cuts -> IO a)
    -> IO a
withCuts v cutDbConfig p2pConfig logger webchain f =
    L.withLoggerLabel ("component", "cuts") logger $ \logger' -> do
        withCutDb cutDbConfig webchain $ \cutDb ->
            withPeerDb p2pConfig $ \pdb ->
                f $ Cuts v cutDbConfig p2pConfig cutDb pdb logger'

runCutsSyncClient
    :: HTTP.Manager
    -> HostAddress
    -> Cuts
    -> IO ()
runCutsSyncClient mgr ha cuts =
    L.withLoggerLabel ("component", "syncCut") (_cutsLogger cuts) $ \syncLogger -> do
        let syncLogg = loggerFunText syncLogger

        -- Create P2P client node
        n <- L.withLoggerLabel ("component", "p2p") syncLogger $ \sessionLogger ->
            p2pCreateNode
                v
                CutNetwork
                (_cutsP2pConfig cuts)
                (loggerFun sessionLogger)
                (_cutsPeerDb cuts)
                ha
                mgr
                (C.syncSession v (_cutsCutDb cuts))

        -- Run P2P client node
        syncLogg Info "initialized"
        p2pStartNode (_cutsP2pConfig cuts) n `finally` do
            p2pStopNode n
            syncLogg Info "stopped"
  where
    v = _cutsChainwebVersion cuts

-- -------------------------------------------------------------------------- --
-- Single Chain Resources

data Chain = Chain
    { _chainChainId :: !ChainId
    , _chainChainwebVersion :: !ChainwebVersion
    , _chainChainwebGraph :: !ChainGraph
        -- ^ should both be part of a broader Chain configuration?
    , _chainP2pConfig :: !P2pConfiguration
    , _chainBlockHeaderDb :: !BlockHeaderDb
    , _chainPeerDb :: !PeerDb
    , _chainLogger :: !Logger
        -- do we need this here?
    , _chainSyncDepth :: !Depth
    }

makeLenses ''Chain

-- Intializes all local Chain resources, but doesn't start any networking.
--
withChain
    :: ChainwebVersion
    -> ChainGraph
    -> ChainId
    -> P2pConfiguration
    -> Logger
    -> (Chain -> IO a)
    -> IO a
withChain v graph cid p2pConfig logger f =
    L.withLoggerLabel ("chain", toText cid) logger $ \logger' -> do
        withBlockHeaderDb Test graph cid $ \cdb ->
            withPeerDb p2pConfig $ \pdb ->
                f $ Chain cid v graph p2pConfig cdb pdb logger' (syncDepth graph)

-- | Synchronize the local block database over the P2P network.
--
runChainSyncClient
    :: HTTP.Manager
        -- ^ HTTP connection pool
    -> HostAddress
        -- ^ public host address of the local node
    -> Chain
        -- ^ chain resources
    -> IO ()
runChainSyncClient mgr ha chain =
    L.withLoggerLabel ("component", "syncChain") (_chainLogger chain) $ \syncLogger -> do
        let syncLogg = loggerFunText syncLogger

        -- Create P2P client node
        n <- L.withLoggerLabel ("component", "p2p") syncLogger $ \sessionLogger ->
            p2pCreateNode
                (_chainChainwebVersion chain)
                netId
                (_chainP2pConfig chain)
                (loggerFun sessionLogger)
                (_chainPeerDb chain)
                ha
                mgr
                (chainSyncP2pSession (_chainSyncDepth chain) (_chainBlockHeaderDb chain))

        -- Run P2P client node
        syncLogg Info "initialized"
        p2pStartNode (_chainP2pConfig chain) n `finally` do
            p2pStopNode n
            syncLogg Info "stopped"
  where
    netId = ChainNetwork (_chainChainId chain)

chainSyncP2pSession :: BlockHeaderTreeDb db => Depth -> db -> P2pSession
chainSyncP2pSession depth db logg env = do
    peer <- PeerTree <$> remoteDb db env
    chainSyncSession db peer depth logg

syncDepth :: ChainGraph -> Depth
syncDepth g = case diameter g of
    Nothing -> error "Failed to compute diameter of ChainGraph. Most likely the graph is not suitable as chainweb graph"
    Just x -> Depth (2 * x)
{-# NOINLINE syncDepth #-}

-- -------------------------------------------------------------------------- --
-- Miner

runMiner
    :: Logger
    -> MinerConfig
    -> ChainwebNodeId
    -> CutDb
    -> WebChainDb
    -> IO ()
runMiner logger conf nid cutDb webDb =
    L.withLoggerLabel ("component", "miner") logger $ \logger' ->
        miner (loggerFun logger') conf nid cutDb webDb

-- -------------------------------------------------------------------------- --
-- Chainweb Resources

data Chainweb = Chainweb
    { _chainwebVersion :: !ChainwebVersion
    , _chainwebGraph :: !ChainGraph
    , _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId Chain)
    , _chainwebCuts :: !Cuts
    , _chainwebChainwebNodeId :: !ChainwebNodeId
    , _chainwebMiner :: !MinerConfig
    }

makeLenses ''Chainweb

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: ChainGraph
    -> ChainwebConfiguration
    -> Logger
    -> (Chainweb -> IO a)
    -> IO a
withChainweb graph conf logger inner =
    give graph $ go mempty (toList chainIds)
  where
    go cs (cid : t) =
        withChain v graph cid p2pConf logger $ \c ->
            go (HM.insert cid c cs) t
    go cs [] = do
        let webchain = mkWebChainDb graph (HM.map _chainBlockHeaderDb cs)
        withCuts v cutConfig p2pConf logger webchain $ \cuts ->
            inner Chainweb
                { _chainwebVersion = v
                , _chainwebGraph = graph
                , _chainwebHostAddress = _p2pConfigHostAddress (_configP2p conf)
                , _chainwebChains = cs
                , _chainwebCuts = cuts
                , _chainwebChainwebNodeId = cwnid
                , _chainwebMiner = _configMiner conf
                }

    v = _configChainwebVersion conf
    cwnid = _configChainwebNodeId conf
    p2pConf = _configP2p conf

    -- FIXME: make this configurable
    cutConfig = (defaultCutDbConfig v graph)
        { _cutDbConfigLogLevel = Info
        , _cutDbConfigTelemetryLevel = Info
        }

-- Starts server and runs all network clients
--
runChainweb :: Chainweb -> Logger -> IO ()
runChainweb cw logger =
    L.withLoggerLabel ("node", "TODO") logger $ \logger' -> do

    let logfun = loggerFunText logger'
    logfun Info "start chainweb node"

    let cutDb = _cutsCutDb $ _chainwebCuts cw
        cutPeerDb = _cutsPeerDb $ _chainwebCuts cw

    -- Startup sequnce:
    --
    -- 1. Start serving Rest API
    -- 2. Start Clients
    --

    -- collect server resources
    let chainDbsToServe = second _chainBlockHeaderDb <$> HM.toList (_chainwebChains cw)
        chainP2pToServe = bimap ChainNetwork _chainPeerDb <$> itoList (_chainwebChains cw)
        serve = serveChainwebOnPort port (_chainwebVersion cw)
            cutDb
            chainDbsToServe
            ((CutNetwork, cutPeerDb) : chainP2pToServe)

    -- 1. start server
    --
    withAsync serve $ \server -> do
        logfun Info "started server"

        -- FIXME: make sure the manager is configure properly for our
        -- usage scenario
        --
        mgr <- HTTP.newManager HTTP.defaultManagerSettings

        -- 2. Run Clients
        --
        void $ mapConcurrently_ id
            $ runMiner logger' (_chainwebMiner cw)
                (_chainwebChainwebNodeId cw)
                cutDb
                (view cutDbWebChainDb cutDb)
                -- FIXME: should we start mining with some delay, so that
                -- the block header base is up to date?
            : runCutsSyncClient mgr myHostAddress (_chainwebCuts cw)
            : (runChainSyncClient mgr myHostAddress <$> HM.elems (_chainwebChains cw))

        wait server
  where
    port = _hostAddressPort myHostAddress
    myHostAddress = _chainwebHostAddress cw

-- -------------------------------------------------------------------------- --
-- Monitor (doesn't belong here)

type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

runMonitor :: Logger -> CutDb -> IO ()
runMonitor logger db =
    L.withLoggerLabel ("component", "monitor") logger $ \logger' -> do
        let logg = loggerFun logger'
        logg Info $ TextLog "Initialized Monitor"
        void
            $ S.mapM_ (go (loggerFun logger'))
            $ S.map (fmap ObjectEncoded)
            $ S.map _cutMap
            $ cutStream db
  where
    go logg c = void $ logg Info $ JsonLog c

-- -------------------------------------------------------------------------- --
-- Run Node

node :: ChainGraph -> ChainwebConfiguration -> Logger -> IO ()
node graph conf logger =
    withChainweb graph conf logger $ \cw ->
        race_
            (runChainweb cw logger)
            (runMonitor logger (_cutsCutDb $ _chainwebCuts cw))

withNodeLogger :: L.LogConfig -> EnableConfig JsonLoggerConfig -> (Logger -> IO a) -> IO a
withNodeLogger logConfig cutsLoggerConfig f =
    withFileHandleBackend (L._logConfigBackend logConfig) $ \baseBackend ->
        withJsonFileHandleBackend @CutLog cutsLoggerConfig $ \monitorBackend -> do
            let loggerBackend = logHandles
                    [ logHandler monitorBackend
                    ] baseBackend
            L.withLogger (L._logConfigLogger logConfig) loggerBackend f

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configChainwebNodeId :: !ChainwebNodeId
    , _configMiner :: !MinerConfig
    , _configLog :: !L.LogConfig
    , _configCutsLogger :: !(EnableConfig JsonLoggerConfig)
    , _configP2p :: !P2pConfiguration
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

defaultChainwebConfiguration :: ChainwebConfiguration
defaultChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion = Test
    , _configChainwebNodeId = ChainwebNodeId 0 -- FIXME
    , _configMiner = defaultMinerConfig
    , _configLog = L.defaultLogConfig
        & L.logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _configCutsLogger =
        EnableConfig True defaultJsonLoggerConfig
    , _configP2p = defaultP2pConfiguration Test
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "chainwebNodeId" .= _configChainwebNodeId o
        , "miner" .= _configMiner o
        , "log" .= _configLog o
        , "cutsLogger" .= _configCutsLogger o
        , "p2p" .= _configP2p o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configChainwebNodeId ..: "chainwebNodeId" % o
        <*< configMiner %.: "miner" % o
        <*< configLog %.: "log" % o
        <*< configCutsLogger %.: "cutsLogger" % o
        <*< configP2p %.: "p2p" % o

pChainwebConfiguration :: MParser ChainwebConfiguration
pChainwebConfiguration = id
    <$< configChainwebVersion .:: textOption
        % long "chainweb-version"
        <> short 'v'
        <> help "the chainweb version that this node is using"
    <*< configChainwebNodeId .:: textOption
        % long "node-id"
        <> short 'i'
        <> help "unique id of the node that is used as miner id in new blocks"
    <*< configMiner %:: pMinerConfig
    <*< configLog %:: L.pLogConfig
    <*< configCutsLogger %::
        pEnableConfig "cuts-logger" % pJsonLoggerConfig (Just "cuts-")
    <*< configP2p %:: pP2pConfiguration Nothing

-- -------------------------------------------------------------------------- --
-- main (doens't belong here)

mainInfo :: ProgramInfo ChainwebConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pChainwebConfiguration
    defaultChainwebConfiguration

main :: IO ()
main = runWithConfiguration mainInfo $ \conf ->
    withNodeLogger (_configLog conf) (_configCutsLogger conf) $ \logger ->
        node petersonChainGraph conf logger

