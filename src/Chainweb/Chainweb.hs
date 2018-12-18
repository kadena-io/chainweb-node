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
, configChainwebNodeId
, configChainwebVersion
, configMiner
, configP2p
, defaultChainwebConfiguration
, pChainwebConfiguration

-- * Cut Resources
, Cuts(..)
, cutsChainwebVersion
, cutsCutConfig
, cutsP2pConfig
, cutsCutDb
, cutsPeerDb
, cutsLogFun

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
, chainLogFun
, chainSyncDepth

, withChain
, runChainSyncClient

-- * Chainweb Logging Functions
, ChainwebLogFunctions(..)
, chainwebNodeLogFun
, chainwebMinerLogFun
, chainwebCutLogFun
, chainwebChainLogFuns

-- * Chainweb Resources
, Chainweb(..)
, chainwebVersion
, chainwebGraph
, chainwebChains
, chainwebCuts
, chainwebChainwebNodeId
, chainwebHostAddress
, chainwebMiner
, chainwebLogFun

, withChainweb
, runChainweb

-- * Miner
, runMiner

) where

import Configuration.Utils

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch

import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Reflection (give)
import Data.String
import qualified Data.Text as T

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp

import System.LogLevel

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
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
import P2P.Node.PeerDB
import P2P.Session

-- -------------------------------------------------------------------------- --
-- Cuts Resources

data Cuts = Cuts
    { _cutsChainwebVersion :: !ChainwebVersion
    , _cutsCutConfig :: !CutDbConfig
    , _cutsP2pConfig :: !P2pConfiguration
    , _cutsCutDb :: !CutDb
    , _cutsPeerDb :: !PeerDb
    , _cutsLogFun :: !ALogFunction
    }

makeLenses ''Cuts

withCuts
    :: ChainwebVersion
    -> CutDbConfig
    -> P2pConfiguration
    -> ALogFunction
    -> WebChainDb
    -> (Cuts -> IO a)
    -> IO a
withCuts v cutDbConfig p2pConfig logfun webchain f =
    withCutDb cutDbConfig webchain $ \cutDb ->
        withPeerDb p2pConfig $ \pdb ->
            f $ Cuts v cutDbConfig p2pConfig cutDb pdb logfun

runCutsSyncClient
    :: HTTP.Manager
    -> HostAddress
    -> Cuts
    -> IO ()
runCutsSyncClient mgr ha cuts = do
    -- Create P2P client node
    n <- p2pCreateNode
        v
        CutNetwork
        (_cutsP2pConfig cuts)
        (_getLogFunction $ _cutsLogFun cuts)
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
    syncLogg = alogFunction @T.Text (_cutsLogFun cuts)

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
    , _chainLogFun :: !ALogFunction
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
    -> ALogFunction
    -> (Chain -> IO a)
    -> IO a
withChain v graph cid p2pConfig logfun inner =
    withBlockHeaderDb Test graph cid $ \cdb ->
        withPeerDb p2pConfig $ \pdb -> inner
            $ Chain cid v graph p2pConfig cdb pdb logfun (syncDepth graph)

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
runChainSyncClient mgr ha chain = do
    -- Create P2P client node
    n <- p2pCreateNode
        (_chainChainwebVersion chain)
        netId
        (_chainP2pConfig chain)
        (_getLogFunction $ _chainLogFun chain)
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
    syncLogg = alogFunction @T.Text (_chainLogFun chain)

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

data Miner = Miner
    { _minerLogFun :: !ALogFunction
    , _minerNodeId :: !ChainwebNodeId
    , _minerCutDb :: !CutDb
    , _minerWebChainDb :: !WebChainDb
    , _minerConfig :: !MinerConfig
    }

withMiner
    :: ALogFunction
    -> MinerConfig
    -> ChainwebNodeId
    -> CutDb
    -> WebChainDb
    -> (Miner -> IO a)
    -> IO a
withMiner logFun conf nid cutDb webDb inner = inner $ Miner
    { _minerLogFun = logFun
    , _minerNodeId = nid
    , _minerCutDb = cutDb
    , _minerWebChainDb = webDb
    , _minerConfig = conf
    }

runMiner :: Miner -> IO ()
runMiner m =
    miner
        (_getLogFunction $ _minerLogFun m)
        (_minerConfig m)
        (_minerNodeId m)
        (_minerCutDb m)
        (_minerWebChainDb m)

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
    { _chainwebVersion :: !ChainwebVersion
    , _chainwebGraph :: !ChainGraph
    , _chainwebHostAddress :: !HostAddress
    , _chainwebChains :: !(HM.HashMap ChainId Chain)
    , _chainwebCuts :: !Cuts
    , _chainwebChainwebNodeId :: !ChainwebNodeId
    , _chainwebMiner :: !Miner
    , _chainwebLogFun :: !ALogFunction
    }

makeLenses ''Chainweb

-- Intializes all local chainweb components but doesn't start any networking.
--
withChainweb
    :: ChainGraph
    -> ChainwebConfiguration
    -> ChainwebLogFunctions
    -> (Chainweb -> IO a)
    -> IO a
withChainweb graph conf logFuns inner =
    give graph $ go mempty (toList chainIds)
  where
    -- Initialize chain resources
    go cs (cid : t) = do
        case HM.lookup cid (_chainwebChainLogFuns logFuns) of
            Nothing -> error $ T.unpack
                $ "Failed to initialize chainweb node: missing log function for chain " <> toText cid
            Just logfun -> withChain v graph cid p2pConf logfun $ \c ->
                go (HM.insert cid c cs) t

    -- Initialize global resources
    go cs [] = do
        let webchain = mkWebChainDb graph (HM.map _chainBlockHeaderDb cs)
        withCuts v cutConfig p2pConf (_chainwebCutLogFun logFuns) webchain $ \cuts ->
            withMiner (_chainwebMinerLogFun logFuns) (_configMiner conf) cwnid (_cutsCutDb cuts) webchain $ \m ->
                inner Chainweb
                    { _chainwebVersion = v
                    , _chainwebGraph = graph
                    , _chainwebHostAddress = _p2pConfigHostAddress (_configP2p conf)
                    , _chainwebChains = cs
                    , _chainwebCuts = cuts
                    , _chainwebChainwebNodeId = cwnid
                    , _chainwebMiner = m
                    , _chainwebLogFun = _chainwebNodeLogFun logFuns
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
    let chainDbsToServe = second _chainBlockHeaderDb <$> HM.toList (_chainwebChains cw)
        chainP2pToServe = bimap ChainNetwork _chainPeerDb <$> itoList (_chainwebChains cw)

        serverSettings = setPort (int port) . setHost host $ defaultSettings
        serve = serveChainweb serverSettings (_chainwebVersion cw)
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
            $ runMiner (_chainwebMiner cw)
                -- FIXME: should we start mining with some delay, so that
                -- the block header base is up to date?
            : runCutsSyncClient mgr myHostAddress (_chainwebCuts cw)
            : (runChainSyncClient mgr myHostAddress <$> HM.elems (_chainwebChains cw))

        wait server
  where
    host = fromString $ sshow $ _hostAddressHost myHostAddress
    port = _hostAddressPort myHostAddress
    myHostAddress = _chainwebHostAddress cw
    logfun = alogFunction @T.Text (_chainwebLogFun cw)

-- -------------------------------------------------------------------------- --
-- Chainweb Configuration

data ChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion :: !ChainwebVersion
    , _configChainwebNodeId :: !ChainwebNodeId
    , _configMiner :: !MinerConfig
    , _configP2p :: !P2pConfiguration
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebConfiguration

defaultChainwebConfiguration :: ChainwebConfiguration
defaultChainwebConfiguration = ChainwebConfiguration
    { _configChainwebVersion = Test
    , _configChainwebNodeId = ChainwebNodeId 0 -- FIXME
    , _configMiner = defaultMinerConfig
    , _configP2p = defaultP2pConfiguration Test
    }

instance ToJSON ChainwebConfiguration where
    toJSON o = object
        [ "chainwebVersion" .= _configChainwebVersion o
        , "chainwebNodeId" .= _configChainwebNodeId o
        , "miner" .= _configMiner o
        , "p2p" .= _configP2p o
        ]

instance FromJSON (ChainwebConfiguration -> ChainwebConfiguration) where
    parseJSON = withObject "ChainwebConfig" $ \o -> id
        <$< configChainwebVersion ..: "chainwebVersion" % o
        <*< configChainwebNodeId ..: "chainwebNodeId" % o
        <*< configMiner %.: "miner" % o
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
    <*< configP2p %:: pP2pConfiguration Nothing

