{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: ChainwebNode
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
(
-- * Configuration
  ChainwebNodeConfiguration(..)

-- * Monitor
, runMonitor

-- * Chainweb Node
, node
, withNodeLogger

-- * Main function
, main
) where

import Configuration.Utils

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad

import GHC.Generics hiding (from)

import qualified Network.HTTP.Client as HTTP

import qualified Streaming.Prelude as S

import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.Chainweb
import Chainweb.Chainweb.CutResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

import Data.CAS.HashMap
import Data.LogMessage

import P2P.Node

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb :: !ChainwebConfiguration
    , _nodeConfigLog :: !L.LogConfig
    , _nodeConfigTelemetryLogger :: !(EnableConfig JsonLoggerConfig)
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebNodeConfiguration

defaultChainwebNodeConfiguration :: ChainwebVersion -> ChainwebNodeConfiguration
defaultChainwebNodeConfiguration v = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = L.defaultLogConfig
        & L.logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigTelemetryLogger =
        EnableConfig True defaultJsonLoggerConfig
    }

instance ToJSON ChainwebNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "log" .= _nodeConfigLog o
        , "telemetryLogger" .= _nodeConfigTelemetryLogger o
        ]

instance FromJSON (ChainwebNodeConfiguration -> ChainwebNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "log" % o
        <*< nodeConfigTelemetryLogger %.: "telemetryLogger" % o

pChainwebNodeConfiguration :: MParser ChainwebNodeConfiguration
pChainwebNodeConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: L.pLogConfig
    <*< nodeConfigTelemetryLogger %::
        pEnableConfig "telemetry-logger" % pJsonLoggerConfig (Just "telemetry-")

-- -------------------------------------------------------------------------- --
-- Monitor


runMonitor :: Logger logger => logger -> CutDb cas -> IO ()
runMonitor logger db =
    L.withLoggerLabel ("component", "monitor") logger $ \logger' -> do
        logFunctionText logger' Info $ "Initialized Monitor"
        void
            $ S.mapM_ (logFunctionJson logger' Info)
            $ S.map (cutToCutHashes Nothing)
            $ cutStream db

            -- This logs complete cuts, which is much more data
            -- $ S.mapM_ (logFunctionJson logger' Info)
            -- $ S.map (fmap ObjectEncoded)
            -- $ S.map _cutMap
            -- $ cutStream db

-- type CutLog = HM.HashMap ChainId (ObjectEncoded BlockHeader)

-- -------------------------------------------------------------------------- --
-- Run Node

node :: Logger logger => ChainwebConfiguration -> logger -> IO ()
node conf logger =
    withChainweb @HashMapCas conf logger $ \cw -> race_
        (runChainweb cw)
        (runMonitor (_chainwebLogger cw) (_cutResCutDb $ _chainwebCutResources cw))

withNodeLogger
    :: L.LogConfig
    -> EnableConfig JsonLoggerConfig
    -> (L.Logger SomeLogMessage -> IO a)
    -> IO a
withNodeLogger logConfig telemetryLoggerConfig f = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
        -- This manager is used only for telmetry
    withFileHandleBackend (L._logConfigBackend logConfig) $ \baseBackend ->

        -- Telemetry Backends
        withJsonFileHandleBackend @CutHashes mgr telemetryLoggerConfig $ \monitorBackend ->
        withJsonFileHandleBackend @P2pSessionInfo mgr telemetryLoggerConfig $ \p2pInfoBackend ->
        withJsonFileHandleBackend @ConnectionManagerStats mgr telemetryLoggerConfig $ \managerBackend -> do
            let loggerBackend = logHandles
                    [ logHandler monitorBackend
                    , logHandler p2pInfoBackend
                    , logHandler managerBackend
                    ] baseBackend
            L.withLogger (L._logConfigLogger logConfig) loggerBackend f

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration (TestWithTime petersonChainGraph))

main :: IO ()
main = runWithConfiguration mainInfo $ \conf ->
    withNodeLogger (_nodeConfigLog conf) (_nodeConfigTelemetryLogger conf) $
        node (_nodeConfigChainweb conf)

