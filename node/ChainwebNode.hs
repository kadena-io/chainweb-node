{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Control.Monad.Managed

import Data.Typeable

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
import Utils.Logging.Config

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb :: !ChainwebConfiguration
    , _nodeConfigLog :: !LogConfig
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebNodeConfiguration

defaultChainwebNodeConfiguration :: ChainwebVersion -> ChainwebNodeConfiguration
defaultChainwebNodeConfiguration v = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = defaultLogConfig
        & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    }

instance ToJSON ChainwebNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "logging" .= _nodeConfigLog o
        ]

instance FromJSON (ChainwebNodeConfiguration -> ChainwebNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "logging" % o

pChainwebNodeConfiguration :: MParser ChainwebNodeConfiguration
pChainwebNodeConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: pLogConfig

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
    :: LogConfig
    -> (L.Logger SomeLogMessage -> IO ())
    -> IO ()
withNodeLogger logConfig f = runManaged $ do

    -- This manager is used only for logging backends
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings

    -- Base Backend
    baseBackend <- managed
        $ withBaseHandleBackend "ChainwebApp" mgr (_logConfigBackend logConfig)

    -- Telemetry Backends
    monitorBackend <- managed
        $ mkTelemetryLogger @CutHashes mgr teleLogConfig
    p2pInfoBackend <- managed
        $ mkTelemetryLogger @P2pSessionInfo mgr teleLogConfig
    managerBackend <- managed
        $ mkTelemetryLogger @ConnectionManagerStats mgr teleLogConfig

    logger <- managed
        $ L.withLogger (_logConfigLogger logConfig) $ logHandles
            [ logHandler monitorBackend
            , logHandler p2pInfoBackend
            , logHandler managerBackend
            ] baseBackend

    liftIO $ f logger
  where
    teleLogConfig = _logConfigTelemetryBackend logConfig

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

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration (TestWithTime petersonChainGraph))

main :: IO ()
main = runWithConfiguration mainInfo $ \conf ->
    withNodeLogger (_nodeConfigLog conf) $ node (_nodeConfigChainweb conf)

