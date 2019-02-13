{-# LANGUAGE DeriveGeneric #-}
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
, CutLog
, runMonitor

-- * Chainweb Node
, node
, withNodeLogger
, chainwebLogFunctions

-- * Main function
, main
) where

import Configuration.Utils

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Foldable
import qualified Data.HashMap.Strict as HM

import GHC.Generics hiding (from)

import qualified Streaming.Prelude as S

import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))

import Data.LogMessage

import Utils.Logging

-- -------------------------------------------------------------------------- --
-- Configuration

data ChainwebNodeConfiguration = ChainwebNodeConfiguration
    { _nodeConfigChainweb :: !ChainwebConfiguration
    , _nodeConfigLog :: !L.LogConfig
    , _nodeConfigCutsLogger :: !(EnableConfig JsonLoggerConfig)
    }
    deriving (Show, Eq, Generic)

makeLenses ''ChainwebNodeConfiguration

defaultChainwebNodeConfiguration :: ChainwebVersion -> ChainwebNodeConfiguration
defaultChainwebNodeConfiguration v = ChainwebNodeConfiguration
    { _nodeConfigChainweb = defaultChainwebConfiguration v
    , _nodeConfigLog = L.defaultLogConfig
        & L.logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _nodeConfigCutsLogger =
        EnableConfig True defaultJsonLoggerConfig
    }

instance ToJSON ChainwebNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _nodeConfigChainweb o
        , "log" .= _nodeConfigLog o
        , "cutsLogger" .= _nodeConfigCutsLogger o
        ]

instance FromJSON (ChainwebNodeConfiguration -> ChainwebNodeConfiguration) where
    parseJSON = withObject "ChainwebNodeConfig" $ \o -> id
        <$< nodeConfigChainweb %.: "chainweb" % o
        <*< nodeConfigLog %.: "log" % o
        <*< nodeConfigCutsLogger %.: "cutsLogger" % o

pChainwebNodeConfiguration :: MParser ChainwebNodeConfiguration
pChainwebNodeConfiguration = id
    <$< nodeConfigChainweb %:: pChainwebConfiguration
    <*< nodeConfigLog %:: L.pLogConfig
    <*< nodeConfigCutsLogger %::
        pEnableConfig "cuts-logger" % pJsonLoggerConfig (Just "cuts-")

-- -------------------------------------------------------------------------- --
-- Monitor

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
    withChainweb graph conf logfuns $ \cw ->
        race_
            (runChainweb cw)
            (runMonitor logger (_cutsCutDb $ _chainwebCuts cw))
  where
    logfuns = chainwebLogFunctions (chainIds_ graph) logger

withNodeLogger :: L.LogConfig -> EnableConfig JsonLoggerConfig -> (Logger -> IO a) -> IO a
withNodeLogger logConfig cutsLoggerConfig f =
    withFileHandleBackend (L._logConfigBackend logConfig) $ \baseBackend ->
        withJsonFileHandleBackend @CutLog cutsLoggerConfig $ \monitorBackend -> do
            let loggerBackend = logHandles
                    [ logHandler monitorBackend
                    ] baseBackend
            L.withLogger (L._logConfigLogger logConfig) loggerBackend f

-- | Initialize Logging Functions
--
-- The code could be simplified (and labeling of log messages more fine-grained)
-- by pushing the initialization down to the respective component
-- initializations. But for that the code of those components would take a
-- dependency on the `Logger` type, or at least the API.
--
chainwebLogFunctions
    :: Foldable f
    => f ChainId
    -> Logger
    -> ChainwebLogFunctions
chainwebLogFunctions cids logger = ChainwebLogFunctions
    { _chainwebNodeLogFun = aLogFunction logger
    , _chainwebMinerLogFun = aLogFunction (compLogger "miner")
    , _chainwebCutLogFun = aLogFunction (compLogger "cuts")
    , _chainwebChainLogFuns = foldl' chainLog mempty cids
    }
  where
    aLogFunction l = ALogFunction $ loggerFun l
    chainLog m c = HM.insert c
        (aLogFunction $ addLabel ("chain", sshow c) $ compLogger "chains")
        m
    addLabel = over L.setLoggerScope . (:)
    compLogger l = addLabel ("component" , l) logger

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo ChainwebNodeConfiguration
mainInfo = programInfo
    "Chainweb Node"
    pChainwebNodeConfiguration
    (defaultChainwebNodeConfiguration Test)

main :: IO ()
main = runWithConfiguration mainInfo $ \conf ->
    withNodeLogger (_nodeConfigLog conf) (_nodeConfigCutsLogger conf) $ \logger ->
        node petersonChainGraph (_nodeConfigChainweb conf) logger
