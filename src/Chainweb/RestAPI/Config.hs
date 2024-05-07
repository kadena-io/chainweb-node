{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.RestAPI.Config
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.RestAPI.Config
( GetConfigApi
, someGetConfigApi
, someGetConfigServer
, newGetConfigServer
) where

import Control.Lens

import Data.Proxy

import Servant
import Network.HTTP.Types
import Web.DeepRoute
import Web.DeepRoute.Wai

-- internal modules
import Chainweb.Chainweb.Configuration
import Chainweb.Miner.Config
import Chainweb.RestAPI.Utils

import P2P.Node.Configuration
import P2P.Peer


-- -------------------------------------------------------------------------- --
-- GET config endpoing

type GetConfigApi = "config" :> Get '[JSON] ChainwebConfiguration

someGetConfigApi :: SomeApi
someGetConfigApi = SomeApi (Proxy @GetConfigApi)

censorConfig :: ChainwebConfiguration -> ChainwebConfiguration
censorConfig config =
    -- hide sensitive information

    -- SSL certificates
    set (configP2p . p2pConfigPeer . peerConfigCertificateChain) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigCertificateChainFile) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigKey) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigKeyFile) Nothing

    -- Miner Info
    $ set (configMining . miningCoordination . coordinationMiners) mempty
    $ set (configMining . miningInNode . nodeMiner) invalidMiner

    -- Service API port
    $ set (configServiceApi . serviceApiConfigPort) 0
    $ set (configServiceApi . serviceApiConfigInterface) "invalid"
    $ set configBackup defaultBackupConfig
    config

someGetConfigServer :: ChainwebConfiguration -> SomeServer
someGetConfigServer config = SomeServer (Proxy @GetConfigApi) $ return $ censorConfig config

newGetConfigServer :: ChainwebConfiguration -> Route Application
newGetConfigServer config =
    endpoint methodGet ("application/json") $ \_ resp ->
        resp $ responseJSON ok200 [] $ censorConfig config
