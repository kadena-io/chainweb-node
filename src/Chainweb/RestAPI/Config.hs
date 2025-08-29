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
) where

import Control.Lens

import Data.Proxy

import Servant

-- internal modules
import Chainweb.Chainweb.Configuration
import Chainweb.PayloadProvider.EVM
import Chainweb.PayloadProvider.Minimal (mpcRedeemAccount)
import Chainweb.PayloadProvider.Minimal.Payload (invalidAccount)
import Chainweb.PayloadProvider.Pact.Configuration (pactConfigMiner)
import Chainweb.RestAPI.Utils

import P2P.Node.Configuration
import P2P.Peer


-- -------------------------------------------------------------------------- --
-- GET config endpoing

type GetConfigApi = "config" :> Get '[JSON] ChainwebConfiguration

someGetConfigApi :: SomeApi
someGetConfigApi = SomeApi (Proxy @GetConfigApi)

someGetConfigServer :: ChainwebConfiguration -> SomeServer
someGetConfigServer config = SomeServer (Proxy @GetConfigApi) $ return
    -- hide sensible information

    -- SSL certificates
    $ set (configP2p . p2pConfigPeer . peerConfigCertificateChain) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigCertificateChainFile) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigKey) Nothing
    $ set (configP2p . p2pConfigPeer . peerConfigKeyFile) Nothing

    -- Miner Info
    $ set (configPayloadProviders . payloadProviderConfigMinimal . mpcRedeemAccount) invalidAccount
    $ set (configPayloadProviders . payloadProviderConfigPact . traversed . pactConfigMiner) Nothing
    $ set (configPayloadProviders . payloadProviderConfigEvm . traversed . evmConfMinerAddress) Nothing
    $ set (configPayloadProviders . payloadProviderConfigEvm . traversed . evmConfEngineJwtSecret) (_evmConfEngineJwtSecret defaultEvmProviderConfig)

    -- Service API port
    $ set (configServiceApi . serviceApiConfigPort) 0
    $ set (configServiceApi . serviceApiConfigInterface) "invalid"
    $ set configBackup defaultBackupConfig
    config
