{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module: Chainweb.Rosetta.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- This module defines the client API for the Chainweb Rosetta
-- integration.
--
module Chainweb.Rosetta.RestAPI.Client
( -- * AccounT Endpoints
  rosettaAccountBalanceApiClient
  -- * Block Endpoints
, rosettaBlockTransactionApiClient
, rosettaBlockApiClient
  -- * Construction Endpoints
, rosettaConstructionMetadataApiClient
, rosettaConstructionSubmitApiClient
  -- * Mempool Endpoints
, rosettaMempoolApiClient
, rosettaMempoolTransactionApiClient
  -- * Network Endpoints
, rosettaNetworkListApiClient
, rosettaNetworkOptionsApiClient
, rosettaNetworkStatusApiClient
)
where


import Rosetta

import Servant.Client

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Rosetta.RestAPI
import Chainweb.Version


-- -------------------------------------------------------------------------- --
-- Accounts Endpoints

rosettaAccountBalanceApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => AccountBalanceReq
        -- ^ Contains a network id, account id, and a partial block identifier
        -- which is not populated.
    -> ClientM AccountBalanceResp
rosettaAccountBalanceApiClient_ = client (rosettaAccountBalanceApi @v)

rosettaAccountBalanceApiClient
    :: ChainwebVersion
    -> AccountBalanceReq
        -- ^ Contains a network id, account id, and a partial block identifier
        -- which is not populated.
    -> ClientM AccountBalanceResp
rosettaAccountBalanceApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaAccountBalanceApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Block Endpoints

rosettaBlockTransactionApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => BlockTransactionReq
        -- ^ Contains a network id, a block id, and a transaction id
    -> ClientM BlockTransactionResp
rosettaBlockTransactionApiClient_ = client (rosettaBlockTransactionApi @v)

rosettaBlockTransactionApiClient
    :: ChainwebVersion
    -> BlockTransactionReq
        -- ^ Contains a network id, a block id, and a transaction id
    -> ClientM BlockTransactionResp
rosettaBlockTransactionApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaBlockTransactionApiClient_ @v

rosettaBlockApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => BlockReq
        -- ^ Contains a network id and a partial block id
    -> ClientM BlockResp
rosettaBlockApiClient_ = client (rosettaBlockApi @v)

rosettaBlockApiClient
    :: ChainwebVersion
    -> BlockReq
        -- ^ Contains a network id and a partial block id
    -> ClientM BlockResp
rosettaBlockApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaBlockApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Construction Endpoints

rosettaConstructionMetadataApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => ConstructionMetadataReq
        -- ^ contains a network id and a metadata object which specifies the
        -- metadata to return.
    -> ClientM ConstructionMetadataResp
rosettaConstructionMetadataApiClient_ = client (rosettaConstructionMetadataApi @v)

rosettaConstructionMetadataApiClient
    :: ChainwebVersion
    -> ConstructionMetadataReq
        -- ^ contains a network id and a metadata object which specifies the
        -- metadata to return.
    -> ClientM ConstructionMetadataResp
rosettaConstructionMetadataApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaConstructionMetadataApiClient_ @v

rosettaConstructionSubmitApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => ConstructionSubmitReq
        -- ^ Contains a network id and a signed transaction
    -> ClientM ConstructionSubmitResp
rosettaConstructionSubmitApiClient_ = client (rosettaConstructionSubmitApi @v)

rosettaConstructionSubmitApiClient
    :: ChainwebVersion
    -> ConstructionSubmitReq
        -- ^ Contains a network id and a signed transaction
    -> ClientM ConstructionSubmitResp
rosettaConstructionSubmitApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaConstructionSubmitApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Mempool Endpoints

rosettaMempoolTransactionApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => MempoolTransactionReq
        -- ^ Contains a network id and a transaction id
    -> ClientM MempoolTransactionResp
rosettaMempoolTransactionApiClient_ = client (rosettaMempoolTransactionApi @v)

rosettaMempoolTransactionApiClient
    :: ChainwebVersion
    -> MempoolTransactionReq
        -- ^ Contains a network id and a transaction id
    -> ClientM MempoolTransactionResp
rosettaMempoolTransactionApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaMempoolTransactionApiClient_ @v

rosettaMempoolApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => MempoolReq
        -- ^ contains a network id
    -> ClientM MempoolResp
rosettaMempoolApiClient_ = client (rosettaMempoolApi @v)

rosettaMempoolApiClient
    :: ChainwebVersion
    -> MempoolReq
      -- ^ contains a network id
    -> ClientM MempoolResp
rosettaMempoolApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaMempoolApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Network Endpoints

rosettaNetworkListApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => MetadataReq
        -- ^ Contains an optional object with metadata
    -> ClientM NetworkListResp
rosettaNetworkListApiClient_ = client (rosettaNetworkListApi @v)

rosettaNetworkListApiClient
    :: ChainwebVersion
    -> MetadataReq
        -- ^ Contains an optional object with metadata
    -> ClientM NetworkListResp
rosettaNetworkListApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaNetworkListApiClient_ @v

rosettaNetworkOptionsApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => NetworkReq
        -- ^ Contains a network identifier and optional object with metadata
    -> ClientM NetworkOptionsResp
rosettaNetworkOptionsApiClient_ = client (rosettaNetworkOptionsApi @v)

rosettaNetworkOptionsApiClient
    :: ChainwebVersion
    -> NetworkReq
        -- ^ Contains a network identifier and optional object with metadata
    -> ClientM NetworkOptionsResp
rosettaNetworkOptionsApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaNetworkOptionsApiClient_ @v

rosettaNetworkStatusApiClient_
    :: forall (v :: ChainwebVersionT)
    . KnownChainwebVersionSymbol v
    => NetworkReq
        -- ^ Contains a network identifier and optional object with metadata
    -> ClientM NetworkStatusResp
rosettaNetworkStatusApiClient_ = client (rosettaNetworkStatusApi @v)

rosettaNetworkStatusApiClient
    :: ChainwebVersion
    -> NetworkReq
        -- ^ Contains a network identifier and optional object with metadata
    -> ClientM NetworkStatusResp
rosettaNetworkStatusApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = rosettaNetworkStatusApiClient_ @v
