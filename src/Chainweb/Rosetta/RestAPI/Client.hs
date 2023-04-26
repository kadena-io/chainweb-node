{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- This module defines the client API for the Chainweb Rosetta
-- integration.
module Chainweb.Rosetta.RestAPI.Client
  ( -- * AccounT Endpoints
    rosettaAccountBalanceApiClient,

    -- * Block Endpoints
    rosettaBlockTransactionApiClient,
    rosettaBlockApiClient,

    -- * Construction Endpoints
    rosettaConstructionDeriveApiClient,
    rosettaConstructionPreprocessApiClient,
    rosettaConstructionMetadataApiClient,
    rosettaConstructionPayloadsApiClient,
    rosettaConstructionParseApiClient,
    rosettaConstructionCombineApiClient,
    rosettaConstructionHashApiClient,
    rosettaConstructionSubmitApiClient,

    -- * Mempool Endpoints
    rosettaMempoolApiClient,
    rosettaMempoolTransactionApiClient,

    -- * Network Endpoints
    rosettaNetworkListApiClient,
    rosettaNetworkOptionsApiClient,
    rosettaNetworkStatusApiClient,
  )
where

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Rosetta.RestAPI
import Chainweb.Version
import Rosetta
import Servant.Client

-- -------------------------------------------------------------------------- --
-- Accounts Endpoints

rosettaAccountBalanceApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network id, account id, and a partial block identifier
  -- which is not populated.
  AccountBalanceReq ->
  ClientM AccountBalanceResp
rosettaAccountBalanceApiClient_ = client (rosettaAccountBalanceApi @v)

rosettaAccountBalanceApiClient ::
  ChainwebVersion ->
  -- | Contains a network id, account id, and a partial block identifier
  -- which is not populated.
  AccountBalanceReq ->
  ClientM AccountBalanceResp
rosettaAccountBalanceApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaAccountBalanceApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Block Endpoints

rosettaBlockTransactionApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network id, a block id, and a transaction id
  BlockTransactionReq ->
  ClientM BlockTransactionResp
rosettaBlockTransactionApiClient_ = client (rosettaBlockTransactionApi @v)

rosettaBlockTransactionApiClient ::
  ChainwebVersion ->
  -- | Contains a network id, a block id, and a transaction id
  BlockTransactionReq ->
  ClientM BlockTransactionResp
rosettaBlockTransactionApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaBlockTransactionApiClient_ @v

rosettaBlockApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network id and a partial block id
  BlockReq ->
  ClientM BlockResp
rosettaBlockApiClient_ = client (rosettaBlockApi @v)

rosettaBlockApiClient ::
  ChainwebVersion ->
  -- | Contains a network id and a partial block id
  BlockReq ->
  ClientM BlockResp
rosettaBlockApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaBlockApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Construction Endpoints

rosettaConstructionDeriveApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionDeriveReq ->
  ClientM ConstructionDeriveResp
rosettaConstructionDeriveApiClient_ = client (rosettaConstructionDeriveApi @v)

rosettaConstructionDeriveApiClient ::
  ChainwebVersion ->
  ConstructionDeriveReq ->
  ClientM ConstructionDeriveResp
rosettaConstructionDeriveApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionDeriveApiClient_ @v

rosettaConstructionPreprocessApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionPreprocessReq ->
  ClientM ConstructionPreprocessResp
rosettaConstructionPreprocessApiClient_ = client (rosettaConstructionPreprocessApi @v)

rosettaConstructionPreprocessApiClient ::
  ChainwebVersion ->
  ConstructionPreprocessReq ->
  ClientM ConstructionPreprocessResp
rosettaConstructionPreprocessApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionPreprocessApiClient_ @v

rosettaConstructionMetadataApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | contains a network id and a metadata object which specifies the
  -- metadata to return.
  ConstructionMetadataReq ->
  ClientM ConstructionMetadataResp
rosettaConstructionMetadataApiClient_ = client (rosettaConstructionMetadataApi @v)

rosettaConstructionMetadataApiClient ::
  ChainwebVersion ->
  -- | contains a network id and a metadata object which specifies the
  -- metadata to return.
  ConstructionMetadataReq ->
  ClientM ConstructionMetadataResp
rosettaConstructionMetadataApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionMetadataApiClient_ @v

rosettaConstructionPayloadsApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionPayloadsReq ->
  ClientM ConstructionPayloadsResp
rosettaConstructionPayloadsApiClient_ = client (rosettaConstructionPayloadsApi @v)

rosettaConstructionPayloadsApiClient ::
  ChainwebVersion ->
  ConstructionPayloadsReq ->
  ClientM ConstructionPayloadsResp
rosettaConstructionPayloadsApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionPayloadsApiClient_ @v

rosettaConstructionParseApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionParseReq ->
  ClientM ConstructionParseResp
rosettaConstructionParseApiClient_ = client (rosettaConstructionParseApi @v)

rosettaConstructionParseApiClient ::
  ChainwebVersion ->
  ConstructionParseReq ->
  ClientM ConstructionParseResp
rosettaConstructionParseApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionParseApiClient_ @v

rosettaConstructionCombineApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionCombineReq ->
  ClientM ConstructionCombineResp
rosettaConstructionCombineApiClient_ = client (rosettaConstructionCombineApi @v)

rosettaConstructionCombineApiClient ::
  ChainwebVersion ->
  ConstructionCombineReq ->
  ClientM ConstructionCombineResp
rosettaConstructionCombineApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionCombineApiClient_ @v

rosettaConstructionHashApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  ConstructionHashReq ->
  ClientM TransactionIdResp
rosettaConstructionHashApiClient_ = client (rosettaConstructionHashApi @v)

rosettaConstructionHashApiClient ::
  ChainwebVersion ->
  ConstructionHashReq ->
  ClientM TransactionIdResp
rosettaConstructionHashApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionHashApiClient_ @v

rosettaConstructionSubmitApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network id and a signed transaction
  ConstructionSubmitReq ->
  ClientM TransactionIdResp
rosettaConstructionSubmitApiClient_ = client (rosettaConstructionSubmitApi @v)

rosettaConstructionSubmitApiClient ::
  ChainwebVersion ->
  -- | Contains a network id and a signed transaction
  ConstructionSubmitReq ->
  ClientM TransactionIdResp
rosettaConstructionSubmitApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaConstructionSubmitApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Mempool Endpoints

rosettaMempoolTransactionApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network id and a transaction id
  MempoolTransactionReq ->
  ClientM MempoolTransactionResp
rosettaMempoolTransactionApiClient_ = client (rosettaMempoolTransactionApi @v)

rosettaMempoolTransactionApiClient ::
  ChainwebVersion ->
  -- | Contains a network id and a transaction id
  MempoolTransactionReq ->
  ClientM MempoolTransactionResp
rosettaMempoolTransactionApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaMempoolTransactionApiClient_ @v

rosettaMempoolApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | contains a network id
  NetworkReq ->
  ClientM MempoolResp
rosettaMempoolApiClient_ = client (rosettaMempoolApi @v)

rosettaMempoolApiClient ::
  ChainwebVersion ->
  -- | contains a network id
  NetworkReq ->
  ClientM MempoolResp
rosettaMempoolApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaMempoolApiClient_ @v

-- -------------------------------------------------------------------------- --
-- Network Endpoints

rosettaNetworkListApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains an optional object with metadata
  MetadataReq ->
  ClientM NetworkListResp
rosettaNetworkListApiClient_ = client (rosettaNetworkListApi @v)

rosettaNetworkListApiClient ::
  ChainwebVersion ->
  -- | Contains an optional object with metadata
  MetadataReq ->
  ClientM NetworkListResp
rosettaNetworkListApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaNetworkListApiClient_ @v

rosettaNetworkOptionsApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network identifier and optional object with metadata
  NetworkReq ->
  ClientM NetworkOptionsResp
rosettaNetworkOptionsApiClient_ = client (rosettaNetworkOptionsApi @v)

rosettaNetworkOptionsApiClient ::
  ChainwebVersion ->
  -- | Contains a network identifier and optional object with metadata
  NetworkReq ->
  ClientM NetworkOptionsResp
rosettaNetworkOptionsApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaNetworkOptionsApiClient_ @v

rosettaNetworkStatusApiClient_ ::
  forall (v :: ChainwebVersionT).
  KnownChainwebVersionSymbol v =>
  -- | Contains a network identifier and optional object with metadata
  NetworkReq ->
  ClientM NetworkStatusResp
rosettaNetworkStatusApiClient_ = client (rosettaNetworkStatusApi @v)

rosettaNetworkStatusApiClient ::
  ChainwebVersion ->
  -- | Contains a network identifier and optional object with metadata
  NetworkReq ->
  ClientM NetworkStatusResp
rosettaNetworkStatusApiClient
  (FromSingChainwebVersion (SChainwebVersion :: Sing v)) =
    rosettaNetworkStatusApiClient_ @v
