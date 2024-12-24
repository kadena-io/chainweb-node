{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wprepositive-qualified-module #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.P2P
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.P2P
( somePayloadApi
, somePayloadApis

--
, getPayloadClient
, getPayloadClient_
, payloadBatchClient
, payloadBatchClient_
) where

import Chainweb.BlockHeaderDB.RestAPI ()
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.PayloadProvider.EVM.Header
import Chainweb.PayloadProvider.P2P.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Version
import Control.Monad.Identity
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Proxy
import Ethereum.RLP
import GHC.Generics (Generic)
import Servant.API hiding (Header)
import Servant.Client
import Control.Lens ((^?!))
import GHC.Stack (HasCallStack)

-- -------------------------------------------------------------------------- --
-- Type-wrappers for some REST API endpoints

-- We want to use application/octet-stream as the content type for types
-- like [Header], but doing that requires encoding the list specifically
-- with a specific binary instance. write some newtype wrappers to do this
-- with a specific encoding function, so we can then later write MimeRender
-- and MimeUnrender instances

newtype HeaderList = HeaderList { _headerList :: [Header] }
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON, RLP)

-- -------------------------------------------------------------------------- --

instance IsPayloadProvider EvmProvider where
    type PayloadType EvmProvider = Header
    type PayloadBatchType EvmProvider = HeaderList

instance MimeRender OctetStream Header where
    mimeRender _ = putRlpLazyByteString
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream Header where
    mimeUnrender _ = get getRlp . BL.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream HeaderList where
    mimeRender _ = putRlpLazyByteString
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream HeaderList where
    mimeUnrender _ = get getRlp . BL.toStrict
    {-# INLINE mimeUnrender #-}

-- -------------------------------------------------------------------------- --

somePayloadApi
    :: ChainwebVersion
    -> ChainId
    -> SomeApi
somePayloadApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $! SomeApi (payloadApi @v' @c' @'EvmProvider)

somePayloadApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePayloadApis v = mconcat . fmap (somePayloadApi v)

-- -------------------------------------------------------------------------- --
-- GET Payload Client

getPayloadClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => RankedBlockPayloadHash
    -> ClientM (PayloadType 'EvmProvider)
getPayloadClient_ h = client (payloadGetApi @v @c @'EvmProvider) height hash
  where
    height = _rankedBlockPayloadHashHeight h
    hash = _rankedBlockPayloadHashHash h

-- | GET payload Client
--
-- This function is also asserting that the provider for the given version and
-- chain is actually the EvmProvider.
--
getPayloadClient
    :: HasCallStack
    => ChainwebVersion
    -> ChainId
    -> RankedBlockPayloadHash
    -> ClientM (PayloadType 'EvmProvider)
getPayloadClient v c h = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    case provider of
        PactProvider -> error
            "Chainweb.PayloadProvider.EVM.P2P: Chain payload provider mismatch: expected Evm but get Pact"
        EvmProvider -> return $! getPayloadClient_ @v @c h
  where
    provider :: PayloadProviderType
    provider = _versionPayloadProviderTypes v ^?! atChain c

-- -------------------------------------------------------------------------- --
-- -- POST Payload Batch Client

payloadBatchClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BatchBody
    -> ClientM (PayloadBatchType 'EvmProvider)
payloadBatchClient_ = client (payloadPostApi @v @c @'EvmProvider)

-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
payloadBatchClient
    :: ChainwebVersion
    -> ChainId
    -> BatchBody
    -> ClientM (PayloadBatchType 'EvmProvider)
payloadBatchClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    case provider of
        PactProvider -> error
            "Chainweb.PayloadProvider.EVM.P2P: Chain payload provider mismatch: expected Evm but get Pact"
        EvmProvider -> return $ payloadBatchClient_ @v @c k
  where
    provider :: PayloadProviderType
    provider = _versionPayloadProviderTypes v ^?! atChain c



