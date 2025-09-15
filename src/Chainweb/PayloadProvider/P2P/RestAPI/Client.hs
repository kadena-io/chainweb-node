{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.PayloadProvider.P2P.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Client implementation of the block payload REST API
--
module Chainweb.PayloadProvider.P2P.RestAPI.Client
( payloadClient
-- , payloadBatchClient
-- , outputsClient
-- , outputsBatchClient
) where

import Servant.Client

-- internal modules

import Chainweb.ChainId
import Chainweb.PayloadProvider.P2P.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.Version
import Chainweb.BlockPayloadHash

-- -------------------------------------------------------------------------- --
-- GET Payload Client

payloadClient
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    . KnownChainwebVersionSymbol v
    => IsPayloadProvider p
    => KnownChainIdSymbol c
    => RankedBlockPayloadHash
    -> ClientM (PayloadType p)
payloadClient rh = _restPayload <$> client (payloadGetApi @v @c @p) height hash
  where
    height =  _rankedBlockPayloadHashHeight rh
    hash = _rankedBlockPayloadHashHash rh

-- payloadClient
--     :: ChainwebVersion
--     -> ChainId
--     -> RankedBlockPayloadHash
--     -> ClientM (RestPayload (PayloadType p))
-- payloadClient v c h = runIdentity $ do
--     SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
--     SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
--     case provider of
--         MinimalProvider ->
--             return $! payloadClient_ @v @c @'MinimalProvider h
--         PactProvider ->
--             return $! payloadClient_ @v @c @'PactProvider h
--         -- EvmProvider ->
--         --     return $! payloadClient_ @v @c @'PactProvider h
--   where
--     provider :: PayloadProviderType
--     provider = payloadProviderTypeForChain v c

-- -- -------------------------------------------------------------------------- --
-- -- Post Payload Batch Client
--
-- payloadBatchClient_
--     :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProvider)
--     . KnownChainwebVersionSymbol v
--     => KnownChainIdSymbol c
--     => BatchBody
--     -> ClientM PayloadDataList
-- payloadBatchClient_ = client (payloadPostApi @v @c)
--
-- -- The query may return any number (including none) of the requested payload
-- -- data. Results are returned in any order.
-- --
-- payloadBatchClient
--     :: ChainwebVersion
--     -> ChainId
--     -> BatchBody
--     -> ClientM PayloadDataList
-- payloadBatchClient v c k = runIdentity $ do
--     SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
--     SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
--     return $ payloadBatchClient_ @v @c k
--
