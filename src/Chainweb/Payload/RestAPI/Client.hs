{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Client implementation of the block payload REST API
--
module Chainweb.Payload.RestAPI.Client
( payloadClient
, payloadBatchClient
, outputsClient
, outputsBatchClient
) where

import Control.Monad.Identity

import Data.Proxy

import Servant.Client

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.Version
import Chainweb.BlockHeight (BlockHeight)

-- -------------------------------------------------------------------------- --
-- GET Payload Client

payloadClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM PayloadData
payloadClient_ = client (payloadGetApi @v @c)

payloadClient
    :: ChainwebVersion
    -> ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM PayloadData
payloadClient v c k h = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ payloadClient_ @v @c k h

-- -------------------------------------------------------------------------- --
-- Post Payload Batch Client

payloadBatchClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BatchBody
    -> ClientM PayloadDataList
payloadBatchClient_ = client (payloadPostApi @v @c)

-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
payloadBatchClient
    :: ChainwebVersion
    -> ChainId
    -> BatchBody
    -> ClientM PayloadDataList
payloadBatchClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ payloadBatchClient_ @v @c k

-- -------------------------------------------------------------------------- --
-- GET Outputs Client

outputsClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM PayloadWithOutputs
outputsClient_ = client (outputsGetApi @v @c)

outputsClient
    :: ChainwebVersion
    -> ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM PayloadWithOutputs
outputsClient v c k h = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ outputsClient_ @v @c k h

-- -------------------------------------------------------------------------- --
-- POST Outputs Batch Client

outputsBatchClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BatchBody
    -> ClientM PayloadWithOutputsList
outputsBatchClient_ = client (outputsPostApi @v @c)

outputsBatchClient
    :: ChainwebVersion
    -> ChainId
    -> BatchBody
    -> ClientM PayloadWithOutputsList
outputsBatchClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ outputsBatchClient_ @v @c k
