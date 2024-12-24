{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Chainweb.PayloadProvider.P2P.RestAPI
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.P2P.RestAPI
( PayloadBatchLimit(..)
, BatchBody(..)
, IsPayloadProvider(..)
, RestPayload(..)

-- * Payload API
, type PayloadApi
, payloadApi
, type PayloadGetApi
, payloadGetApi
, type PayloadPostApi
, payloadPostApi
-- ** Legacy API
, type ServicePayloadGetApi
, servicePayloadGetApi

-- * SomePayloadApi
, somePayloadApi
, somePayloadApis
) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI ()
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Version
import Control.Monad.Identity
import Data.Aeson
import Data.Kind
import Data.Proxy
import Numeric.Natural
import Servant.API

import Chainweb.PayloadProvider.Minimal.Payload qualified as Minimal
import Chainweb.Utils.Serialization (runPutL, putWord32le)
import Chainweb.Utils
import Chainweb.Payload qualified as Pact
import Chainweb.BlockPayloadHash
import GHC.Generics (Generic)
import Chainweb.Ranked

-- -------------------------------------------------------------------------- --
-- Constants

-- | The maximum number of items that are returned in a batch
--
newtype PayloadBatchLimit = PayloadBatchLimit Natural
    deriving (Show, Eq)
    deriving newtype (Ord, Enum, Num, Real, Integral, ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --

-- | Class of Payload Provider APIs
--
-- We need to dispatch the APIs for different payload providers at type level
-- because servant composes APIs on type level. Once we run payload providers in
-- different processes with their own server instances we don't need that any
-- more.
--
class
    ( ToJSON (PayloadType p)
    , FromJSON (PayloadType p)
    , ToJSON (PayloadBatchType p)
    , FromJSON (PayloadBatchType p)
    , MimeRender OctetStream (PayloadType p)
    , MimeRender OctetStream (PayloadBatchType p)
    )
    => IsPayloadProvider (p :: PayloadProviderType)
  where
    type PayloadType p :: Type
    type PayloadBatchType p :: Type

    batch :: [Maybe (PayloadType p)] -> PayloadBatchType p

    -- default Payload Batch limit
    p2pPayloadBatchLimit :: PayloadBatchLimit
    p2pPayloadBatchLimit = 0

-- -------------------------------------------------------------------------- --

-- | The body of a batch payload request.
--
newtype BatchBody = BatchBody [RankedBlockPayloadHash]
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via [JsonRanked "hash" BlockPayloadHash]

-- -------------------------------------------------------------------------- --
-- Wrapper for Rest API Payloads

-- | Wrapper for Rest API Payloads
--
-- The purpose of this newtype is to avoid IncohrentInstances when resolving
-- Servants type classes.
--
newtype RestPayload a = RestPayload { _restPayload :: a }
    deriving (Show, Eq)
    deriving newtype (ToJSON, FromJSON)

instance MimeRender OctetStream a => MimeRender OctetStream (RestPayload a) where
    mimeRender p = mimeRender p . _restPayload

-- -------------------------------------------------------------------------- --
-- Legacy Payload GET API

-- | The legacy API is not supported by all Payload Providers.
--
-- @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/\<BlockPayloadHash\>@
--
-- * For Pact the a parameter is PayloadData
-- * For EVM the a parameter is Header
--
type ServicePayloadGetApi_ (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = "payload"
    :> Capture "BlockPayloadHash" BlockPayloadHash
    :> QueryParam "height" BlockHeight
    :> Get '[JSON, OctetStream] (RestPayload (PayloadType p))

type ServicePayloadGetApi (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = 'ChainwebEndpoint v
    :> ChainEndpoint c
    :> ServicePayloadGetApi_ v c p

servicePayloadGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    . IsPayloadProvider p
    => Proxy (ServicePayloadGetApi v c p)
servicePayloadGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload GET API

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/height/\<BlockHeight\>/payload\/\<BlockPayloadHash\>@
--
-- * For Pact the a parameter is PayloadData
-- * For EVM the a parameter is Header
--
type PayloadGetApi_ (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = "height"
    :> Capture "BlockHeight" BlockHeight
    :> "payload"
    :> Capture "BlockPayloadHash" BlockPayloadHash
    :> Get '[JSON, OctetStream] (RestPayload (PayloadType p))

type PayloadGetApi (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PayloadGetApi_ v c p

payloadGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    . Proxy (PayloadGetApi v c p)
payloadGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload POST API

-- | @POST \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/batch@
--
-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
type PayloadPostApi_ (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = "payload"
    :> "batch"
    :> ReqBody '[JSON] BatchBody
    :> Post '[JSON, OctetStream] (RestPayload (PayloadBatchType p))

type PayloadPostApi (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PayloadPostApi_ v c p

payloadPostApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    . Proxy (PayloadPostApi v c p)
payloadPostApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload API

type PayloadApi v c p
    = PayloadGetApi v c p
    :<|> PayloadPostApi v c p

payloadApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (p :: PayloadProviderType)
    . Proxy (PayloadApi v c p)
payloadApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Payload API

-- | Dispatch provider specific APIs
--
-- FIXME: Should we split this up and move it into the scope of the respective
-- payload provider.
--
somePayloadApi
    :: IsPayloadProvider 'MinimalProvider
    => IsPayloadProvider 'PactProvider
    -- => IsPayloadProvider 'EvmProvider
    => ChainwebVersion
    -> ChainId
    -> SomeApi
somePayloadApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    case provider of
        MinimalProvider ->
            return $! SomeApi (payloadApi @v' @c' @'MinimalProvider)
        PactProvider ->
            return $! SomeApi (payloadApi @v' @c' @'PactProvider)
        EvmProvider ->
            error "Chainweb.PayloadProvider.P2P.RestAPI.somePayloadApi: IsPayloadProvider not implemented for EVM"
            -- return $! SomeApi (payloadApi @v' @c' @'EvmProvider)
  where
    provider :: PayloadProviderType
    provider = payloadProviderTypeForChain v c

somePayloadApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePayloadApis v = mconcat . fmap (somePayloadApi v)

-- -------------------------------------------------------------------------- --
-- Implementations of IsPayloadProvider

instance IsPayloadProvider MinimalProvider where
    type PayloadType MinimalProvider = Minimal.Payload
    type PayloadBatchType MinimalProvider = [Minimal.Payload]

    p2pPayloadBatchLimit = 20 -- FIXME

instance MimeRender OctetStream Minimal.Payload where
    mimeRender _ = runPutL . Minimal.encodePayload

instance MimeRender OctetStream [Minimal.Payload] where
    mimeRender _ ps = runPutL $ do
        putWord32le (int $ length ps)
        mapM_ Minimal.encodePayload ps

-- FIXME: fix the following instance to conform with the current API (or even
-- better fix the current bad binary encoding of payload data).

instance IsPayloadProvider PactProvider where
    type PayloadType PactProvider = Pact.PayloadData
    type PayloadBatchType PactProvider = Pact.PayloadDataList

    p2pPayloadBatchLimit = 20 -- FIXME

