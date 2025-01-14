{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

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
import Chainweb.BlockPayloadHash
import Chainweb.ChainId
import Chainweb.Payload qualified as Pact
import Chainweb.PayloadProvider.Minimal.Payload qualified as Minimal
import Chainweb.Ranked
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Serialization (runPutL, putWord32le, runGetEitherL, getWord32le)
import Chainweb.Version
import Control.Monad
import Control.Monad.Identity
import Data.Aeson
import Data.Kind
import Data.Maybe
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeNats
import Servant.API
import Data.Singletons

-- -------------------------------------------------------------------------- --
-- Constants

-- | The maximum number of items that are returned in a batch
--
newtype PayloadBatchLimit = PayloadBatchLimit Natural
    deriving (Show, Eq)
    deriving newtype (Ord, Enum, Num, Real, Integral, ToJSON, FromJSON)

-- -------------------------------------------------------------------------- --
-- IsPayload Provider Class
--
-- NOTE:
--
-- Currently, this class is only available for provider P2P. It might be useful
-- more generally for the implementation of payload providers. However, not all
-- payload providers use payload data that is visible to the chainweb-node
-- beacon. Therefore PayloadData concepts must not leak to the ChainwebVersion
-- level and the definition of the PayloadProviderType tag. Also, the
-- 'PayloadProvider' class as well as the moduel "Chainweb.PayloadProvider" must
-- not depend on it.
--
-- TODO:
--
-- - Todo rename payload that is visible to the chainweb-node beacon to
--   something like "PayloadData" or "PayloadInfo" to indicate that it does not
--   represent the complete block payload
--
-- - In the module hierachy, distinguishy more clearly between general Payload
--   Provider features, that concern all payload providers, and auxiliary
--   payload provider features, that serve as optional utils to implement
--   aspects of payload providers that are hosted in the chainweb-node beacon.
--

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
-- payload providers. Otherwise this module has to depend on the payload
-- providers.
--
somePayloadApi
    :: IsPayloadProvider 'MinimalProvider
    => IsPayloadProvider 'PactProvider
    => ChainwebVersion
    -> ChainId
    -> SomeApi
somePayloadApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    withSomeSing (payloadProviderTypeForChain v c) $ \case
        SMinimalProvider ->
            return $! SomeApi (payloadApi @v' @c' @'MinimalProvider)
        SPactProvider ->
            return $! SomeApi (payloadApi @v' @c' @'PactProvider)
        SEvmProvider @n _ ->
            error "Chainweb.PayloadProvider.P2P.RestAPI.somePayloadApi: IsPayloadProvider not implemented for EVM"

somePayloadApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePayloadApis v = mconcat . fmap (somePayloadApi v)

-- -------------------------------------------------------------------------- --
-- Implementations of IsPayloadProvider
-- -------------------------------------------------------------------------- --

-- | IsPayloadProvider instance for the Minimal Payload provider
--
instance IsPayloadProvider MinimalProvider where
    type PayloadType MinimalProvider = Minimal.Payload
    type PayloadBatchType MinimalProvider = [Minimal.Payload]
    p2pPayloadBatchLimit = 20 -- FIXME
    batch = catMaybes

instance MimeRender OctetStream Minimal.Payload where
    mimeRender _ = runPutL . Minimal.encodePayload
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream Minimal.Payload where
    mimeUnrender _ = runGetEitherL Minimal.decodePayload
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream [Minimal.Payload] where
    mimeRender _ ps = runPutL $ do
        putWord32le (int $ length ps)
        mapM_ Minimal.encodePayload ps

instance MimeUnrender OctetStream [Minimal.Payload] where
    mimeUnrender _ = runGetEitherL $ do
        l <- int <$> getWord32le
        replicateM l Minimal.decodePayload
    {-# INLINE mimeUnrender #-}

-- | IsPayloadProvider instance for the Pact Payload provider
--
-- FIXME: fix this instance to conform with the current API (or even
-- better fix the current bad binary encoding of payload data).
--
instance IsPayloadProvider PactProvider where
    type PayloadType PactProvider = Pact.PayloadData
    type PayloadBatchType PactProvider = Pact.PayloadDataList
    p2pPayloadBatchLimit = 20 -- FIXME
    batch = Pact.PayloadDataList . catMaybes


