{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "UNKNOWN"
#endif

-- |
-- Module: Chainweb.Utils.API
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Utils for Chainweb REST APIs
--
module Chainweb.RestAPI.Utils
(
-- * Servant Utils
  Reassoc

-- * API Version
, Version
, ApiVersion(..)
, apiVersion
, prettyApiVersion

-- * Chainweb Node Version Header
, type ChainwebNodeVersionHeaderName
, type ChainwebNodeVersionHeaderValue
, chainwebNodeVersionHeaderName
, chainwebNodeVersionHeaderValue
, chainwebNodeVersionHeader

-- * Paging
, type PageParams
, type LimitParam
, type NextParam

-- * Chainweb API Endpoints
, ChainwebEndpoint(..)
, NetworkEndpoint(..)
, ChainEndpoint
, MempoolEndpoint
, CutEndpoint

-- * Some API
--
-- $someapi
, SomeApi(..)
, someApi

-- ** Some Server
, SomeServer(..)
, someServerApplication

-- * Misc Utils
, bindPortTcp
, allocateSocket
, deallocateSocket
, withSocket

-- ** Content Types for Clients
, SupportedReqBodyContentType
, SetReqBodyContentType
, SupportedRespBodyContentType
, SetRespBodyContentType

) where

import Control.Monad.Catch (bracket)

import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Kind
import Data.Proxy
import Data.Streaming.Network (bindPortGen)
import Data.String
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeLits

import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.Socket as N
import Network.Wai.Handler.Warp (HostPreference)

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger

-- internal modules
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Orphans ()
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Servant Utils

type Reassoc (api :: Type) = ReassocBranch api '[]

type family ReassocBranch (a :: s) (b :: [Type]) :: Type where
    ReassocBranch (a :> b) rest = ReassocBranch a (b ': rest)
    ReassocBranch a '[] = a
    ReassocBranch a (b ': rest) = a :> ReassocBranch b rest

-- -------------------------------------------------------------------------- --
-- API Version

type Version = "0.0"

newtype ApiVersion = ApiVersion T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, FromHttpApiData)

apiVersion :: ApiVersion
apiVersion = ApiVersion . T.pack $ symbolVal (Proxy @Version)

prettyApiVersion :: T.Text
prettyApiVersion = case apiVersion of
    ApiVersion t -> t

-- -------------------------------------------------------------------------- --
-- Chainweb Node Version header

type ChainwebNodeVersionHeaderName = "X-Chainweb-Node-Version"
type ChainwebNodeVersionHeaderValue = CURRENT_PACKAGE_VERSION

chainwebNodeVersionHeaderName :: IsString a => CI.FoldCase a => CI.CI a
chainwebNodeVersionHeaderName = fromString $ symbolVal $ Proxy @ChainwebNodeVersionHeaderName
{-# INLINE chainwebNodeVersionHeaderName #-}

chainwebNodeVersionHeaderValue :: IsString a => a
chainwebNodeVersionHeaderValue = fromString $ symbolVal $ Proxy @ChainwebNodeVersionHeaderValue
{-# INLINE chainwebNodeVersionHeaderValue #-}

chainwebNodeVersionHeader :: HTTP.Header
chainwebNodeVersionHeader = (chainwebNodeVersionHeaderName, chainwebNodeVersionHeaderValue)
{-# INLINE chainwebNodeVersionHeader #-}

-- -------------------------------------------------------------------------- --
-- Paging Utils

-- | Pages Parameters
--
-- *   limit :: Natural
-- *   next :: k
--
type PageParams k = LimitParam :> NextParam k

type LimitParam = QueryParam "limit" Limit
type NextParam k = QueryParam "next" k

-- -------------------------------------------------------------------------- --
-- Chainweb API Endpoints

newtype ChainwebEndpoint = ChainwebEndpoint ChainwebVersionT

type ChainwebEndpointApi c a = "chainweb"
    :> Version
    :> ChainwebVersionSymbol c
    :> a

-- | Server implementations can use this type to gain access to the
-- @X-chainweb-node-version@. Currently, this isn't used. It should be used with
-- care; rejecting clients based on their chainweb version can cause forks in
-- the network.
--
-- The @X-chainweb-node-version@ request header is a constant on the client
-- side. Therefore clients don't need to provide it. It is injected in the
-- 'HasClient' instance.
--
type ChainwebEndpointApiWithHeader c a = "chainweb"
    :> Version
    :> ChainwebVersionSymbol c
    :> Header ChainwebNodeVersionHeaderName T.Text -- Cabal.Version
    :> a

instance
    (HasServer api ctx, KnownChainwebVersionSymbol c)
    => HasServer ('ChainwebEndpoint c :> api) ctx
  where
    type ServerT ('ChainwebEndpoint c :> api) m
        = ServerT (ChainwebEndpointApi c api) m

    route _ = route $ Proxy @(ChainwebEndpointApi c api)

    hoistServerWithContext _ = hoistServerWithContext
        $ Proxy @(ChainwebEndpointApi c api)

instance
    (KnownChainwebVersionSymbol c, HasSwagger api)
    => HasSwagger ('ChainwebEndpoint c :> api)
  where
    toSwagger _ = toSwagger (Proxy @(ChainwebEndpointApi c api))

instance
    (KnownChainwebVersionSymbol v, HasClient m api)
    => HasClient m ('ChainwebEndpoint v :> api)
  where
    type Client m ('ChainwebEndpoint v :> api)
        = Client m (ChainwebEndpointApi v api)

    -- Inject @X-chainweb-node-version@ header
    --
    clientWithRoute pm _ r
        = clientWithRoute
            pm
            (Proxy @(ChainwebEndpointApiWithHeader v api))
            r
            (Just chainwebNodeVersionHeaderValue)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(ChainwebEndpointApi v api)

instance
    (KnownChainwebVersionSymbol v, HasLink api)
    => HasLink ('ChainwebEndpoint v :> api)
  where
    type MkLink ('ChainwebEndpoint v :> api) a
        = MkLink (ChainwebEndpointApi v api) a

    toLink toA _ = toLink toA $ Proxy @(ChainwebEndpointApi v api)

-- -------------------------------------------------------------------------- --
-- Network API Endpoint

newtype NetworkEndpoint = NetworkEndpoint NetworkIdT

type ChainEndpoint (c :: ChainIdT) = 'NetworkEndpoint ('ChainNetworkT c)
type MempoolEndpoint (c :: ChainIdT) = 'NetworkEndpoint ('MempoolNetworkT c)
type CutEndpoint = 'NetworkEndpoint 'CutNetworkT

type family NetworkEndpointApi (net :: NetworkIdT) (api :: Type) :: Type where
    NetworkEndpointApi 'CutNetworkT api = "cut" :> api
    NetworkEndpointApi ('ChainNetworkT c) api = "chain" :> ChainIdSymbol c :> api
    NetworkEndpointApi ('MempoolNetworkT c) api = "chain" :> ChainIdSymbol c :> "mempool" :> api

-- HasServer

instance
    (HasServer api ctx, KnownChainIdSymbol c, x ~ 'ChainNetworkT c)
    => HasServer ('NetworkEndpoint ('ChainNetworkT c) :> api) ctx
  where
    type ServerT ('NetworkEndpoint ('ChainNetworkT c) :> api) m
        = ServerT (NetworkEndpointApi ('ChainNetworkT c) api) m

    route _ = route (Proxy @(NetworkEndpointApi x api))

    hoistServerWithContext _ = hoistServerWithContext
        (Proxy @(NetworkEndpointApi x api))

instance
    (HasServer api ctx, KnownChainIdSymbol c, x ~ 'MempoolNetworkT c)
    => HasServer ('NetworkEndpoint ('MempoolNetworkT c) :> api) ctx
  where
    type ServerT ('NetworkEndpoint ('MempoolNetworkT c) :> api) m
        = ServerT (NetworkEndpointApi ('MempoolNetworkT c) api) m

    route _ = route (Proxy @(NetworkEndpointApi x api))

    hoistServerWithContext _ = hoistServerWithContext
        (Proxy @(NetworkEndpointApi x api))

instance
    HasServer api ctx => HasServer ('NetworkEndpoint 'CutNetworkT :> api) ctx
  where
    type ServerT ('NetworkEndpoint 'CutNetworkT :> api) m
        = ServerT (NetworkEndpointApi 'CutNetworkT api) m

    route _ = route (Proxy @(NetworkEndpointApi 'CutNetworkT api))

    hoistServerWithContext _ = hoistServerWithContext
        (Proxy @(NetworkEndpointApi 'CutNetworkT api))

-- HasSwagger

instance
    (KnownChainIdSymbol c, HasSwagger api)
    => HasSwagger ('NetworkEndpoint ('ChainNetworkT c) :> api)
  where
    toSwagger _ = toSwagger (Proxy @(NetworkEndpointApi ('ChainNetworkT c) api))

instance
    (KnownChainIdSymbol c, HasSwagger api)
    => HasSwagger ('NetworkEndpoint ('MempoolNetworkT c) :> api)
  where
    toSwagger _ = toSwagger (Proxy @(NetworkEndpointApi ('MempoolNetworkT c) api))

instance
    (HasSwagger api) => HasSwagger ('NetworkEndpoint 'CutNetworkT :> api)
  where
    toSwagger _ = toSwagger (Proxy @(NetworkEndpointApi 'CutNetworkT api))

-- HasClient

instance
    (KnownChainIdSymbol c, HasClient m api)
    => HasClient m ('NetworkEndpoint ('ChainNetworkT c) :> api)
  where
    type Client m ('NetworkEndpoint ('ChainNetworkT c) :> api)
        = Client m (NetworkEndpointApi ('ChainNetworkT c) api)

    clientWithRoute pm _
        = clientWithRoute pm $ Proxy @(NetworkEndpointApi ('ChainNetworkT c) api)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(NetworkEndpointApi ('ChainNetworkT c) api)

instance
    (KnownChainIdSymbol c, HasClient m api)
    => HasClient m ('NetworkEndpoint ('MempoolNetworkT c) :> api)
  where
    type Client m ('NetworkEndpoint ('MempoolNetworkT c) :> api)
        = Client m (NetworkEndpointApi ('MempoolNetworkT c) api)

    clientWithRoute pm _
        = clientWithRoute pm $ Proxy @(NetworkEndpointApi ('MempoolNetworkT c) api)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(NetworkEndpointApi ('MempoolNetworkT c) api)

instance
    (HasClient m api) => HasClient m ('NetworkEndpoint 'CutNetworkT :> api)
  where
    type Client m ('NetworkEndpoint 'CutNetworkT :> api)
        = Client m (NetworkEndpointApi 'CutNetworkT api)

    clientWithRoute pm _
        = clientWithRoute pm $ Proxy @(NetworkEndpointApi 'CutNetworkT api)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(NetworkEndpointApi 'CutNetworkT api)

-- Has Link

instance
    (KnownChainIdSymbol c, HasLink api)
    => HasLink ('NetworkEndpoint ('ChainNetworkT c) :> api)
  where
    type MkLink ('NetworkEndpoint ('ChainNetworkT c) :> api) a
        = MkLink (NetworkEndpointApi ('ChainNetworkT c) api) a

    toLink toA _
        = toLink toA $ Proxy @(NetworkEndpointApi ('ChainNetworkT c) api)

instance
    (KnownChainIdSymbol c, HasLink api)
    => HasLink ('NetworkEndpoint ('MempoolNetworkT c) :> api)
  where
    type MkLink ('NetworkEndpoint ('MempoolNetworkT c) :> api) a
        = MkLink (NetworkEndpointApi ('MempoolNetworkT c) api) a

    toLink toA _
        = toLink toA $ Proxy @(NetworkEndpointApi ('MempoolNetworkT c) api)

instance (HasLink api) => HasLink ('NetworkEndpoint 'CutNetworkT :> api) where
    type MkLink ('NetworkEndpoint 'CutNetworkT :> api) a
        = MkLink (NetworkEndpointApi 'CutNetworkT api) a

    toLink toA _ = toLink toA $ Proxy @(NetworkEndpointApi 'CutNetworkT api)

-- -------------------------------------------------------------------------- --
-- Some API

-- $someapi
--
-- The chain graph and thus the list of chain ids is encoded as runtime values.
-- In order to combin these in statically defined APIs we reify the the chainweb
-- version and chainid as types and wrap them existenially so that they can
-- be passed around and be combined.

data SomeApi = forall (a :: Type)
    . (HasSwagger a, HasServer a '[], HasClient ClientM a) => SomeApi (Proxy a)

instance Semigroup SomeApi where
    SomeApi (Proxy :: Proxy a) <> SomeApi (Proxy :: Proxy b)
        = SomeApi (Proxy @(a :<|> b))

instance Monoid SomeApi where
    mappend = (<>)
    mempty = SomeApi (Proxy @EmptyAPI)

someApi
    :: forall proxy (a :: Type)
    . HasServer a '[]
    => HasClient ClientM a
    => HasSwagger a
    => proxy a
    -> SomeApi
someApi _ = SomeApi (Proxy @a)

-- -------------------------------------------------------------------------- --
-- Some API Server

data SomeServer = forall (a :: Type)
    . HasServer a '[] => SomeServer (Proxy a) (Server a)

instance Semigroup SomeServer where
    SomeServer (Proxy :: Proxy a) a <> SomeServer (Proxy :: Proxy b) b
        = SomeServer (Proxy @(a :<|> b)) (a :<|> b)

instance Monoid SomeServer where
    mappend = (<>)
    mempty = SomeServer (Proxy @EmptyAPI) emptyServer

someServerApplication :: SomeServer -> Application
someServerApplication (SomeServer a server) = serve a server

-- -------------------------------------------------------------------------- --
-- Content Types For Clients

-- Response Body Content Type

type family SetRespBodyContentType ct api where
    SetRespBodyContentType ct (Verb a b _ c) = Verb a b '[ct] c
    SetRespBodyContentType ct (a :> b) = a :> SetRespBodyContentType ct b

type SupportedRespBodyContentType ct api t = (SupportedRespBodyCT_ ct api api ~ 'True, MimeUnrender ct t)

type family SupportedRespBodyCT_ (ct :: Type) (api :: k) (arg :: k1) :: Bool where
    SupportedRespBodyCT_ ct api (Verb _ _ '[] _) = RespBodyContentTypeNotSupportedMsg ct api
    SupportedRespBodyCT_ ct api (Verb _ _ (ct ': _) _) = 'True
    SupportedRespBodyCT_ ct api (Verb a b (_ ': t) c) = SupportedRespBodyCT_ ct api (Verb a b t c)
    SupportedRespBodyCT_ ct api (a :> b) = SupportedRespBodyCT_ ct api b

type family RespBodyContentTypeNotSupportedMsg ct api where
    RespBodyContentTypeNotSupportedMsg ct api = TypeError
        ( 'Text "The response content type "
        ':<>: 'ShowType ct
        ':<>: 'Text " is not supported by the servant API "
        ':$$: 'ShowType api
        )

-- Request Body Content Type

type family SetReqBodyContentType (ct :: Type) (api :: k) :: k1 where
    SetReqBodyContentType ct (ReqBody _ t :> a) = ReqBody '[ct] t :> a
    SetReqBodyContentType ct (a :> b) = a :> SetReqBodyContentType ct b

type SupportedReqBodyContentType ct api t = (SupportedReqBodyCT_ ct api api ~ 'True, MimeRender ct t)

type family SupportedReqBodyCT_ (ct :: Type) (api :: k) (arg :: k1) :: Bool where
    SupportedReqBodyCT_ ct api (ReqBody '[] _ :> _) = ReqBodyContentTypeNotSupportedMsg ct api
    SupportedReqBodyCT_ ct api (ReqBody (ct ': _) _ :> _) = 'True
    SupportedReqBodyCT_ ct api (ReqBody (_ ': x) t :> a) = SupportedReqBodyCT_ ct api (ReqBody x t :> a)
    SupportedReqBodyCT_ ct api (a :> b) = SupportedReqBodyCT_ ct api b

type family ReqBodyContentTypeNotSupportedMsg ct api where
    ReqBodyContentTypeNotSupportedMsg ct api = TypeError
        ( 'Text "The request content type "
        ':<>: 'ShowType ct
        ':<>: 'Text " is not supported by the servant API "
        ':$$: 'ShowType api
        )

-- -------------------------------------------------------------------------- --
-- Misc Utils

bindPortTcp :: Port -> HostPreference -> IO (Port, N.Socket)
bindPortTcp p interface = do
    (port, sock) <- do
        socket <- bindPortGen N.Stream (int p) interface
        port <- N.socketPort socket
        return (int port, socket)
    N.listen sock (max 2048 N.maxListenQueue)
    return (port, sock)

allocateSocket :: Port -> HostPreference -> IO (Port, N.Socket)
allocateSocket port interface = do
    (!p, !sock) <- bindPortTcp port interface
    return (p, sock)

deallocateSocket :: (Port, N.Socket) -> IO ()
deallocateSocket (_, sock) = N.close sock

withSocket :: Port -> HostPreference -> ((Port, N.Socket) -> IO a) -> IO a
withSocket port interface = bracket (allocateSocket port interface) deallocateSocket

