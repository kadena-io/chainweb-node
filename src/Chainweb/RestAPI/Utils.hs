{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- * Paging
, Page(..)
, PageParams
, streamToPage

-- * API Version
, Version
, ApiVersion(..)
, apiVersion
, prettyApiVersion

-- * Chainweb API Endpoints
, ChainwebEndpoint(..)
, NetworkEndpoint(..)
, ChainEndpoint
, CutEndpoint

-- * Some API
--
-- $someapi
, SomeApi(..)
, someApi

-- ** Some Server
, SomeServer(..)
, someServerApplication

-- * Properties
, properties
) where

import Control.Lens hiding ((.=), (:>))

import Data.Aeson
import Data.Functor.Of
import Data.Kind
import Data.Maybe
import Data.Proxy
import qualified Data.Swagger as Swagger
import Data.Swagger hiding (properties)
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger

import qualified Streaming.Prelude as SP

import Test.QuickCheck
import Test.QuickCheck.Instances.Natural ({- Arbitrary Natural -})

-- internal modules

import Chainweb.ChainId
import Chainweb.RestAPI.NetworkID
import Chainweb.TreeDB (Limit(..))
import Chainweb.Utils hiding ((==>))
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Servant Utils

type Reassoc (api :: Type) = ReassocBranch api '[]

type family ReassocBranch (a :: s) (b :: [Type]) :: Type where
    ReassocBranch (a :> b) rest = ReassocBranch a (b ': rest)
    ReassocBranch a '[] = a
    ReassocBranch a (b ': rest) = a :> ReassocBranch b rest

-- -------------------------------------------------------------------------- --
-- Paging

data Page k a = Page
    { _pageLimit :: !Limit
    , _pageItems :: ![a]
    , _pageNext :: !(Maybe k)
    }
    deriving (Show, Eq, Ord, Generic)

instance (ToJSON k, ToJSON a) => ToJSON (Page k a) where
    toJSON p = object
        [ "limit" .= _getLimit (_pageLimit p)
        , "items" .= _pageItems p
        , "next" .= _pageNext p
        ]

instance (FromJSON k, FromJSON a) => FromJSON (Page k a) where
    parseJSON = withObject "page" $ \o -> Page
        <$> (Limit <$> (o .: "limit"))
        <*> o .: "items"
        <*> o .: "next"

instance (ToSchema k, ToSchema a) => ToSchema (Page k a) where
    declareNamedSchema _ = do
        naturalSchema <- declareSchemaRef (Proxy :: Proxy Natural)
        keySchema <- declareSchemaRef (Proxy :: Proxy k)
        itemsSchema <- declareSchemaRef (Proxy :: Proxy [a])
        return $ NamedSchema (Just "Page") $ mempty
            & type_ .~ SwaggerObject
            & Swagger.properties .~
                [ ("limit", naturalSchema)
                , ("items", itemsSchema)
                , ("next", keySchema)
                ]
            & required .~ [ "limit", "items" ]

-- | Pages Parameters
--
-- *   maxitems :: Natural
-- *   from :: BlockHash
--
type PageParams k = LimitParam :> NextParam k

type LimitParam = QueryParam "limit" Limit
type NextParam k = QueryParam "next" k

-- -------------------------------------------------------------------------- --
-- Paging Tools

-- | Quick and dirty pagin implementation
--
streamToPage
    :: Monad m
    => Eq k
    => (a -> k)
    -> Maybe k
    -> Maybe Limit
    -> SP.Stream (Of a) m ()
    -> m (Page k a)
streamToPage k next limit s = do
    (items' :> limit' :> tailStream) <- id

        -- count and collect items from first stream
        . SP.toList
        . SP.length
        . SP.copy

        -- split the stream
        . maybe (SP.each ([]::[a]) <$) (SP.splitAt . int) limit

        -- search for requested next item
        . maybe id (\n -> SP.dropWhile (\x -> k x /= n)) next
        $ s

    -- get next item from the tail stream
    next' <- SP.head_ tailStream

    return $ Page (int limit') items' (k <$> next')

prop_streamToPage_limit :: [Int] -> Limit -> Property
prop_streamToPage_limit l i = i <= len l ==> actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (i == len l) "limit == length of stream"
    & cover 1 (i == 0) "limit == 0"
    & cover 1 (length l == 0) "length of stream == 0"
#endif
  where
    actual = runIdentity (streamToPage id Nothing (Just i) (SP.each l))
    expected = Page i (take (int i) l) (listToMaybe $ drop (int i) l)

prop_streamToPage_id :: [Int] -> Property
prop_streamToPage_id l = actual === expected
#if MIN_VERSION_QuickCheck(2,12,0)
    & cover 1 (length l == 0) "len l == 0"
#endif
  where
    actual = runIdentity (streamToPage id Nothing Nothing (SP.each l))
    expected = Page (len l) l Nothing

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
-- Chainweb API Endpoints

newtype ChainwebEndpoint = ChainwebEndpoint ChainwebVersionT

type ChainwebEndpointApi c a = "chainweb" :> Version :> ChainwebVersionSymbol c :> a

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
        = Client m (NetworkEndpointApi 'CutNetworkT api)

    clientWithRoute pm _
        = clientWithRoute pm $ Proxy @(ChainwebEndpointApi v api)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(ChainwebEndpointApi v api)

-- -------------------------------------------------------------------------- --
-- Network API Endpoint

newtype NetworkEndpoint = NetworkEndpoint NetworkIdT

type ChainEndpoint (c :: ChainIdT) = 'NetworkEndpoint ('ChainNetworkT c)
type CutEndpoint = 'NetworkEndpoint 'CutNetworkT

type family NetworkEndpointApi (net :: NetworkIdT) (api :: Type) :: Type where
    NetworkEndpointApi 'CutNetworkT api = "cut" :> api
    NetworkEndpointApi ('ChainNetworkT c) api = "chain" :> ChainIdSymbol c :> api

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
    (HasClient m api) => HasClient m ('NetworkEndpoint 'CutNetworkT :> api)
  where
    type Client m ('NetworkEndpoint 'CutNetworkT :> api)
        = Client m (NetworkEndpointApi 'CutNetworkT api)

    clientWithRoute pm _
        = clientWithRoute pm $ Proxy @(NetworkEndpointApi 'CutNetworkT api)
    hoistClientMonad pm _
        = hoistClientMonad pm $ Proxy @(NetworkEndpointApi 'CutNetworkT api)

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
-- Properties

properties :: [(String, Property)]
properties =
    [ ("streamToPage_limit", property prop_streamToPage_limit)
    , ("streamToPage_id", property prop_streamToPage_id)
    ]
