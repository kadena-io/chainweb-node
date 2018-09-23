{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- * API Version
, Version
, ApiVersion(..)
, apiVersion
, prettyApiVersion

-- * Some API
--
-- $someapi
, SomeApi(..)
, someApi

-- ** Some Server
, SomeServer(..)
, someServerApplication

-- ** Typelevel ChainID
, ChainIdT(..)
, ChainIdSymbol
, chainIdSymbolVal
, SomeChainIdT(..)
, KnownChainIdSymbol
, someChainIdVal

-- ** Typelevel ChainwebVersion
, ChainwebVersionT(..)
, ChainwebVersionSymbol
, chainwebVersionSymbolVal
, SomeChainwebVersionT(..)
, KnownChainwebVersionSymbol
, someChainwebVersionVal

-- * Chainweb API Endpoints
, ChainwebEndpoint
, ChainEndpoint
) where

import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Kind
import Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Swagger
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger

-- internal modules
import Chainweb.ChainId
import Chainweb.Utils
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
    { _pageLimit :: !Natural
    , _pageItems :: ![a]
    , _pageNext :: !(Maybe k)
    }
    deriving (Show, Eq, Ord, Generic)

instance (ToJSON k, ToJSON a) => ToJSON (Page k a) where
    toJSON p = object
        [ "limit" .= _pageLimit p
        , "items" .= _pageItems p
        , "next" .= _pageNext p
        ]

instance (FromJSON k, FromJSON a) => FromJSON (Page k a) where
    parseJSON = withObject "page" $ \o -> Page
        <$> o .: "limit"
        <*> o .: "items"
        <*> o .: "next"

instance (ToSchema k, ToSchema a) => ToSchema (Page k a) where
    declareNamedSchema _ = do
        naturalSchema <- declareSchemaRef (Proxy :: Proxy Natural)
        keySchema <- declareSchemaRef (Proxy :: Proxy k)
        itemsSchema <- declareSchemaRef (Proxy :: Proxy [a])
        return $ NamedSchema (Just "Page") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
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

type LimitParam = QueryParam "limit" Natural
type NextParam k = QueryParam "next" k

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
-- Type level ChainId

-- it's easier to use Symbol here. If we wanted to use Nat we'd need a
-- typelevel function Nat -> Symbol
--
newtype ChainIdT = ChainIdT Symbol

data SomeChainIdT = forall (a :: ChainIdT)
    . KnownChainIdSymbol a => SomeChainIdT (Proxy a)

class KnownSymbol (ChainIdSymbol n) => KnownChainIdSymbol (n :: ChainIdT) where
    type ChainIdSymbol n :: Symbol
    chainIdSymbolVal :: Proxy n -> String

instance KnownSymbol n => KnownChainIdSymbol ('ChainIdT n) where
    type ChainIdSymbol ('ChainIdT n) = n
    chainIdSymbolVal _ = symbolVal (Proxy @n)

someChainIdVal :: ChainId -> SomeChainIdT
someChainIdVal cid = case someSymbolVal (T.unpack (prettyChainId cid)) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainIdT (Proxy @('ChainIdT v))

-- -------------------------------------------------------------------------- --
-- Type level ChainwebVersion

newtype ChainwebVersionT = ChainwebVersionT Symbol

data SomeChainwebVersionT = forall (a :: ChainwebVersionT)
        . KnownChainwebVersionSymbol a => SomeChainwebVersionT (Proxy a)

class KnownSymbol (ChainwebVersionSymbol n) => KnownChainwebVersionSymbol (n :: ChainwebVersionT) where
    type ChainwebVersionSymbol n :: Symbol
    chainwebVersionSymbolVal :: Proxy n -> String

instance (KnownSymbol n) => KnownChainwebVersionSymbol ('ChainwebVersionT n) where
    type ChainwebVersionSymbol ('ChainwebVersionT n) = n
    chainwebVersionSymbolVal _ = symbolVal (Proxy @n)

someChainwebVersionVal :: ChainwebVersion -> SomeChainwebVersionT
someChainwebVersionVal v = case someSymbolVal (sshow v) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainwebVersionT (Proxy @('ChainwebVersionT v))

-- -------------------------------------------------------------------------- --
-- Chainweb API Endpoints

-- | Chainweb Endpoint Constructor
--
type family ChainwebEndpoint (v :: ChainwebVersionT) (e :: Type) where
    ChainwebEndpoint v e
        = "chainweb" :> Version :> ChainwebVersionSymbol v
        :> e

-- | Chain Endpoint Constructor
--
type family ChainEndpoint (v :: ChainwebVersionT) (cid :: ChainIdT) (e :: Type) where
    ChainEndpoint v c e = ChainwebEndpoint v ("chain" :> ChainIdSymbol c :> e)

