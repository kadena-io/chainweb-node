{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: P2P.Node.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Node.RestAPI
(
-- * P2P API
  PeerGetApi
, peerGetApi
, PeerPutApi
, peerPutApi
, P2pApi
, p2pApi

-- * Some P2P API
, someP2pApi
, someP2pApis
) where

import Control.Lens hiding ((.=))

import Data.Bifunctor
import Data.Proxy
import Data.Swagger
import qualified Data.Text.Encoding as T
import qualified Data.UUID as V4

import Servant

-- internal modules

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Version

import P2P.Node.Configuration

-- -------------------------------------------------------------------------- --
-- Instances

-- FIXME
--
instance ToSchema HostAddress where
    declareNamedSchema _ = return $ NamedSchema (Just "HostAddress") $ mempty
        & type_ .~ SwaggerString
        & pattern ?~ "<hostname>:<port>"
        & minLength ?~ 3
        & maxLength ?~ 258

instance FromHttpApiData HostAddress where
    parseUrlPiece = first sshow . readHostAddressBytes . T.encodeUtf8

instance ToSchema PeerId where
    declareNamedSchema _ = declareNamedSchema (Proxy @V4.UUID)

instance ToParamSchema PeerId where
    toParamSchema _ = toParamSchema (Proxy @V4.UUID)

deriving newtype instance FromHttpApiData PeerId
deriving newtype instance ToHttpApiData PeerId

instance ToSchema PeerInfo

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/peer/@

type PeerGetApi_
    = "peer"
    :> PageParams PeerId
    :> Get '[JSON] (Page PeerId PeerInfo)

type PeerGetApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c PeerGetApi_)

peerGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PeerGetApi v c)
peerGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- @PUT /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/peer/@

type PeerPutApi_
    = "peer"
    :> ReqBody '[JSON] PeerInfo
    :> PutNoContent '[JSON] NoContent

type PeerPutApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c PeerPutApi_)

peerPutApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PeerPutApi v c)
peerPutApi = Proxy

-- -------------------------------------------------------------------------- --
-- P2P API

type P2pApi v c
    = PeerGetApi v c
    :<|> PeerPutApi v c

p2pApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (P2pApi v c)
p2pApi = Proxy

-- -------------------------------------------------------------------------- --
-- Mulit Chain API

someP2pApi :: ChainwebVersion -> ChainId -> SomeApi
someP2pApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (p2pApi @v' @c')

someP2pApis :: ChainwebVersion -> [ChainId] -> SomeApi
someP2pApis v = mconcat . fmap (someP2pApi v)

