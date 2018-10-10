{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Node.RestAPI.Client
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.Node.RestAPI.Client
( peerGetClient_
, peerGetClient
, peerPutClient_
, peerPutClient
) where

import Control.Monad.Identity

import Data.Proxy

import Numeric.Natural

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules
import P2P.Node.PeerDB
import P2P.Node.RestAPI
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- GET Peer Client

peerGetClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Natural
    -> Maybe PeerId
    -> ClientM (Page PeerId PeerInfo)
peerGetClient_ = client (peerGetApi @v @c)

peerGetClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Natural
    -> Maybe PeerId
    -> ClientM (Page PeerId PeerInfo)
peerGetClient v c limit next = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ peerGetClient_ @v @c limit next

-- -------------------------------------------------------------------------- --
-- PUT Peer Client

peerPutClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => PeerInfo
    -> ClientM NoContent
peerPutClient_ = client (peerPutApi @v @c)

peerPutClient
    :: ChainwebVersion
    -> ChainId
    -> PeerInfo
    -> ClientM NoContent
peerPutClient v c e = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ peerPutClient_ @v @c e

