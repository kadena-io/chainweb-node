{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.Node.RestAPI.Client
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- REST API client implementation for the Chainweb P2P network endpoints.
--
module P2P.Node.RestAPI.Client
( peerGetClient
, peerPutClient
) where

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules

import Chainweb.ChainId
import Chainweb.RestAPI.NetworkID
import Chainweb.Utils.Paging
import Chainweb.Version

import P2P.Node.RestAPI
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- GET Peer Client

peerGetClient
    :: HasVersion
    => NetworkId
    -> Maybe Limit
    -> Maybe (NextItem Int)
    -> ClientM (Page (NextItem Int) PeerInfo)
peerGetClient nid =
  case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) -> case nid of
      FromSingNetworkId (SChainNetwork SChainId :: Sing n) -> client $ peerGetApi @v @n
      FromSingNetworkId (SMempoolNetwork SChainId :: Sing n) -> client $ peerGetApi @v @n
      FromSingNetworkId (SCutNetwork :: Sing n) -> client $ peerGetApi @v @n

-- -------------------------------------------------------------------------- --
-- PUT Peer Client

peerPutClient
    :: HasVersion
    => NetworkId
    -> PeerInfo
    -> ClientM NoContent
peerPutClient n = case implicitVersion of
  FromSingChainwebVersion (SChainwebVersion :: Sing v) -> case n of
    FromSingNetworkId (SChainNetwork SChainId :: Sing n) -> client $ peerPutApi @v @n
    FromSingNetworkId (SMempoolNetwork SChainId :: Sing n) -> client $ peerPutApi @v @n
    FromSingNetworkId (SCutNetwork :: Sing n) -> client $ peerPutApi @v @n
