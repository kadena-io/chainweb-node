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
    :: ChainwebVersion
    -> NetworkId
    -> Maybe Limit
    -> Maybe (NextItem Int)
    -> ClientM (Page (NextItem Int) PeerInfo)
peerGetClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = f
  where
    f (FromSingNetworkId (SChainNetwork SChainId :: Sing n)) = client $ peerGetApi @v @n
    f (FromSingNetworkId (SMempoolNetwork SChainId :: Sing n)) = client $ peerGetApi @v @n
    f (FromSingNetworkId (SCutNetwork :: Sing n)) = client $ peerGetApi @v @n

-- -------------------------------------------------------------------------- --
-- PUT Peer Client

peerPutClient
    :: ChainwebVersion
    -> NetworkId
    -> PeerInfo
    -> ClientM NoContent
peerPutClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = f
  where
    f (FromSingNetworkId (SChainNetwork SChainId :: Sing n)) = client $ peerPutApi @v @n
    f (FromSingNetworkId (SMempoolNetwork SChainId :: Sing n)) = client $ peerPutApi @v @n
    f (FromSingNetworkId (SCutNetwork :: Sing n)) = client $ peerPutApi @v @n

