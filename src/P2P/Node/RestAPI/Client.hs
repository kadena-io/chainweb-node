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
-- TODO
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

import Data.Singletons

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
peerGetClient (FromSing (SChainwebVersion :: Sing v)) = f
  where
    f (FromSing (SChainNetwork SChainId :: Sing n)) = client $ peerGetApi @v @n
    f (FromSing (SMempoolNetwork SChainId :: Sing n)) = client $ peerGetApi @v @n
    f (FromSing (SCutNetwork :: Sing n)) = client $ peerGetApi @v @n

-- -------------------------------------------------------------------------- --
-- PUT Peer Client

peerPutClient
    :: ChainwebVersion
    -> NetworkId
    -> PeerInfo
    -> ClientM NoContent
peerPutClient (FromSing (SChainwebVersion :: Sing v)) = f
  where
    f (FromSing (SChainNetwork SChainId :: Sing n)) = client $ peerPutApi @v @n
    f (FromSing (SMempoolNetwork SChainId :: Sing n)) = client $ peerPutApi @v @n
    f (FromSing (SCutNetwork :: Sing n)) = client $ peerPutApi @v @n
