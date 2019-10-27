{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.CutDB.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB.RestAPI
(
-- * Cut API
  CutGetApi
, cutGetApi
, CutPutApi
, cutPutApi
, CutApi
, cutApi

-- * Some Cut API
, someCutApi
) where

import Data.Proxy

import Servant

-- internal modules

import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.RestAPI.NetworkID
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Version

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/cut@

type CutGetApi_
    = QueryParam "maxheight" MaxRank
    :> Get '[JSON] CutHashes

type CutGetApi (v :: ChainwebVersionT)
    = 'ChainwebEndpoint v :> 'NetworkEndpoint 'CutNetworkT :> Reassoc CutGetApi_

cutGetApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (CutGetApi v)
cutGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- @PUT /chainweb/<ApiVersion>/<ChainwebVersion>/cut/@

type CutPutApi_
    = ReqBody '[JSON] CutHashes
    :> Verb 'PUT 204 '[JSON] NoContent

type CutPutApi (v :: ChainwebVersionT)
    = 'ChainwebEndpoint v :> 'NetworkEndpoint 'CutNetworkT :> Reassoc CutPutApi_

cutPutApi
    :: forall (v :: ChainwebVersionT)
    . Proxy (CutPutApi v)
cutPutApi = Proxy

-- -------------------------------------------------------------------------- --
-- Cut API

type CutApi v
    = CutGetApi v
    :<|> CutPutApi v

cutApi :: forall (v :: ChainwebVersionT) . Proxy (CutApi v)
cutApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

someCutApi :: ChainwebVersion -> SomeApi
someCutApi (FromSing (SChainwebVersion :: Sing v)) = SomeApi $ cutApi @v
