{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.SPV.RestAPI
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>, Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- This module defines the API for the main Pact service
-- and its spv continuation proof endpoints.
--
module Chainweb.Pact.RestAPI
(
-- * Pact V1 Api
  PactApi
, pactApi

-- * Pact Spv Api
, PactSpvApi
, pactSpvApi

-- * Pact Service Api
, PactServiceApi
, pactServiceApi

-- * Some Spv Api
, somePactServiceApi
, somePactServiceApis
) where


import Servant

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Pact.Service.Types
import Chainweb.RestAPI.Utils
import Chainweb.Version

import Data.Singletons

-- internal pact modules

import Pact.Server.API as API
import Pact.Types.Command (RequestKey)

-- -------------------------------------------------------------------------- --
-- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/@

type PactApi_ = "pact" :> API.ApiV1API -- TODO unify with Pact versioning

type PactApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc PactApi_

pactApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactApi v c)
pactApi = Proxy

-- -------------------------------------------------------------------------- --
-- GET Pact Spv Transaction Proof

type PactSpvApi_
    = "pact"
    :> "spv"
    :> Capture "chainId" ChainId
    :> Capture "txHash" RequestKey
    :> Get '[PlainText] TransactionOutputProofB64

type PactSpvApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PactSpvApi_

pactSpvApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactSpvApi v c)
pactSpvApi = Proxy

-- -------------------------------------------------------------------------- --
-- PactService Api

type PactServiceApi v c
    = PactApi v c
    :<|> PactSpvApi v c

pactServiceApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) . Proxy (PactServiceApi v c)
pactServiceApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

somePactServiceApi :: ChainwebVersion -> ChainId -> SomeApi
somePactServiceApi
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = SomeApi $ pactServiceApi @v @c

somePactServiceApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePactServiceApis v = mconcat . fmap (somePactServiceApi v)
