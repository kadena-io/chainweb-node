{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Pact.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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

-- ** Pact APIs for Individual commands
, PactLocalApi
, pactLocalApi
, PactListenApi
, pactListenApi
, PactSendApi
, pactSendApi
, PactLocalWithQueryApi
, pactLocalWithQueryApi
, PactPollWithQueryApi
, pactPollWithQueryApi
-- * Pact Spv Api
, PactSpvApi
, pactSpvApi

-- * Pact Spv Api Version 2
, PactSpv2Api
, pactSpv2Api

-- * Eth Spv Api
, EthSpvApi
, ethSpvApi

-- * Pact Service Api
, PactServiceApi
, pactServiceApi

-- * Some Pact Service Api
, somePactServiceApi
, somePactServiceApis
) where


import Data.Text (Text)

import qualified Pact.Types.Command as Pact
import qualified Pact.Server.API as Pact4
import Pact.Utils.Servant

import Servant

-- internal modules

import Chainweb.ChainId
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Types
import Chainweb.RestAPI.Utils
import Chainweb.SPV.PayloadProof
import Chainweb.Version
import qualified Pact.Core.Command.Server as Pact5

-- -------------------------------------------------------------------------- --
-- @POST /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/@

-- TODO unify with Pact versioning
type PactApi_
    = "pact"
    :> "api"
    :> "v1"
    :> ( Pact4.ApiSend
       :<|> PactPollWithQueryApi_
       :<|> ApiListen
       :<|> PactLocalWithQueryApi_
       )

type PactApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc PactApi_

pactApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactApi v c)
pactApi = Proxy

-- -------------------------------------------------------------------------- --
-- Individual Pact V1 Endpoints

type PactV1ApiEndpoint (v :: ChainwebVersionT) (c :: ChainIdT) api
    = 'ChainwebEndpoint v
    :> ChainEndpoint c
    :> "pact"
    :> "api"
    :> "v1"
    :> api

type PactLocalApi v c = PactV1ApiEndpoint v c Pact4.ApiLocal
type PactSendApi v c = PactV1ApiEndpoint v c Pact4.ApiSend
type PactListenApi v c = PactV1ApiEndpoint v c ApiListen

type ApiListen = ("listen" :> ReqBody '[PactJson] Pact5.ListenRequest :> Post '[PactJson] Pact5.ListenResponse)

pactLocalApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactLocalApi v c)
pactLocalApi = Proxy

pactSendApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactSendApi v c)
pactSendApi = Proxy

pactListenApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactListenApi v c)
pactListenApi = Proxy

-- -------------------------------------------------------------------------- --
-- POST Queries for Pact Local Pre-flight

type PactLocalWithQueryApi_
    = "local"
    :> QueryParam "preflight" LocalPreflightSimulation
    :> QueryParam "signatureVerification" LocalSignatureVerification
    :> QueryParam "rewindDepth" RewindDepth
    :> ReqBody '[PactJson] (Pact.Command Text)
    :> Post '[PactJson] LocalResult

type PactLocalWithQueryApi v c = PactV1ApiEndpoint v c PactLocalWithQueryApi_

pactLocalWithQueryApi
  :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
  . Proxy (PactLocalWithQueryApi v c)
pactLocalWithQueryApi = Proxy

-- -------------------------------------------------------------------------- --
-- POST Queries for Pact Poll

type PactPollWithQueryApi_
    = "poll"
    :> QueryParam "confirmationDepth" ConfirmationDepth
    :> ReqBody '[PactJson] Pact5.PollRequest
    :> Post '[PactJson] Pact5.PollResponse

type PactPollWithQueryApi v c = PactV1ApiEndpoint v c PactPollWithQueryApi_

pactPollWithQueryApi
  :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
  . Proxy (PactPollWithQueryApi v c)
pactPollWithQueryApi = Proxy

-- -------------------------------------------------------------------------- --
-- POST Pact Spv Transaction Proof

type PactSpvApi_
    = "pact"
    :> "spv"
    :> ReqBody '[PactJson] SpvRequest
    :> Post '[JSON] TransactionOutputProofB64

type PactSpvApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PactSpvApi_

pactSpvApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactSpvApi v c)
pactSpvApi = Proxy

-- -------------------------------------------------------------------------- --
-- POST Query for Pact SPV Proof

--  | The chain endpoint is the target chain of the proof, i.e. the chain where
--  the root of the proof is located.
--
type PactSpv2Api_
    = "pact"
    :> "spv2"
    :> ReqBody '[JSON] Spv2Request
    :> Post '[JSON] SomePayloadProof

type PactSpv2Api (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PactSpv2Api_

pactSpv2Api
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactSpv2Api v c)
pactSpv2Api = Proxy

-- -------------------------------------------------------------------------- --
-- POST Eth Receipt SPV Proof

type EthSpvApi_
    = "pact"
    :> "spv"
    :> "eth"
    :> ReqBody '[JSON] EthSpvRequest
    :> Post '[JSON] EthSpvResponse

type EthSpvApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> EthSpvApi_

ethSpvApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (EthSpvApi v c)
ethSpvApi = Proxy

-- -------------------------------------------------------------------------- --
-- PactService Api

type PactServiceApi v c
    = PactApi v c
    :<|> PactSpvApi v c
    :<|> EthSpvApi v c
    :<|> PactSpv2Api v c

pactServiceApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactServiceApi v c)
pactServiceApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

somePactServiceApi :: ChainwebVersion -> ChainId -> SomeApi
somePactServiceApi
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = SomeApi $ pactServiceApi @v @c

somePactServiceApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePactServiceApis v = mconcat . fmap (somePactServiceApi v)
