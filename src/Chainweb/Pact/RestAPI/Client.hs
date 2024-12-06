{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Pact.RestAPI.Client
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- This module defines the client API for the main Pact service
-- and its spv continuation proof endpoints.
module Chainweb.Pact.RestAPI.Client
( pactSpvApiClient_
, pactSpvApiClient
, pactSpv2ApiClient_
, pactSpv2ApiClient
, ethSpvApiClient_
, ethSpvApiClient
, pactListenApiClient_
, pactListenApiClient
, pactSendApiClient_
, pactSendApiClient
, pactLocalApiClient_
, pactLocalApiClient
, pactLocalWithQueryApiClient_
, pactLocalWithQueryApiClient
, pactPollWithQueryApiClient_
, pactPollWithQueryApiClient
) where


import qualified Data.Text as T

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash

import Servant.Client

-- internal modules

import Chainweb.ChainId
import Chainweb.Pact.RestAPI
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.RestAPI.SPV
import Chainweb.Pact.Types
import Chainweb.SPV.PayloadProof
import Chainweb.Version
import qualified Pact.Core.Command.Server as Pact5

-- -------------------------------------------------------------------------- --
-- Pact Spv Transaction Output Proof Client

pactSpvApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => SpvRequest
        -- ^ Contains the chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> ClientM TransactionOutputProofB64
pactSpvApiClient_ = client (pactSpvApi @v @c)

pactSpvApiClient
    :: ChainwebVersion
    -> ChainId
        -- ^ the chain id of the source chain id used in the
        -- execution of a cross-chain-transfer.
    -> SpvRequest
        -- ^ Contains the chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> ClientM TransactionOutputProofB64
pactSpvApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactSpvApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact ETH Spv Transaction Output Proof Client

ethSpvApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => EthSpvRequest
    -> ClientM EthSpvResponse
ethSpvApiClient_ = client (ethSpvApi @v @c)

ethSpvApiClient
    :: ChainwebVersion
    -> ChainId
        -- ^ chain to which the request is submitted. The resuting proof does
        -- not depend on the chain and can be validated on any chain.
    -> EthSpvRequest
    -> ClientM EthSpvResponse
ethSpvApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = ethSpvApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact ETH Spv Transaction Output Proof Client

pactSpv2ApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Spv2Request
        -- ^ Contains the chain id of the target chain id used in the
        -- 'target-chain' field of a cross-chain-transfer.
        -- Also contains the request key of of the cross-chain transfer
        -- tx request.
    -> ClientM SomePayloadProof
pactSpv2ApiClient_ = client (pactSpv2Api @v @c)

pactSpv2ApiClient
    :: ChainwebVersion
    -> ChainId
        -- ^ the chain id of the target chain of the proof.
    -> Spv2Request
    -> ClientM SomePayloadProof
pactSpv2ApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactSpv2ApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact local

pactLocalApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Command T.Text
    -> ClientM (CommandResult Hash)
pactLocalApiClient_ = client (pactLocalApi @v @c)

pactLocalApiClient
    :: ChainwebVersion
    -> ChainId
    -> Command T.Text
    -> ClientM (CommandResult Hash)
pactLocalApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactLocalApiClient_ @v @c

pactLocalWithQueryApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Command T.Text
    -> ClientM LocalResult
pactLocalWithQueryApiClient_ = client (pactLocalWithQueryApi @v @c)

pactLocalWithQueryApiClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe LocalPreflightSimulation
    -> Maybe LocalSignatureVerification
    -> Maybe RewindDepth
    -> Command T.Text
    -> ClientM LocalResult
pactLocalWithQueryApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactLocalWithQueryApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact Listen

pactListenApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Pact5.ListenRequest
    -> ClientM Pact5.ListenResponse
pactListenApiClient_ = client (pactListenApi @v @c)

pactListenApiClient
    :: ChainwebVersion
    -> ChainId
    -> Pact5.ListenRequest
    -> ClientM Pact5.ListenResponse
pactListenApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactListenApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact Send

pactSendApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => SubmitBatch
    -> ClientM RequestKeys
pactSendApiClient_ = client (pactSendApi @v @c)

pactSendApiClient
    :: ChainwebVersion
    -> ChainId
    -> SubmitBatch
    -> ClientM RequestKeys
pactSendApiClient
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = pactSendApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact Poll

pactPollWithQueryApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe ConfirmationDepth
    -> Pact5.PollRequest
    -> ClientM Pact5.PollResponse
pactPollWithQueryApiClient_ = client (pactPollWithQueryApi @v @c)

pactPollWithQueryApiClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe ConfirmationDepth
    -> Pact5.PollRequest
    -> ClientM Pact5.PollResponse
pactPollWithQueryApiClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) (FromSingChainId (SChainId :: Sing c)) confirmationDepth poll = do
    pactPollWithQueryApiClient_ @v @c confirmationDepth poll
