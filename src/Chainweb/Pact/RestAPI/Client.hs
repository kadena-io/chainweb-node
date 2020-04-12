{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
, pactPollApiClient_
, pactPollApiClient
, pactListenApiClient_
, pactListenApiClient
, pactSendApiClient_
, pactSendApiClient
, pactLocalApiClient_
, pactLocalApiClient
)
where

import qualified Data.Text as T

import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash

import Servant.Client

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Pact.RestAPI
import Chainweb.Pact.Service.Types
import Chainweb.Version

import Data.Singletons

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
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = pactSpvApiClient_ @v @c

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
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = pactLocalApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact Listen

pactListenApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ListenerRequest
    -> ClientM ListenResponse
pactListenApiClient_ = client (pactListenApi @v @c)

pactListenApiClient
    :: ChainwebVersion
    -> ChainId
    -> ListenerRequest
    -> ClientM ListenResponse
pactListenApiClient
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
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
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = pactSendApiClient_ @v @c

-- -------------------------------------------------------------------------- --
-- Pact Poll

pactPollApiClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Poll
    -> ClientM PollResponses
pactPollApiClient_ = client (pactPollApi @v @c)

pactPollApiClient
    :: ChainwebVersion
    -> ChainId
    -> Poll
    -> ClientM PollResponses
pactPollApiClient
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = pactPollApiClient_ @v @c
