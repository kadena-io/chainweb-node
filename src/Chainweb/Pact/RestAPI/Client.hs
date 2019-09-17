{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module: Chainweb.Pact.RestAPI.Client
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- This module defines the client API for the main Pact service
-- and its spv continuation proof endpoints.
module Chainweb.Pact.RestAPI.Client
( pactSpvApiClient
)
where

import Servant.Client

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.Pact.Service.Types
import Chainweb.Pact.RestAPI
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
