{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.SPV.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- SPV RestAPI
--
module Chainweb.SPV.RestAPI
(
-- * Transaction Proof API
  SpvGetTransactionProofApi
, spvGetTransactionProofApi

-- * Transaction Output Proof API
, SpvGetTransactionOutputProofApi
, spvGetTransactionOutputProofApi

-- * Event Proof API
, SpvGetEventProofApi
, spvGetEventProofApi

-- * SPV API
, SpvApi
, spvApi

-- * Some SPV API
, someSpvApi
, someSpvApis
) where

import Crypto.Hash.Algorithms

import Data.Proxy

import Numeric.Natural

import Servant.API

-- internal modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.SPV
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- GET Transaction Proof

type SpvGetTransactionProofApi_
    = "spv"
    :> "chain" :> Capture "spvChain" ChainId
    :> "height" :> Capture "spvHeight" BlockHeight
    :> "transaction" :> Capture "spvTransactionIndex" Natural
    :> Get '[JSON] (TransactionProof SHA512t_256)

type SpvGetTransactionProofApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> SpvGetTransactionProofApi_

spvGetTransactionProofApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (SpvGetTransactionProofApi v c)
spvGetTransactionProofApi = Proxy

-- -------------------------------------------------------------------------- --
-- GET Transaction Output Proof

type SpvGetTransactionOutputProofApi_
    = "spv"
    :> "chain" :> Capture "spvChain" ChainId
    :> "height" :> Capture "spvHeight" BlockHeight
    :> "output" :> Capture "spvTransactionOutputIndex" Natural
    :> Get '[JSON] (TransactionOutputProof SHA512t_256)

type SpvGetTransactionOutputProofApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> SpvGetTransactionOutputProofApi_

spvGetTransactionOutputProofApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (SpvGetTransactionOutputProofApi v c)
spvGetTransactionOutputProofApi = Proxy

-- -------------------------------------------------------------------------- --
-- GET Event Output Proof

type SpvGetEventProofApi_
    = "spv"
    :> "chain" :> Capture "spvChain" ChainId
    :> "height" :> Capture "spvHeight" BlockHeight
    :> "transaction" :> Capture "spvTransactionactionIndex" Natural
    :> "event" :> Capture "spvEventIndex" Natural
    :> Get '[JSON] FakeEventProof

type SpvGetEventProofApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> SpvGetEventProofApi_

spvGetEventProofApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (SpvGetEventProofApi v c)
spvGetEventProofApi = Proxy

-- -------------------------------------------------------------------------- --
-- SPV API

type SpvApi v c
    = SpvGetEventProofApi v c
    -- :<|>SpvGetTransactionProofApi v c
    -- :<|> SpvGetTransactionOutputProofApi v c

spvApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) . Proxy (SpvApi v c)
spvApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

someSpvApi :: ChainwebVersion -> ChainId -> SomeApi
someSpvApi
    (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    (FromSingChainId (SChainId :: Sing c))
    = SomeApi $ spvApi @v @c

someSpvApis :: ChainwebVersion -> [ChainId] -> SomeApi
someSpvApis v = mconcat . fmap (someSpvApi v)
