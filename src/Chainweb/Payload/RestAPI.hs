{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Payload.RestAPI
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload.RestAPI
(
-- * Type indexed PayloadDb
  PayloadDb_(..)
, SomePayloadDb(..)
, somePayloadDbVal

-- * Payload GET API
, PayloadGetApi
, payloadGetApi

-- * Transaction Proof API
, SpvGetTransactionProofApi
, spvGetTransactionProofApi

-- * Transaction Output Proof API
, SpvGetTransactionOutputProofApi
, spvGetTransactionOutputProofApi

-- * Payload API
, PayloadApi
, payloadApi

-- * Some Payload API
, somePayloadApi
, somePayloadApis
) where

import Control.Monad.Identity

import Crypto.Hash.Algorithms

import Data.Proxy

import Numeric.Natural

import Servant.API

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.SPV
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Type indexed PayloadDb

newtype PayloadDb_ cas (v :: ChainwebVersionT) (c :: ChainIdT) = PayloadDb_ (PayloadDb cas)

data SomePayloadDb cas = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomePayloadDb (PayloadDb_ cas v c)

somePayloadDbVal :: forall cas . ChainwebVersion -> ChainId -> PayloadDb cas -> SomePayloadDb cas
somePayloadDbVal v cid db = runIdentity $ do
    SomeChainwebVersionT (Proxy :: Proxy vt) <- return $ someChainwebVersionVal v
    SomeChainIdT (Proxy :: Proxy cidt) <- return $ someChainIdVal cid
    return $ SomePayloadDb (PayloadDb_ @cas @vt @cidt db)

-- -------------------------------------------------------------------------- --
-- Payload GET API

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/\<BlockPayloadHash\>@

type PayloadGetApi_
    = "payload"
    :> Capture "BlockPayloadHash" BlockPayloadHash
    :> Get '[JSON] PayloadData

type PayloadGetApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PayloadGetApi_

payloadGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PayloadGetApi v c)
payloadGetApi = Proxy

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
-- Payload API

type PayloadApi v c = PayloadGetApi v c
    :<|> SpvGetTransactionProofApi v c
    :<|> SpvGetTransactionOutputProofApi v c

payloadApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PayloadApi v c)
payloadApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Payload API

somePayloadApi :: ChainwebVersion -> ChainId -> SomeApi
somePayloadApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (payloadApi @v' @c')

somePayloadApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePayloadApis v = mconcat . fmap (somePayloadApi v)

