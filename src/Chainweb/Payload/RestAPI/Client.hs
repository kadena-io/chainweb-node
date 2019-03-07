{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Payload.RestAPI.Client
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Payload.RestAPI.Client
( payloadClient
, spvGetTransactionProofClient
, spvGetTransactionOutputProofClient
) where

import Control.Monad.Identity

import Crypto.Hash.Algorithms

import Data.Proxy

import Numeric.Natural

import Servant.Client

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.RestAPI
import Chainweb.Payload.SPV
import Chainweb.RestAPI.Orphans ()
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- GET Header Client

payloadClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockPayloadHash
    -> ClientM PayloadData
payloadClient_ = client (payloadGetApi @v @c)

payloadClient
    :: ChainwebVersion
    -> ChainId
    -> BlockPayloadHash
    -> ClientM PayloadData
payloadClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ payloadClient_ @v @c k

-- -------------------------------------------------------------------------- --
-- SPV Transaction Proof Client

spvGetTransactionProofClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ChainId
    -> BlockHeight
    -> Natural
    -> ClientM (TransactionProof SHA512t_256)
spvGetTransactionProofClient_ = client (spvGetTransactionProofApi @v @c)

spvGetTransactionProofClient
    :: ChainwebVersion
    -> ChainId
    -> ChainId
    -> BlockHeight
    -> Natural
    -> ClientM (TransactionProof SHA512t_256)
spvGetTransactionProofClient v tcid scid h i = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal tcid
    return $ spvGetTransactionProofClient_ @v @c scid h i

-- -------------------------------------------------------------------------- --
-- SPV Transaction Output Proof Client

spvGetTransactionOutputProofClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => ChainId
    -> BlockHeight
    -> Natural
    -> ClientM (TransactionOutputProof SHA512t_256)
spvGetTransactionOutputProofClient_ = client (spvGetTransactionOutputProofApi @v @c)

spvGetTransactionOutputProofClient
    :: ChainwebVersion
    -> ChainId
    -> ChainId
    -> BlockHeight
    -> Natural
    -> ClientM (TransactionOutputProof SHA512t_256)
spvGetTransactionOutputProofClient v tcid scid h i = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal tcid
    return $ spvGetTransactionOutputProofClient_ @v @c scid h i

