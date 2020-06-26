{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Payload.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The Block Payload REST API
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

-- * Payload GET API
, OutputsGetApi
, outputsGetApi

-- * Payload API
, PayloadApi
, payloadApi

-- * Some Payload API
, somePayloadApi
, somePayloadApis
) where

import Control.Monad.Identity

import Data.Proxy

import Servant.API

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
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
    return $! SomePayloadDb (PayloadDb_ @cas @vt @cidt db)

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
-- Outputs GET API

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/\<BlockPayloadHash\>/outputs@

type OutputsGetApi_
    = "payload"
    :> Capture "BlockPayloadHash" BlockPayloadHash
    :> "outputs"
    :> Get '[JSON] PayloadWithOutputs

type OutputsGetApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> OutputsGetApi_

outputsGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (OutputsGetApi v c)
outputsGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload API

type PayloadApi v c = PayloadGetApi v c :<|> OutputsGetApi v c

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
    return $! SomeApi (payloadApi @v' @c')

somePayloadApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePayloadApis v = mconcat . fmap (somePayloadApi v)
