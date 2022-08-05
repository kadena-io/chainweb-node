{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- * Batch size limits
  PayloadBatchLimit(..)
, p2pPayloadBatchLimit
, defaultServicePayloadBatchLimit

-- * Type indexed PayloadDb
, PayloadDb'(..)
, SomePayloadDb(..)
, somePayloadDbVal

-- * Payload GET API
, PayloadGetApi
, payloadGetApi

-- * Payload batch POST API
, PayloadPostApi
, payloadPostApi

-- * Outputs GET API
, OutputsGetApi
, outputsGetApi

-- * Outputs batch POST API
, OutputsPostApi
, outputsPostApi

-- * Payload API
, PayloadApi
, payloadApi

-- * Some Payload API
, somePayloadApi
, somePayloadApis
) where

import Control.Monad.Identity

import Data.Aeson
import Data.Proxy

import Numeric.Natural

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
-- Constants

-- | The maximum number of items that are returned in a batch
--
newtype PayloadBatchLimit = PayloadBatchLimit Natural
    deriving (Show, Eq)
    deriving newtype (Ord, Enum, Num, Real, Integral, ToJSON, FromJSON)

p2pPayloadBatchLimit :: PayloadBatchLimit
p2pPayloadBatchLimit = 50

defaultServicePayloadBatchLimit :: PayloadBatchLimit
defaultServicePayloadBatchLimit = 1000

-- -------------------------------------------------------------------------- --
-- Type indexed PayloadDb

newtype PayloadDb' cas (v :: ChainwebVersionT) (c :: ChainIdT) = PayloadDb' (PayloadDb cas)

data SomePayloadDb cas = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomePayloadDb (PayloadDb' cas v c)

somePayloadDbVal :: forall cas . ChainwebVersion -> ChainId -> PayloadDb cas -> SomePayloadDb cas
somePayloadDbVal v cid db = runIdentity $ do
    SomeChainwebVersionT (Proxy :: Proxy vt) <- return $ someChainwebVersionVal v
    SomeChainIdT (Proxy :: Proxy cidt) <- return $ someChainIdVal cid
    return $! SomePayloadDb (PayloadDb' @cas @vt @cidt db)

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
-- Payload POST API

-- | @POST \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/batch@
--
-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
type PayloadPostApi_
    = "payload"
    :> "batch"
    :> ReqBody '[JSON] [BlockPayloadHash]
    :> Post '[JSON] [PayloadData]

type PayloadPostApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PayloadPostApi_

payloadPostApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PayloadPostApi v c)
payloadPostApi = Proxy

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
-- Outputs POST API

-- | @POST \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/outputs\/batch@
--
-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
type OutputsPostApi_
    = "payload"
    :> "outputs"
    :> "batch"
    :> ReqBody '[JSON] [BlockPayloadHash]
    :> Post '[JSON] [PayloadWithOutputs]

type OutputsPostApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> OutputsPostApi_

outputsPostApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (OutputsPostApi v c)
outputsPostApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload API

type PayloadApi v c
    = PayloadGetApi v c
    :<|> OutputsGetApi v c
    :<|> PayloadPostApi v c
    :<|> OutputsPostApi v c

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
