{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
, BatchBody(..)
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
import qualified Data.Vector as V

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
import Chainweb.BlockHeight
import Chainweb.BlockHeaderDB.RestAPI ()

-- -------------------------------------------------------------------------- --
-- Constants

-- | The maximum number of items that are returned in a batch
--
newtype PayloadBatchLimit = PayloadBatchLimit Natural
    deriving (Show, Eq)
    deriving newtype (Ord, Enum, Num, Real, Integral, ToJSON, FromJSON)

p2pPayloadBatchLimit :: PayloadBatchLimit
p2pPayloadBatchLimit = 0

defaultServicePayloadBatchLimit :: PayloadBatchLimit
defaultServicePayloadBatchLimit = 1000

-- -------------------------------------------------------------------------- --
-- Type indexed PayloadDb

newtype PayloadDb' tbl (v :: ChainwebVersionT) (c :: ChainIdT) = PayloadDb' (PayloadDb tbl)

data SomePayloadDb tbl = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomePayloadDb (PayloadDb' tbl v c)

somePayloadDbVal :: forall tbl . ChainwebVersion -> ChainId -> PayloadDb tbl -> SomePayloadDb tbl
somePayloadDbVal v cid db = runIdentity $ do
    SomeChainwebVersionT (Proxy :: Proxy vt) <- return $ someChainwebVersionVal v
    SomeChainIdT (Proxy :: Proxy cidt) <- return $ someChainIdVal cid
    return $! SomePayloadDb (PayloadDb' @tbl @vt @cidt db)

-- -------------------------------------------------------------------------- --
-- Payload GET API

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/\<BlockPayloadHash\>@

type PayloadGetApi_
    = "payload"
    :> Capture "BlockPayloadHash" BlockPayloadHash
    :> QueryParam "height" BlockHeight
    :> Get '[JSON, OctetStream] PayloadData

type PayloadGetApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> PayloadGetApi_

payloadGetApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PayloadGetApi v c)
payloadGetApi = Proxy

-- -------------------------------------------------------------------------- --
-- Payload POST API

-- | The body of a batch payload request, which can be either a list of hashes
-- or a list of hashes with their corresponding heights.
data BatchBody
    = WithoutHeights [BlockPayloadHash]
    | WithHeights [(BlockHeight, BlockPayloadHash)]

instance FromJSON BatchBody where
    -- first, for backwards compat, parse the WithoutHeight variant
    -- as a raw Array. this is for backwards compatibility; the original
    -- /payload/batch endpoint only accepted an array of hashes without heights,
    -- so we need to support that.
    parseJSON (Array x) = WithoutHeights <$> traverse parseJSON (V.toList x)
    -- then, try to parse the WithHeight variant as an Object like so:
    --    { "heights": [ height1, height2, ... ], "hashes": [ "hash1", "hash2", ... ] }
    parseJSON (Object o) = WithHeights <$> (zip <$> o .: "heights" <*> o .: "hashes")
    -- anything else is invalid
    parseJSON _ = fail "Invalid payload batch body"

instance ToJSON BatchBody where
    toJSON (WithoutHeights xs) = toJSON xs
    toJSON (WithHeights xs) = object
        [ "heights" .= map fst xs
        , "hashes" .= map snd xs
        ]

-- | @POST \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/payload\/batch@
--
-- The query may return any number (including none) of the requested payload
-- data. Results are returned in any order.
--
type PayloadPostApi_
    = "payload"
    :> "batch"
    :> ReqBody '[JSON] BatchBody
    :> Post '[JSON, OctetStream] PayloadDataList

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
    :> QueryParam "height" BlockHeight
    :> Get '[JSON, OctetStream] PayloadWithOutputs

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
    :> ReqBody '[JSON] BatchBody
    :> Post '[JSON, OctetStream] PayloadWithOutputsList

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
