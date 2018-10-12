{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.ChainDB.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Collections of hashes or block headers are returned in an order that is
-- compatible with the order of the block heights of the respective block
-- headers. This allows guarantees that all dependencies of a block header are
-- returned before the block header itself is returned.
--
-- Block hashes and block headers are base64-URL encoded.
--
-- If a filter parameter is given the result is the intersection of the headers
-- that are defined by the filter and the headers from the rest of the query.
--
-- The branch parameter specifies a sequence of block headers such that each
-- header is the parent of the succeeding header in the sequence. The last
-- header of the sequence is the second parameter value. The first header in the
-- sequence is the largest header that is smaller or equal than both parameter
-- values.
--
-- All functions return 404 if provided with a block hash argument or parameter
-- that doesn't exist in the database.
--
-- There are default values for @minheight@ and page @limit@ applied to some
-- queries minrank. These can be discovered through
-- @GET chainweb/<ApiVersion>/<InstanceId>/api@
--
module Chainweb.ChainDB.RestAPI
(
-- * ChainDB with typelevel ChainId and ChainwebVersion parameters
  ChainDb_(..)
, SomeChainDb(..)
, someChainDbVal

-- * ChainDB API
, ChainDbApi
, chainDbApi

-- * Multichain APIs
, someChainDbApi
, someChainDbApis

-- * Sub APIs
, BranchesApi
, branchesApi
, HeaderApi
, headerApi
, HeaderPutApi
, headerPutApi
, HeadersApi
, headersApi
, HashesApi
, hashesApi
) where

import Control.Monad.Identity

import Data.Proxy

import Numeric.Natural

import Servant.API

-- internal modules
import Chainweb.ChainDB
import Chainweb.ChainDB.RestAPI.Orphans ()
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Type indexed ChainDb

newtype ChainDb_ (v :: ChainwebVersionT) (c :: ChainIdT) = ChainDb_ ChainDb

data SomeChainDb = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomeChainDb (ChainDb_ v c)

someChainDbVal :: ChainwebVersion -> ChainId -> ChainDb -> SomeChainDb
someChainDbVal v cid db = case someChainwebVersionVal v of
     (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
         (SomeChainIdT (Proxy :: Proxy cidt)) -> SomeChainDb (ChainDb_ @vt @cidt db)

-- -------------------------------------------------------------------------- --
-- Query Parameters

-- | Filter:
--
-- *   minheight :: Natural
-- *   maxheight :: Natural
-- *   branch :: BlockHash,BlockHash
--
type FilterParams = MinHeightParam :> MaxHeightParam :> BranchParam

type MinHeightParam = QueryParam "minheight" Natural
type MaxHeightParam = QueryParam "maxheight" Natural
type BranchParam = QueryParam "branch" (Key 'Unchecked, Key 'Unchecked)

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/branch@
--
type BranchesApi_
    = "branch"
    :> PageParams (Key 'Unchecked)
    :> MinHeightParam
    :> MaxHeightParam
    :> Get '[JSON] (Page (Key 'Unchecked) (Key 'Unchecked))

type BranchesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c BranchesApi_)

branchesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BranchesApi v c)
branchesApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/hash@
--
type HashesApi_
    = "hash"
    :> PageParams (Key 'Unchecked)
    :> FilterParams
    :> Get '[JSON] (Page (Key 'Unchecked) (Key 'Unchecked))

type HashesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c HashesApi_)

hashesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HashesApi v c)
hashesApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header@
--
type HeadersApi_
    = "header"
    :> PageParams (Key 'Unchecked)
    :> FilterParams
    :> Get '[JSON] (Page (Key 'Unchecked) (Entry 'Unchecked))

type HeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c HeadersApi_)

headersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeadersApi v c)
headersApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header/<BlockHash>@
--
type HeaderApi_
    = "header"
    :> Capture "BlockHash" (Key 'Unchecked)
    :> Get '[JSON] (Entry 'Unchecked)

type HeaderApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c HeaderApi_)

headerApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderApi v c)
headerApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @PUT /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header@
--
type HeaderPutApi_
    = "header"
    :> ReqBody '[JSON] (Entry 'Unchecked)
    :> PutNoContent '[JSON] NoContent

type HeaderPutApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = Reassoc (ChainEndpoint v c HeaderPutApi_)

headerPutApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderPutApi v c)
headerPutApi = Proxy

-- -------------------------------------------------------------------------- --
-- | ChainDb Api
--
type ChainDbApi v c
    = BranchesApi v c
    :<|> HashesApi v c
    :<|> HeadersApi v c
    :<|> HeaderApi v c
    :<|> HeaderPutApi v c

chainDbApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (ChainDbApi v c)
chainDbApi = Proxy

-- -------------------------------------------------------------------------- --
-- Multi Chain API

someChainDbApi :: ChainwebVersion -> ChainId -> SomeApi
someChainDbApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (chainDbApi @v' @c')

someChainDbApis :: ChainwebVersion -> [ChainId] -> SomeApi
someChainDbApis v = mconcat . fmap (someChainDbApi v)
