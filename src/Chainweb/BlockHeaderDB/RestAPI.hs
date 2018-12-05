{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.BlockHeaderDB.RestAPI
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
-- @GET chainweb\/`ApiVersion`\/InstanceId\/api@
--
module Chainweb.BlockHeaderDB.RestAPI
(
-- * BlockHeaderDb with typelevel ChainId and ChainwebVersion parameters
  BlockHeaderDb_(..)
, SomeBlockHeaderDb(..)
, someBlockHeaderDbVal

-- * BlockHeaderDb API
, BlockHeaderDbApi
, blockHeaderDbApi

-- * Multichain APIs
, someBlockHeaderDbApi
, someBlockHeaderDbApis

-- * Sub APIs
, LeavesApi
, leavesApi
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

import Servant.API

-- internal modules
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging hiding (properties)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Type indexed BlockHeaderDb

newtype BlockHeaderDb_ (v :: ChainwebVersionT) (c :: ChainIdT) = BlockHeaderDb_ BlockHeaderDb

data SomeBlockHeaderDb = forall v c
    . (KnownChainwebVersionSymbol v, KnownChainIdSymbol c)
    => SomeBlockHeaderDb (BlockHeaderDb_ v c)

someBlockHeaderDbVal :: ChainwebVersion -> ChainId -> BlockHeaderDb -> SomeBlockHeaderDb
someBlockHeaderDbVal v cid db = case someChainwebVersionVal v of
     (SomeChainwebVersionT (Proxy :: Proxy vt)) -> case someChainIdVal cid of
         (SomeChainIdT (Proxy :: Proxy cidt)) -> SomeBlockHeaderDb (BlockHeaderDb_ @vt @cidt db)

-- -------------------------------------------------------------------------- --
-- Query Parameters

-- | Filter:
--
-- *   minheight :: Natural
-- *   maxheight :: Natural
--
type FilterParams = MinHeightParam :> MaxHeightParam

type MinHeightParam = QueryParam "minheight" MinRank
type MaxHeightParam = QueryParam "maxheight" MaxRank

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/branch@
--
-- Returns a set of branches. A branch is obtained by traversing the block
-- header tree database starting at some entry along the parent relation, i.e.
-- in direction towards the root or decending order with respect to the children
-- relation.
--
-- If lower bound for the traversal is given no node is returned that is in a
-- branch (is equal to, parent, or grantparent) of the lower bound. This means
-- the query stops a the fork point of the upper bound and the lower bound and
-- returns all nodes between from the upper bound down to fork point. The fork
-- point itself isn't included.
--
-- Simultaneously traversing more than a single branch results in a a tree that
-- is a sub-graph, but not necessarily a sub-tree, of the data base tree. Search
-- stops at the first matching lower bound.
--
type BranchesApi_
    = "branch"
    :> PageParams (NextItem (DbKey BlockHeaderDb))
    :> MinHeightParam
    :> MaxHeightParam
    :> ReqBody '[JSON] (BranchBounds BlockHeaderDb)
    :> Post '[JSON] (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))

type BranchesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc BranchesApi_

branchesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BranchesApi v c)
branchesApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/leave@
--
-- Returns the hashes of the entries of the block header tree database. Querying
-- the database isn't atomic entries may be added concurrently. Therefor the
-- result of this query is a set of block hashes that represent a possible set
-- of leaves at some point in the history of the database. The server is
-- expected to try to return a large and recent set.
--
type LeavesApi_
    = "leave"
    :> PageParams (NextItem (DbKey BlockHeaderDb))
    :> MinHeightParam
    :> MaxHeightParam
    :> Get '[JSON] (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))

type LeavesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc LeavesApi_

leavesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (LeavesApi v c)
leavesApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/hash@
--
-- Returns hashes in the block header tree database in ascending order with
-- respect the children relation.
--
-- Note, for block hashes on different branches the order isn't determined.
-- Therefor a block hash of higher block height can be returned before a block
-- hash of lower block height.
--
type HashesApi_
    = "hash"
    :> PageParams (NextItem (DbKey BlockHeaderDb))
    :> FilterParams
    :> Get '[JSON] (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))

type HashesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc HashesApi_

hashesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HashesApi v c)
hashesApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header@
--
-- Returns block headers in the block header tree database in ascending order
-- with respect to the children relation.
--
-- Note, for block headers on different branches the order isn't determined.
-- Therefor a block header of higher block height can be returned before a block
-- header of lower block height.
--
type HeadersApi_
    = "header"
    :> PageParams (NextItem (DbKey BlockHeaderDb))
    :> FilterParams
    :> Get '[JSON] (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))

type HeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc HeadersApi_

headersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeadersApi v c)
headersApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @GET /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header/<BlockHash>@
--
-- Returns a single block headers for a given block hash.
--
type HeaderApi_
    = "header"
    :> Capture "BlockHash" (DbKey BlockHeaderDb)
    :> Get '[JSON] (DbEntry BlockHeaderDb)

type HeaderApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> HeaderApi_

headerApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderApi v c)
headerApi = Proxy

-- -------------------------------------------------------------------------- --
-- | @PUT /chainweb/<ApiVersion>/<InstanceId>/chain/<ChainId>/header@
--
-- Adds a block header to the block header tree database. Returns a failure with
-- status code 400 if the block header can't be addded because of a validation
-- failure or missing dependencies.
--
type HeaderPutApi_
    = "header"
    :> ReqBody '[JSON] (DbEntry BlockHeaderDb)
    :> PutNoContent '[JSON] NoContent

type HeaderPutApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> HeaderPutApi_

headerPutApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderPutApi v c)
headerPutApi = Proxy

-- -------------------------------------------------------------------------- --
-- | BlockHeaderDb Api
--
type BlockHeaderDbApi v c
    = LeavesApi v c
    :<|> HashesApi v c
    :<|> HeadersApi v c
    :<|> HeaderApi v c
    :<|> HeaderPutApi v c
    :<|> BranchesApi v c

blockHeaderDbApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BlockHeaderDbApi v c)
blockHeaderDbApi = Proxy

-- -------------------------------------------------------------------------- --
-- Multi Chain API

someBlockHeaderDbApi :: ChainwebVersion -> ChainId -> SomeApi
someBlockHeaderDbApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (blockHeaderDbApi @v' @c')

someBlockHeaderDbApis :: ChainwebVersion -> [ChainId] -> SomeApi
someBlockHeaderDbApis v = mconcat . fmap (someBlockHeaderDbApi v)
