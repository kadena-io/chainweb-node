{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
-- * API types
  BlockHashPage
, BlockHeaderPage

-- * Encodings
, JsonBlockHeaderObject

-- * BlockHeaderDb with typelevel ChainId and ChainwebVersion parameters
, BlockHeaderDb_(..)
, SomeBlockHeaderDb(..)
, someBlockHeaderDbVal

-- * BlockHeaderDb API
, BlockHeaderDbApi
, blockHeaderDbApi

-- * Multichain APIs
, someBlockHeaderDbApi
, someBlockHeaderDbApis

-- * BlockHeader Event Stream
, HeaderStream(..)
, HeaderUpdate(..)
, HeaderStreamApi
, headerStreamApi
, someHeaderStreamApi

-- * Sub APIs
, BranchHashesApi
, branchHashesApi
, BranchHeadersApi
, branchHeadersApi
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

import Data.Aeson
import Data.Bifunctor
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
import Data.Text (Text)

import Network.HTTP.Media ((//), (/:))

import Servant.API

-- internal modules
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging hiding (properties)
import Chainweb.Version

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- API types

-- | A page of BlockHashes
--
type BlockHashPage = Page (NextItem BlockHash) BlockHash

-- | A page of BlockHeaders
--
type BlockHeaderPage = Page (NextItem BlockHash) BlockHeader

-- -------------------------------------------------------------------------- --
-- Encodings

-- | Orphan instance to encode BlockHeaders as OctetStream
--
instance MimeUnrender OctetStream BlockHeader where
    mimeUnrender _ = runGetS decodeBlockHeader . BL.toStrict
    {-# INLINE mimeUnrender #-}

-- | Orphan instance to encode BlockHeaders as OctetStream
--
instance MimeRender OctetStream BlockHeader where
    mimeRender _ = runPutL . encodeBlockHeader
    {-# INLINE mimeRender #-}

-- | The default JSON instance of BlockHeader is an unpadded base64Url encoding of
-- the block header bytes. There a newtype wrapper that provides an alternative
-- encoding with as an JSON object.
--
-- The mime type @application/json;blockheader-encoding=object@ is used in
-- the HTTP @accept@ header and HTTP @content-type header@ to indicate that
-- block headers are encoded as JSON objects
--
data JsonBlockHeaderObject

instance Accept JsonBlockHeaderObject where
    contentType _ = "application" // "json" /: ("blockheader-encoding", "object")

instance MimeUnrender JsonBlockHeaderObject BlockHeader where
    mimeUnrender _ = second _objectEncoded . eitherDecode
    {-# INLINE mimeUnrender #-}

instance MimeRender JsonBlockHeaderObject BlockHeader where
    mimeRender _ = encode . ObjectEncoded
    {-# INLINE mimeRender #-}

instance MimeUnrender JsonBlockHeaderObject BlockHeaderPage where
    mimeUnrender _ = second (fmap _objectEncoded) . eitherDecode
    {-# INLINE mimeUnrender #-}

instance MimeRender JsonBlockHeaderObject BlockHeaderPage where
    mimeRender _ = encode . fmap ObjectEncoded
    {-# INLINE mimeRender #-}

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
type BranchHashesApi_
    = "hash" :> "branch"
    :> PageParams (NextItem BlockHash)
    :> MinHeightParam
    :> MaxHeightParam
    :> ReqBody '[JSON] (BranchBounds BlockHeaderDb)
    :> Post '[JSON] BlockHashPage

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/hash\/branch@
--
-- Returns a set of branches. A branch is obtained by traversing the block
-- header tree database starting at some entry along the parent relation, i.e.
-- in direction towards the root or decending order with respect to the children
-- relation.
--
-- If a lower bound for the traversal is given, then no node is returned that is in a
-- branch (is equal to, parent, or grantparent) of the lower bound. This means
-- the query stops at the fork point of the upper bound and the lower bound and
-- returns all nodes between from the upper bound down to fork point. The fork
-- point itself isn't included.
--
-- Simultaneously traversing more than a single branch results in a tree that
-- is a sub-graph, but not necessarily a sub-tree, of the database tree. Search
-- stops at the first matching lower bound.
--
type BranchHashesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc BranchHashesApi_

branchHashesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BranchHashesApi v c)
branchHashesApi = Proxy

-- -------------------------------------------------------------------------- --
type BranchHeadersApi_
    = "header" :> "branch"
    :> PageParams (NextItem BlockHash)
    :> MinHeightParam
    :> MaxHeightParam
    :> ReqBody '[JSON] (BranchBounds BlockHeaderDb)
    :> Post '[JSON, JsonBlockHeaderObject] BlockHeaderPage

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/header\/branch@
--
-- Returns a set of branches. A branch is obtained by traversing the block
-- header tree database starting at some entry along the parent relation, i.e.
-- in direction towards the root or decending order with respect to the children
-- relation.
--
-- If a lower bound for the traversal is given, then no node is returned that is in a
-- branch (is equal to, parent, or grantparent) of the lower bound. This means
-- the query stops at the fork point of the upper bound and the lower bound and
-- returns all nodes between from the upper bound down to fork point. The fork
-- point itself isn't included.
--
-- Simultaneously traversing more than a single branch results in a tree that
-- is a sub-graph, but not necessarily a sub-tree, of the database tree. Search
-- stops at the first matching lower bound.
--
type BranchHeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc BranchHeadersApi_

branchHeadersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BranchHeadersApi v c)
branchHeadersApi = Proxy

-- -------------------------------------------------------------------------- --
type HashesApi_
    = "hash"
    :> PageParams (NextItem BlockHash)
    :> FilterParams
    :> Get '[JSON] BlockHashPage

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/hash@
--
-- Returns hashes in the block header tree database in ascending order with
-- respect to the children relation.
--
-- Note that for block hashes on different branches, the order isn't determined.
-- Therefore a block hash of higher block height can be returned before a block
-- hash of lower block height.
--
type HashesApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc HashesApi_

hashesApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HashesApi v c)
hashesApi = Proxy

-- -------------------------------------------------------------------------- --
type HeadersApi_
    = "header"
    :> PageParams (NextItem BlockHash)
    :> FilterParams
    :> Get '[JSON, JsonBlockHeaderObject] BlockHeaderPage

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/header@
--
-- Returns block headers in the block header tree database in ascending order
-- with respect to the children relation.
--
-- Note that for block headers on different branches, the order isn't determined.
-- Therefore a block header of higher block height can be returned before a block
-- header of lower block height.
--
type HeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc HeadersApi_

headersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeadersApi v c)
headersApi = Proxy

-- -------------------------------------------------------------------------- --
type HeaderApi_
    = "header"
    :> Capture "BlockHash" BlockHash
    :> Get '[JSON, JsonBlockHeaderObject, OctetStream] BlockHeader

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/header\/\<BlockHash\>@
--
-- Returns a single block headers for a given block hash.
--
type HeaderApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> HeaderApi_

headerApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderApi v c)
headerApi = Proxy

-- -------------------------------------------------------------------------- --
type HeaderPutApi_
    = "header"
    :> ReqBody '[JSON, JsonBlockHeaderObject, OctetStream] BlockHeader
    :> Verb 'PUT 204 '[JSON] NoContent

-- | @PUT \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/header@
--
-- Adds a block header to the block header tree database. Returns a failure with
-- status code 400 if the block header can't be addded because of a validation
-- failure or missing dependencies.
--
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
    = HashesApi v c
    :<|> HeadersApi v c
    :<|> HeaderApi v c
    :<|> HeaderPutApi v c
    :<|> BranchHashesApi v c
    :<|> BranchHeadersApi v c

blockHeaderDbApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BlockHeaderDbApi v c)
blockHeaderDbApi = Proxy

-- -------------------------------------------------------------------------- --
-- Multi Chain API

-- TODO Just use @case@ statements.
someBlockHeaderDbApi :: ChainwebVersion -> ChainId -> SomeApi
someBlockHeaderDbApi v c = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v') <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c') <- return $ someChainIdVal c
    return $ SomeApi (blockHeaderDbApi @v' @c')

someBlockHeaderDbApis :: ChainwebVersion -> [ChainId] -> SomeApi
someBlockHeaderDbApis v = mconcat . fmap (someBlockHeaderDbApi v)

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

newtype HeaderStream = HeaderStream Bool

data HeaderUpdate = HeaderUpdate
    { _huHeader :: !(ObjectEncoded BlockHeader)
    , _huTxCount :: !Int
    , _huPowHash :: !Text
    , _huTarget :: !Text }

instance ToJSON HeaderUpdate where
    toJSON o = object
        [ "header"  .= _huHeader o
        , "txCount" .= _huTxCount o
        , "powHash" .= _huPowHash o
        , "target"  .= _huTarget o ]

type HeaderStreamApi_ = "header" :> "updates" :> Raw

-- | A stream of all new `BlockHeader`s that are accepted into the true `Cut`.
--
type HeaderStreamApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> HeaderStreamApi_

headerStreamApi :: forall (v :: ChainwebVersionT). Proxy (HeaderStreamApi v)
headerStreamApi = Proxy

someHeaderStreamApi :: ChainwebVersion -> SomeApi
someHeaderStreamApi (FromSing (SChainwebVersion :: Sing v)) = SomeApi $ headerStreamApi @v
