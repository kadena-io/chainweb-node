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
{-# LANGUAGE ImportQualifiedPost #-}

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
, BlockPage

-- * Encodings
, JsonBlockHeaderObject

-- * BlockHeaderDb with typelevel ChainId and ChainwebVersion parameters
, BlockHeaderDb_(..)
, SomeBlockHeaderDb(..)
, someBlockHeaderDbVal

-- * BlockHeaderDb API
, BlockHeaderDbApi
, P2pBlockHeaderDbApi

-- * BlockHeader Event Stream
, HeaderUpdate(..)
, BlockStreamApi

-- * Sub APIs
, BranchHashesApi
, branchHashesApi
, BranchHeadersApi
, branchHeadersApi
, P2pBranchHeadersApi
, p2pBranchHeadersApi
, HeaderApi
, P2pHeaderApi
, headerApi
, p2pHeaderApi
, HeadersApi
, P2pHeadersApi
, p2pHeadersApi
, headersApi
, HashesApi
, hashesApi
, BlocksApi
, BranchBlocksApi
) where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy qualified as L
import Data.Maybe
import Data.Proxy
import Data.Text (Text)

import Network.HTTP.Media ((//), (/:))

import Servant.API

-- internal modules
import Chainweb.Block
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization hiding (Get)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- API types

type BlockHashPage = Page (NextItem BlockHash) BlockHash

type BlockHeaderPage = Page (NextItem BlockHash) BlockHeader

-- because this endpoint is only used on the service API, we assume clients
-- want object-encoded block headers.
blockProperties :: KeyValue e kv => Block -> [kv]
blockProperties o =
    [ "header"  .= ObjectEncoded (_blockHeader o)
    , "payloadWithOutputs" .= _blockPayloadWithOutputs o
    ]

instance ToJSON Block where
    toJSON = object . blockProperties
    toEncoding = pairs . mconcat . blockProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Block where
    parseJSON = withObject "Block" $ \o -> Block
        <$> (_objectEncoded <$> o .: "header")
        <*> o .: "payloadWithOutputs"
    {-# INLINE parseJSON #-}

type BlockPage = Page (NextItem BlockHash) Block

-- -------------------------------------------------------------------------- --
-- Encodings

-- | Orphan instance to decode BlockHeaders from OctetStream
--
instance MimeUnrender OctetStream BlockHeader where
    mimeUnrender _ = runGetEitherL decodeBlockHeader
    {-# INLINE mimeUnrender #-}

-- | Orphan instance to encode BlockHeaders as OctetStream
--
instance MimeRender OctetStream BlockHeader where
    mimeRender _ = runPutL . encodeBlockHeader
    {-# INLINE mimeRender #-}

-- | Orphan instance to encode pages of blocks as JSON
instance MimeRender JSON BlockPage where
    mimeRender _ = encode
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

instance MimeRender OctetStream BlockPayload where
    mimeRender _ = L.fromStrict . encodeBlockPayloads
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream BlockPayload where
    mimeUnrender _ = first show . decodeBlockPayloads . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream BlockTransactions where
    mimeRender _ = L.fromStrict . encodeBlockTransactions
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream BlockTransactions where
    mimeUnrender _ = first show . decodeBlockTransactions . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream BlockOutputs where
    mimeRender _ = L.fromStrict . encodeBlockOutputs
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream BlockOutputs where
    mimeUnrender _ = first show . decodeBlockOutputs . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream TransactionTree where
    mimeRender _ = L.fromStrict . encodeTransactionTree
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream TransactionTree where
    mimeUnrender _ = first show . decodeTransactionTree . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream OutputTree where
    mimeRender _ = L.fromStrict . encodeOutputTree
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream OutputTree where
    mimeUnrender _ = first show . decodeOutputTree . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream PayloadWithOutputs where
    mimeRender _ = L.fromStrict . encodePayloadWithOutputs
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream PayloadWithOutputs where
    mimeUnrender _ = first show . decodePayloadWithOutputs . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream PayloadData where
    mimeRender _ = L.fromStrict . encodePayloadData
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream PayloadData where
    mimeUnrender _ = first show . decodePayloadData . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream PayloadDataList where
    mimeRender _ = L.fromStrict . encodePayloadDataList
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream PayloadDataList where
    mimeUnrender _ = first show . decodePayloadDataList . L.toStrict
    {-# INLINE mimeUnrender #-}

instance MimeRender OctetStream PayloadWithOutputsList where
    mimeRender _ = L.fromStrict . encodePayloadWithOutputsList
    {-# INLINE mimeRender #-}

instance MimeUnrender OctetStream PayloadWithOutputsList where
    mimeUnrender _ = first show . decodePayloadWithOutputsList . L.toStrict
    {-# INLINE mimeUnrender #-}

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

type P2pBranchHeadersApi_
    = "header" :> "branch"
    :> PageParams (NextItem BlockHash)
    :> MinHeightParam
    :> MaxHeightParam
    :> ReqBody '[JSON] (BranchBounds BlockHeaderDb)
    :> Post '[JSON] BlockHeaderPage

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
type P2pBranchHeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc P2pBranchHeadersApi_

branchHeadersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (BranchHeadersApi v c)
branchHeadersApi = Proxy

p2pBranchHeadersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (P2pBranchHeadersApi v c)
p2pBranchHeadersApi = Proxy

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

type P2pHeadersApi_
    = "header"
    :> PageParams (NextItem BlockHash)
    :> FilterParams
    :> Get '[JSON] BlockHeaderPage

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

type P2pHeadersApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc P2pHeadersApi_

headersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeadersApi v c)
headersApi = Proxy

p2pHeadersApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (P2pHeadersApi v c)
p2pHeadersApi = Proxy

-- -------------------------------------------------------------------------- --
type HeaderApi_
    = "header"
    :> Capture "BlockHash" BlockHash
    :> Get '[JSON, JsonBlockHeaderObject, OctetStream] BlockHeader

type P2pHeaderApi_
    = "header"
    :> Capture "BlockHash" BlockHash
    :> Get '[JSON, OctetStream] BlockHeader

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/header\/\<BlockHash\>@
--
-- Returns a single block headers for a given block hash.
--
type HeaderApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> HeaderApi_
type P2pHeaderApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> P2pHeaderApi_

headerApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (HeaderApi v c)
headerApi = Proxy

p2pHeaderApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (P2pHeaderApi v c)
p2pHeaderApi = Proxy

-- -------------------------------------------------------------------------- --
type BlocksApi_
    = "block"
    :> PageParams (NextItem BlockHash)
    :> FilterParams
    :> Get '[JSON] BlockPage

-- | @GET \/chainweb\/\<ApiVersion\>\/\<InstanceId\>\/chain\/\<ChainId\>\/block@
--
-- Returns blocks in the block header tree database in ascending order
-- with respect to the children relation.
--
-- Note that for blocks on different branches, the order isn't determined.
-- Therefore a block of higher block height can be returned before a block of
-- lower block height.
--
type BlocksApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc BlocksApi_

-- -------------------------------------------------------------------------- --
type BranchBlocksApi_
    = "block" :> "branch"
    :> PageParams (NextItem BlockHash)
    :> MinHeightParam
    :> MaxHeightParam
    :> ReqBody '[JSON] (BranchBounds BlockHeaderDb)
    :> Post '[JSON] BlockPage

type BranchBlocksApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v :> ChainEndpoint c :> Reassoc BranchBlocksApi_

-- -------------------------------------------------------------------------- --
-- | BlockHeaderDb Api
--
type BlockHeaderDbApi v c
    = HashesApi v c
    :<|> HeadersApi v c
    :<|> BlocksApi v c
    :<|> HeaderApi v c
    :<|> BranchHashesApi v c
    :<|> BranchHeadersApi v c
    :<|> BranchBlocksApi v c

-- | Restricted P2P BlockHeader DB API
--
type P2pBlockHeaderDbApi v c
    = P2pHeadersApi v c
    :<|> P2pHeaderApi v c
    :<|> P2pBranchHeadersApi v c

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

data HeaderUpdate = HeaderUpdate
    { _huHeader :: !(ObjectEncoded BlockHeader)
    , _huPayloadWithOutputs :: !(Maybe PayloadWithOutputs)
    , _huTxCount :: !Int
    , _huPowHash :: !Text
    , _huTarget :: !Text
    }
    deriving (Show, Eq)

headerUpdateProperties :: KeyValue e kv => HeaderUpdate -> [kv]
headerUpdateProperties o =
    [ "header"  .= _huHeader o
    , "txCount" .= _huTxCount o
    , "powHash" .= _huPowHash o
    , "target"  .= _huTarget o
    ] <> concatMap maybeToList
    [ ("payloadWithOutputs" .=) <$> _huPayloadWithOutputs o
    ]
{-# INLINE headerUpdateProperties #-}

instance ToJSON HeaderUpdate where
    toJSON = object . headerUpdateProperties
    toEncoding = pairs . mconcat . headerUpdateProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON HeaderUpdate where
    parseJSON = withObject "HeaderUpdate" $ \o -> HeaderUpdate
        <$> o .: "header"
        <*> o .:? "payloadWithOutputs"
        <*> o .: "txCount"
        <*> o .: "powHash"
        <*> o .: "target"
    {-# INLINE parseJSON #-}

type BlockStreamApi_ =
    "block" :> "updates" :> Raw :<|>
    "header" :> "updates" :> Raw

-- | A stream of all new blocks that are accepted into the true `Cut`.
--
type BlockStreamApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> BlockStreamApi_
