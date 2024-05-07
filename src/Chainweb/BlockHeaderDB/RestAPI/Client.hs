{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHeaderDB.RestAPI.Client
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockHeaderDB.RestAPI.Client
( headerClient
, getHeaderBinary
, getHeaderJSON
, getHeaderJSONPretty

, hashesClient
, headersClient
, getHashesJSON
, getHeadersJSON
, getBlocksJSON
, getBranchHashes
, getBranchHeadersJSON
, getBranchHeadersJSONPretty

, branchHashesClient_
, branchHashesClient

, branchHeadersClient_
, branchHeadersClient
, branchHeadersClientContentType_
, branchHeadersClientJson
, branchHeadersClientJsonPretty
, branchBlocksClient
, headersClientJsonPretty
, blocksClient
, headersClient_
, headerClientJson
, headerClientJsonPretty
, headerClient_
) where

import Control.Lens

import Data.Aeson(encode, AesonException)
import Data.Kind
import Data.Proxy
import Data.Singletons

import Servant.API
import Servant.Client (ClientM, client)

import Network.HTTP.Client(RequestBody(..), responseBody)
import Network.HTTP.Media
import Network.HTTP.Types

import Web.DeepRoute.Client

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization
import Chainweb.Version
import qualified Network.HTTP.Client as Client
import Control.Exception (evaluate)
import qualified Data.Text as T
import Chainweb.Utils (EncodingException(..))

-- -------------------------------------------------------------------------- --
-- GET Header Client

headerClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbKey BlockHeaderDb
    -> ClientM BlockHeader
headerClient_ = headerClientContentType_ @v @c @OctetStream

headerClient
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM BlockHeader
headerClient = headerClientJsonBinary

headerClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) x
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Servant.API.Accept ct
    => SupportedRespBodyContentType ct x BlockHeader
    => (HeaderApi v c) ~ x
    => BlockHash
    -> ClientM BlockHeader
headerClientContentType_ = client (Proxy @(SetRespBodyContentType ct x))

headerClientJson
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM BlockHeader
headerClientJson v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JSON k

headerClientJsonPretty
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonPretty v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JsonBlockHeaderObject k

headerClientJsonBinary
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonBinary v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @OctetStream k

getHeaderBinary
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ApiRequest (Either EncodingException BlockHeader)
getHeaderBinary v cid bh = mkApiRequest
    methodGet
    (evaluate . fmap (over _Left (DecodeException . T.pack) . runGetEitherL decodeBlockHeader))
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@@ bh)
    & requestAcceptable ?~ [maxQuality $ "application" // "octet-stream"]

getHeaderJSON
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ApiRequest (Either AesonException BlockHeader)
getHeaderJSON v cid bh = mkApiRequest
    methodGet
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@@ bh)
    & requestAcceptable ?~ [maxQuality "application/json"]

getHeaderJSONPretty
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ApiRequest (Either AesonException BlockHeader)
getHeaderJSONPretty v cid bh = mkApiRequest
    methodGet
    (traverse ((fmap . fmap) _objectEncoded . jsonBody))
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@@ bh)
    & requestAcceptable ?~ [maxQuality "application/json;blockheader-encoding=object"]

-- -------------------------------------------------------------------------- --
-- Headers Client

headersClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHeaderPage
headersClient_ = headersClientContentType_ @v @c @JSON

headersClient
    :: ChainwebVersion
        -- ^ The remote chainweb that you wish to query from.
    -> ChainId
        -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Limit
        -- ^ The number of responses per-`Page` to return.
    -> Maybe (NextItem BlockHash)
        -- ^ The first header you want to see within the `Page`.
        -- `Page` contains a field `_pageNext`, which can be used
        -- to produce the value needed for subsequent calls.
    -> Maybe MinRank
        -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe MaxRank
        -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> ClientM BlockHeaderPage
headersClient = headersClientJson

headersClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) x
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Servant.API.Accept ct
    => HeadersApi v c ~ x
    => SupportedRespBodyContentType ct x BlockHeaderPage
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHeaderPage
headersClientContentType_ = client $ Proxy @(SetRespBodyContentType ct (HeadersApi v c))

headersClientJson
    :: ChainwebVersion
        -- ^ The remote chainweb that you wish to query from.
    -> ChainId
        -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Limit
        -- ^ The number of responses per-`Page` to return.
    -> Maybe (NextItem BlockHash)
        -- ^ The first header you want to see within the `Page`.
        -- `Page` contains a field `_pageNext`, which can be used
        -- to produce the value needed for subsequent calls.
    -> Maybe MinRank
        -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe MaxRank
        -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> ClientM BlockHeaderPage
headersClientJson v c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JSON limit start minr maxr

getHeadersJSON
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ApiRequest (Either AesonException BlockHeaderPage)
getHeadersJSON v cid limit start minr maxr = mkApiRequest
    methodGet
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header")
    & requestAcceptable ?~ [maxQuality "application/json"]
    & includePageParams limit start
    & includeFilterParams minr maxr

getBlocksJSON
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ApiRequest (Either AesonException BlockPage)
getBlocksJSON v cid limit start minr maxr = mkApiRequest
    methodGet
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "block")
    & requestAcceptable ?~ [maxQuality "application/json"]
    & includePageParams limit start
    & includeFilterParams minr maxr

getHashesJSON
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ApiRequest (Either AesonException BlockHashPage)
getHashesJSON v cid limit start minr maxr = mkApiRequest
    methodGet
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "hash")
    & requestAcceptable ?~ [maxQuality "application/json"]
    & includePageParams limit start
    & includeFilterParams minr maxr

getBranchHeadersJSON
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ApiRequest (Either AesonException BlockHeaderPage)
getBranchHeadersJSON v cid limit start minr maxr bb = mkApiRequest
    methodPost
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@ "branch")
    & requestAcceptable ?~ [maxQuality "application/json"]
    & includePageParams limit start
    & includeFilterParams minr maxr
    & requestBody .~ RequestBodyLBS (encode bb)

getBranchHeadersJSONPretty
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ApiRequest (Either AesonException BlockHeaderPage)
getBranchHeadersJSONPretty v cid limit start minr maxr bb = mkApiRequest
    methodPost
    (traverse (fmap (fmap (fmap _objectEncoded)) . jsonBody))
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@ "branch")
    & requestAcceptable ?~ [maxQuality "application/json;blockheader-encoding=object"]
    & includePageParams limit start
    & includeFilterParams minr maxr
    & requestBody .~ RequestBodyLBS (encode bb)

getBranchHashes
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ApiRequest (Either AesonException BlockHashPage)
getBranchHashes v cid limit start minr maxr bb = mkApiRequest
    methodPost
    (traverse jsonBody)
    ("chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "hash" /@ "branch")
    & requestAcceptable ?~ [maxQuality "application/json"]
    & includePageParams limit start
    & includeFilterParams minr maxr
    & requestBody .~ RequestBodyLBS (encode bb)

headersClientJsonPretty
    :: ChainwebVersion
        -- ^ The remote chainweb that you wish to query from.
    -> ChainId
        -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Limit
        -- ^ The number of responses per-`Page` to return.
    -> Maybe (NextItem BlockHash)
        -- ^ The first header you want to see within the `Page`.
        -- `Page` contains a field `_pageNext`, which can be used
        -- to produce the value needed for subsequent calls.
    -> Maybe MinRank
        -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe MaxRank
        -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> ClientM BlockHeaderPage
headersClientJsonPretty (FromSingChainwebVersion (SChainwebVersion :: Sing v)) c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JsonBlockHeaderObject limit start minr maxr

blocksClient
    :: ChainwebVersion
        -- ^ The remote chainweb that you wish to query from.
    -> ChainId
        -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Limit
        -- ^ The number of responses per-`Page` to return.
    -> Maybe (NextItem BlockHash)
        -- ^ The first header you want to see within the `Page`.
        -- `Page` contains a field `_pageNext`, which can be used
        -- to produce the value needed for subsequent calls.
    -> Maybe MinRank
        -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe MaxRank
        -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> ClientM BlockPage
blocksClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client (Proxy @(SetRespBodyContentType JSON (BlocksApi v c))) limit start minr maxr

-- -------------------------------------------------------------------------- --
-- Branch Hashes Client

branchHashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHashPage
branchHashesClient_ = client (branchHashesApi @v @c)

branchHashesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHashPage
branchHashesClient v c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName v)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ branchHashesClient_ @v @c limit start minr maxr bounds

-- -------------------------------------------------------------------------- --
-- Branch Headers Client

branchHeadersClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClient_ = branchHeadersClientContentType_ @v @c @JSON

branchHeadersClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClient = branchHeadersClientJson

branchHeadersClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) api
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => HeadersApi v c ~ api
    => SupportedRespBodyContentType ct api BlockHeaderPage
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClientContentType_ = client
    $ Proxy @(SetRespBodyContentType ct (BranchHeadersApi v c))

branchHeadersClientJson
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClientJson v c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName v)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ branchHeadersClientContentType_ @v @c @JSON limit start minr maxr bounds

branchHeadersClientJsonPretty
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClientJsonPretty v c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName v)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ branchHeadersClientContentType_ @v @c @JsonBlockHeaderObject limit start minr maxr bounds

-- -------------------------------------------------------------------------- --
-- Branch Blocks Client

branchBlocksClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockPage
branchBlocksClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) c limit start minr maxr bounds = runIdentity $ do
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client (Proxy @(SetRespBodyContentType JSON (BranchBlocksApi v c))) limit start minr maxr bounds

-- -------------------------------------------------------------------------- --
-- Hashes Client

hashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHashPage
hashesClient_ = client (hashesApi @v @c)

hashesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHashPage
hashesClient v c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ hashesClient_ @v @c limit start minr maxr
