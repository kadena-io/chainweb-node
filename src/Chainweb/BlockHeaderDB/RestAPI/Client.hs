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
, newHeaderClient

, hashesClient
, headersClient
, newHashesClient
, newHeadersClient
, newBranchHashesClient
, newBranchHeadersClient

, branchHashesClient

, branchHeadersClient
, branchHeadersClientJson
) where

import Control.Lens
import Control.Monad.Identity

import Data.Aeson(encode)
import Data.Kind
import Data.Proxy
import Data.Singletons

import Servant.API
import Servant.Client (ClientM, client)

import Network.HTTP.Client(RequestBody(..), responseBody)
import Network.HTTP.Media hiding ((//))
import Network.HTTP.Types

import Web.DeepRoute hiding (QueryParam)
import Web.DeepRoute.Client
import Web.DeepRoute.Wai

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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JSON k

headerClientJsonPretty
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonPretty v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JsonBlockHeaderObject k

headerClientJsonBinary
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonBinary v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @OctetStream k

newHeaderClient
    :: ClientEnv
    -> ChainwebVersion
    -> ChainId
    -> BlockHash
    -> IO BlockHeader
newHeaderClient e v cid bh =
    let req = doRequest e $
            "chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@@ bh `withMethod` methodGet
                & requestAcceptable ?~ ["application/octet-stream"]
    in req $ readLazyResponseBody (runGetL decodeBlockHeader)

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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JSON limit start minr maxr

includePageParams :: ToHttpApiData (NextItem k) => Maybe Limit -> Maybe (NextItem k) -> ApiRequest -> ApiRequest
includePageParams limit start r = r
    & requestQuery <>~ [ ("limit", Just $ toQueryParam lim) | Just lim <- [limit] ]
    & requestQuery <>~ [ ("next", Just $ toQueryParam next) | Just next <- [start] ]

includeFilterParams :: Maybe MinRank -> Maybe MaxRank -> ApiRequest -> ApiRequest
includeFilterParams minr maxr r = r
    & requestQuery <>~ [ ("minheight", Just $ toQueryParam mh) | Just mh <- [minr] ]
    & requestQuery <>~ [ ("maxheight", Just $ toQueryParam mh) | Just mh <- [maxr] ]

newHeadersClient
    :: ClientEnv
    -> ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> IO BlockHeaderPage
newHeadersClient e v cid limit start minr maxr = doJSONRequest e $
     "chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" `withMethod` methodGet
        & requestAcceptable ?~ ["application/json"]
        & includePageParams limit start
        & includeFilterParams minr maxr

newHashesClient
    :: ClientEnv
    -> ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> IO BlockHashPage
newHashesClient e v cid limit start minr maxr = doJSONRequest e $
    "chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "hash" `withMethod` methodGet
        & requestAcceptable ?~ ["application/json"]
        & includePageParams limit start
        & includeFilterParams minr maxr

newBranchHeadersClient
    :: ClientEnv
    -> ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> IO BlockHeaderPage
newBranchHeadersClient e v cid limit start minr maxr bb = doJSONRequest e $
    "chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "header" /@ "branch" `withMethod` methodPost
        & requestAcceptable ?~ ["application/json"]
        & includePageParams limit start
        & includeFilterParams minr maxr
        & requestBody .~ RequestBodyLBS (encode bb)

newBranchHashesClient
    :: ClientEnv
    -> ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> IO BlockHashPage
newBranchHashesClient e v cid limit start minr maxr bb = doJSONRequest (e ^. clientEnv) $
    "chainweb" /@ "0.0" /@@ v /@ "chain" /@@ cid /@ "hash" /@ "branch" `withMethod` methodPost
        & requestAcceptable ?~ ["application/json"]
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
headersClientJsonPretty v c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JsonBlockHeaderObject limit start minr maxr

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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ branchHeadersClientContentType_ @v @c @JsonBlockHeaderObject limit start minr maxr bounds

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
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ hashesClient_ @v @c limit start minr maxr
