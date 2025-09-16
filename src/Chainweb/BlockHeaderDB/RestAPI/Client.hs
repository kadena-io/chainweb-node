{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
( headerClient_
, headerClient
, headerClientContentType_
, headerClientJson
, headerClientJsonPretty
, headerClientJsonBinary

, hashesClient_
, hashesClient
, headersClient_
, headersClient
, headersClientContentType_
, headersClientJson
, headersClientJsonPretty


, branchHashesClient_
, branchHashesClient

, branchHeadersClient_
, branchHeadersClient
, branchHeadersClientContentType_
, branchHeadersClientJson
, branchHeadersClientJsonPretty
) where

import Control.Monad.Identity

import Data.Kind
import Data.Proxy
import Data.Singletons

import Servant.API
import Servant.Client

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging
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
    :: HasVersion
    => ChainId
    -> DbKey BlockHeaderDb
    -> ClientM BlockHeader
headerClient = headerClientJsonBinary

headerClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) x
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Accept ct
    => SupportedRespBodyContentType ct x BlockHeader
    => (HeaderApi v c) ~ x
    => BlockHash
    -> ClientM BlockHeader
headerClientContentType_ = client (Proxy @(SetRespBodyContentType ct x))

headerClientJson
    :: HasVersion
    => ChainId
    -> DbKey BlockHeaderDb
    -> ClientM BlockHeader
headerClientJson c k = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JSON k

headerClientJsonPretty
    :: HasVersion
    => ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonPretty c k = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JsonBlockHeaderObject k

headerClientJsonBinary
    :: HasVersion
    => ChainId
    -> BlockHash
    -> ClientM BlockHeader
headerClientJsonBinary c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @OctetStream k

-- -------------------------------------------------------------------------- --
-- Ranked Header Client

rankedHeaderClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbKey RankedBlockHeaderDb
    -> ClientM BlockHeader
rankedHeaderClient_ = rankedHeaderClientContentType_ @v @c @OctetStream

rankedHeaderClient
    :: HasVersion
    => ChainId
    -> DbKey RankedBlockHeaderDb
    -> ClientM BlockHeader
rankedHeaderClient = rankedHeaderClientJsonBinary

rankedHeaderClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) x
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Accept ct
    => SupportedRespBodyContentType ct x BlockHeader
    => (HeaderApi v c) ~ x
    => RankedBlockHash
    -> ClientM BlockHeader
rankedHeaderClientContentType_ = client (Proxy @(SetRespBodyContentType ct x))

rankedHeaderClientJson
    :: HasVersion
    => ChainId
    -> DbKey RankedBlockHeaderDb
    -> ClientM BlockHeader
rankedHeaderClientJson c (RankedBlockHash h k) = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ rankedHeaderClientContentType_ @v @c @JSON h k

rankedHeaderClientJsonPretty
    :: HasVersion
    => ChainId
    -> RankedBlockHash
    -> ClientM BlockHeader
rankedHeaderClientJsonPretty c (RankedBlockHash h k) = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ rankedHeaderClientContentType_ @v @c @JsonBlockHeaderObject h k

rankedHeaderClientJsonBinary
    :: HasVersion
    => ChainId
    -> RankedBlockHash
    -> ClientM BlockHeader
rankedHeaderClientJsonBinary c (RankedBlockHash h k) = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ rankedHeaderClientContentType_ @v @c @OctetStream h k

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
    :: HasVersion
        -- ^ The remote chainweb that you wish to query from.
    => ChainId
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
    => Accept ct
    => HeadersApi v c ~ x
    => SupportedRespBodyContentType ct x BlockHeaderPage
    => Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHeaderPage
headersClientContentType_ = client $ Proxy @(SetRespBodyContentType ct (HeadersApi v c))

headersClientJson
    :: HasVersion
        -- ^ The remote chainweb that you wish to query from.
    => ChainId
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
headersClientJson c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JSON limit start minr maxr

headersClientJsonPretty
    :: HasVersion
        -- ^ The remote chainweb that you wish to query from.
    => ChainId
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
headersClientJsonPretty c limit start minr maxr = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
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
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHashPage
branchHashesClient c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
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
    :: HasVersion
    => ChainId
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
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClientJson c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
    return $ branchHeadersClientContentType_ @v @c @JSON limit start minr maxr bounds

branchHeadersClientJsonPretty
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM BlockHeaderPage
branchHeadersClientJsonPretty c limit start minr maxr bounds = runIdentity $ do
    SomeSing (SChainwebVersion :: Sing v) <- return $ toSing (_versionName implicitVersion)
    SomeSing (SChainId :: Sing c) <- return $ toSing c
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
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM BlockHashPage
hashesClient c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ hashesClient_ @v @c limit start minr maxr
