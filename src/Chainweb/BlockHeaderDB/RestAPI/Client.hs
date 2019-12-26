{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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

, headerPutClient_
, headerPutClient
, headerPutClientContentType_
, headerPutClientJson
, headerPutClientJsonPretty
, headerPutClientBinary

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
import Data.Text (Text)

import GHC.TypeLits

import Servant.API
import Servant.Client

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.RestAPI.Orphans ()
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Utils

-- Response Body Content Type

type family SetRespBodyContentType ct api where
    SetRespBodyContentType ct (Verb a b _ c) = Verb a b '[ct] c
    SetRespBodyContentType ct (a :> b) = a :> SetRespBodyContentType ct b

type SupportedRespBodyContentType ct api t = (SupportedRespBodyCT_ ct api api ~ 'True, MimeUnrender ct t)

type family SupportedRespBodyCT_ (ct :: Type) (api :: k) (arg :: k1) :: Bool where
    SupportedRespBodyCT_ ct api (Verb _ _ '[] _) = RespBodyContentTypeNotSupportedMsg ct api
    SupportedRespBodyCT_ ct api (Verb _ _ (ct ': _) _) = 'True
    SupportedRespBodyCT_ ct api (Verb a b (_ ': t) c) = SupportedRespBodyCT_ ct api (Verb a b t c)
    SupportedRespBodyCT_ ct api (a :> b) = SupportedRespBodyCT_ ct api b

type family RespBodyContentTypeNotSupportedMsg ct api where
    RespBodyContentTypeNotSupportedMsg ct api = TypeError
        ( 'Text "The response content type "
        ':<>: 'ShowType ct
        ':<>: 'Text " is not supported by the servant API "
        ':$$: 'ShowType api
        )

-- Request Body Content Type

type family SetReqBodyContentType (ct :: Type) (api :: k) :: k1 where
    SetReqBodyContentType ct (ReqBody _ t :> a) = ReqBody '[ct] t :> a
    SetReqBodyContentType ct (a :> b) = a :> SetReqBodyContentType ct b

type SupportedReqBodyContentType ct api t = (SupportedReqBodyCT_ ct api api ~ 'True, MimeRender ct t)

type family SupportedReqBodyCT_ (ct :: Type) (api :: k) (arg :: k1) :: Bool where
    SupportedReqBodyCT_ ct api (ReqBody '[] _ :> _) = ReqBodyContentTypeNotSupportedMsg ct api
    SupportedReqBodyCT_ ct api (ReqBody (ct ': _) _ :> _) = 'True
    SupportedReqBodyCT_ ct api (ReqBody (_ ': x) t :> a) = SupportedReqBodyCT_ ct api (ReqBody x t :> a)
    SupportedReqBodyCT_ ct api (a :> b) = SupportedReqBodyCT_ ct api b

type family ReqBodyContentTypeNotSupportedMsg ct api where
    ReqBodyContentTypeNotSupportedMsg ct api = TypeError
        ( 'Text "The request content type "
        ':<>: 'ShowType ct
        ':<>: 'Text " is not supported by the servant API "
        ':$$: 'ShowType api
        )

-- -------------------------------------------------------------------------- --
-- GET Header Client

headerClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbKey BlockHeaderDb
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClient_ = headerClientContentType_ @v @c @JSON

headerClient
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClient = headerClientJson

headerClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type) x
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Accept ct
    => SupportedRespBodyContentType ct x BlockHeader
    => (HeaderApi v c) ~ x
    => BlockHash
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClientContentType_ = client (Proxy @(SetRespBodyContentType ct x))

headerClientJson
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClientJson v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JSON k

headerClientJsonPretty
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClientJsonPretty v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @JsonBlockHeaderObject k

headerClientJsonBinary
    :: ChainwebVersion
    -> ChainId
    -> BlockHash
    -> ClientM (Headers '[Header "Vary" Text] BlockHeader)
headerClientJsonBinary v c k = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headerClientContentType_ @v @c @OctetStream k

-- -------------------------------------------------------------------------- --
-- Header Put Client

headerPutClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClient_ = headerPutClientContentType_ @v @c @JSON

headerPutClient
    :: ChainwebVersion
    -> ChainId
    -> DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClient = headerPutClientJson

headerPutClientContentType_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT) (ct :: Type)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Accept ct
    => SupportedReqBodyContentType ct (HeaderPutApi v c) BlockHeader
    => DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClientContentType_ = client
    $ Proxy @(SetReqBodyContentType ct (HeaderPutApi v c))

headerPutClientJson
    :: ChainwebVersion
    -> ChainId
    -> DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClientJson (FromSing (SChainwebVersion :: Sing v)) (FromSing (SChainId :: Sing c))
    = headerPutClientContentType_ @v @c @JSON

headerPutClientJsonPretty
    :: ChainwebVersion
    -> ChainId
    -> DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClientJsonPretty (FromSing (SChainwebVersion :: Sing v)) (FromSing (SChainId :: Sing c))
    = headerPutClientContentType_ @v @c @JsonBlockHeaderObject

headerPutClientBinary
    :: ChainwebVersion
    -> ChainId
    -> DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClientBinary (FromSing (SChainwebVersion :: Sing v)) (FromSing (SChainId :: Sing c))
    = headerPutClientContentType_ @v @c @OctetStream

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
    -> ClientM (Headers '[Header "Vary" Text] BlockHeaderPage)
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
    -> ClientM (Headers '[Header "Vary" Text] BlockHeaderPage)
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
    -> ClientM (Headers '[Header "Vary" Text] BlockHeaderPage)
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
    -> ClientM (Headers '[Header "Vary" Text] BlockHeaderPage)
headersClientJson v c limit start minr maxr = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing v
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ headersClientContentType_ @v @c @JSON limit start minr maxr

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
    -> ClientM (Headers '[Header "Vary" Text] BlockHeaderPage)
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
