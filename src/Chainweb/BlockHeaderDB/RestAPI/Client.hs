{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
, headerPutClient_
, headerPutClient
, hashesClient_
, hashesClient
, headersClient_
, headersClient
, branchHashesClient_
, branchHashesClient
, branchHeadersClient_
, branchHeadersClient
, childHashesClient_
, childHashesClient
, childHeadersClient_
, childHeadersClient
) where

import Control.Monad.Identity

import Data.Proxy

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.ChainId
import Chainweb.RestAPI.Orphans ()
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
    -> ClientM (DbEntry BlockHeaderDb)
headerClient_ = client (headerApi @v @c)

headerClient
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM (DbEntry BlockHeaderDb)
headerClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ headerClient_ @v @c k

-- -------------------------------------------------------------------------- --
-- Header Put Client

headerPutClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClient_ = client (headerPutApi @v @c)

headerPutClient
    :: ChainwebVersion
    -> ChainId
    -> DbEntry BlockHeaderDb
    -> ClientM NoContent
headerPutClient v c e = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ headerPutClient_ @v @c e

-- -------------------------------------------------------------------------- --
-- Headers Client

headersClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
headersClient_ = client (headersApi @v @c)

headersClient
    :: ChainwebVersion
        -- ^ The remote chainweb that you wish to query from.
    -> ChainId
        -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Limit
        -- ^ The number of responses per-`Page` to return.
    -> Maybe (NextItem (DbKey BlockHeaderDb))
        -- ^ The first header you want to see within the `Page`.
        -- `Page` contains a field `_pageNext`, which can be used
        -- to produce the value needed for subsequent calls.
    -> Maybe MinRank
        -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe MaxRank
        -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
headersClient v c limit start minr maxr = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ headersClient_ @v @c limit start minr maxr

-- -------------------------------------------------------------------------- --
-- Branch Hashes Client

branchHashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
branchHashesClient_ = client (branchHashesApi @v @c)

branchHashesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
branchHashesClient v c limit start minr maxr bounds = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ branchHashesClient_ @v @c limit start minr maxr bounds

-- -------------------------------------------------------------------------- --
-- Branch Headers Client

branchHeadersClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
branchHeadersClient_ = client (branchHeadersApi @v @c)

branchHeadersClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
branchHeadersClient v c limit start minr maxr bounds = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ branchHeadersClient_ @v @c limit start minr maxr bounds

-- -------------------------------------------------------------------------- --
-- Hashes Client

hashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
hashesClient_ = client (hashesApi @v @c)

hashesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
hashesClient v c limit start minr maxr = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ hashesClient_ @v @c limit start minr maxr

-- -------------------------------------------------------------------------- --
-- Children Hashes Client

childHashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbKey BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
childHashesClient_ = client (childHashesApi @v @c)

childHashesClient
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
childHashesClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ childHashesClient_ @v @c k

-- -------------------------------------------------------------------------- --
-- Children Headers Client

childHeadersClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => DbKey BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
childHeadersClient_ = client (childHeadersApi @v @c)

childHeadersClient
    :: ChainwebVersion
    -> ChainId
    -> DbKey BlockHeaderDb
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbEntry BlockHeaderDb))
childHeadersClient v c k = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ childHeadersClient_ @v @c k
