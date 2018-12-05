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
-- Usage Example:
--
-- @
-- clientTest :: IO ()
-- clientTest = do
--     mgr <- HTTP.newManager HTTP.defaultManagerSettings
--     _ <- runClientM session (mkClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
--     _ <- runClientM session_ (mkClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
--     return ()
--   where
--
--     cid0 = testChainId 0
--     cid1 = testChainId 1
--
--     # Client API with untyped ChainIds
--     session = do
--         liftIO $ putStrLn "Test 1"
--         r <- headersClient Test cid0 Nothing Nothing Nothing Nothing Nothing
--         liftIO $ print r
--
--         liftIO $ putStrLn "\nTest 2"
--         _ <- headerPutClient Test cid0 (head $ _pageItems r)
--
--         liftIO $ putStrLn "\nTest 3"
--         _ <- headersClient_ Test cid1
--             Nothing Nothing Nothing Nothing Nothing
--         liftIO $ print r
--
--     # Client API with typed ChainIds
--     session_ = do
--         liftIO $ putStrLn "Test 1"
--         r <- headersClient_ @('ChainwebVersionT "Test") @('ChainIdT "0")
--             Nothing Nothing Nothing Nothing Nothing
--         liftIO $ print r
--
--         liftIO $ putStrLn "\nTest 2"
--         _ <- headerPutClient_ @('ChainwebVersionT "Test") @('ChainIdT "0") (head $ _pageItems r)
--
--         liftIO $ putStrLn "\nTest 3"
--         _ <- headersClient_ @('ChainwebVersionT "Test") @('ChainIdT "1")
--             Nothing Nothing Nothing Nothing Nothing
--         liftIO $ print r
-- @
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
, leavesClient_
, leavesClient
-- , branchesClient_
-- , branchesClient
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
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
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
-- Branches Client

leavesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
leavesClient_ = client (leavesApi @v @c)

leavesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
leavesClient v c limit start minr maxr = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ leavesClient_ @v @c limit start minr maxr

{-
-- -------------------------------------------------------------------------- --
-- Branches Client

branchesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Maybe (Bounds (DbKey BlockHeaderDb))
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
branchesClient_ = client (branchesApi @v @c)

branchesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Maybe (Bounds (DbKey BlockHeaderDb))
    -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
branchesClient v c limit start minr maxr range = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ branchesClient_ @v @c limit start minr maxr range
-}

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
