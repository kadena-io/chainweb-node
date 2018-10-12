{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.ChainDB.RestAPI.Client
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
module Chainweb.ChainDB.RestAPI.Client
( headerClient_
, headerClient
, headerPutClient_
, headerPutClient
, hashesClient_
, hashesClient
, headersClient_
, headersClient
, branchesClient_
, branchesClient
) where

import Control.Monad.Identity

import Data.Proxy

import Numeric.Natural

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules
import Chainweb.ChainDB
import Chainweb.ChainDB.RestAPI
import Chainweb.ChainDB.RestAPI.Orphans ()
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Version


-- -------------------------------------------------------------------------- --
-- GET Header Client

headerClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Key 'Unchecked
    -> ClientM (Entry 'Unchecked)
headerClient_ = client (headerApi @v @c)

headerClient
    :: ChainwebVersion
    -> ChainId
    -> Key 'Unchecked
    -> ClientM (Entry 'Unchecked)
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
    => Entry 'Unchecked
    -> ClientM NoContent
headerPutClient_ = client (headerPutApi @v @c)

headerPutClient
    :: ChainwebVersion
    -> ChainId
    -> Entry 'Unchecked
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
    => Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Maybe (Key 'Unchecked, Key 'Unchecked)
    -> ClientM (Page (Key 'Unchecked) (Entry 'Unchecked))
headersClient_ = client (headersApi @v @c)

headersClient
    :: ChainwebVersion  -- ^ The remote chainweb that you wish to query from.
    -> ChainId  -- ^ The remote chain within the web that you wish to query from.
    -> Maybe Natural  -- ^ The number of responses per-`Page` to return.
    -> Maybe (Key 'Unchecked)  -- ^ The first header you want to see within the `Page`.
                               -- `Page` contains a field `_pageNext`, which can be used
                               -- to produce the value needed for subsequent calls.
    -> Maybe Natural  -- ^ Filter: no header of `BlockHeight` lower than this will be returned.
    -> Maybe Natural  -- ^ Filter: no header of `BlockHeight` higher than this will be returned.
    -> Maybe (Key 'Unchecked, Key 'Unchecked)  -- ^ Filter: only yield headers between two specific hashes,
                                               -- i.e. a single, specific branch.
    -> ClientM (Page (Key 'Unchecked) (Entry 'Unchecked))
headersClient v c limit start minr maxr range = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ headersClient_ @v @c limit start minr maxr range

-- -------------------------------------------------------------------------- --
-- Branches Client

branchesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> ClientM (Page (Key 'Unchecked) (Key 'Unchecked))
branchesClient_ = client (branchesApi @v @c)

branchesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> ClientM (Page (Key 'Unchecked) (Key 'Unchecked))
branchesClient v c limit start minr maxr = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ branchesClient_ @v @c limit start minr maxr

-- -------------------------------------------------------------------------- --
-- Hashes Client

hashesClient_
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Maybe (Key 'Unchecked, Key 'Unchecked)
    -> ClientM (Page (Key 'Unchecked) (Key 'Unchecked))
hashesClient_ = client (hashesApi @v @c)

hashesClient
    :: ChainwebVersion
    -> ChainId
    -> Maybe Natural
    -> Maybe (Key 'Unchecked)
    -> Maybe Natural
    -> Maybe Natural
    -> Maybe (Key 'Unchecked, Key 'Unchecked)
    -> ClientM (Page (Key 'Unchecked) (Key 'Unchecked))
hashesClient v c limit start minr maxr range = runIdentity $ do
    SomeChainwebVersionT (_ :: Proxy v) <- return $ someChainwebVersionVal v
    SomeChainIdT (_ :: Proxy c) <- return $ someChainIdVal c
    return $ hashesClient_ @v @c limit start minr maxr range
