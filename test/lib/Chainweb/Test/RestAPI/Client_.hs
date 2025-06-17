{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.RestAPI.Client_
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Consensus APIs for 'ClientM_'.
--
-- 'ClientM_' is a version of 'ClientM' that provides hooks to run actions on
-- the requests and responses, which is useful for testing and debugging the
-- REST API of a chainweb node.
--
-- 'ClientM_' computations can be executed using 'runClientM_' from
-- "Servant.Client_".
--
module Chainweb.Test.RestAPI.Client_
(
-- * Payload API
  payloadGetClient'
, outputsGetClient'

-- * Cut API
, cutGetClient'
, cutPutClient'

-- * BlockHeaderDB API
, headerClient'
, hashesClient'
, headersClient'
, branchHashesClient'
, branchHeadersClient'
) where

import Data.Functor.Identity

import Servant.API.ContentTypes

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI
import Chainweb.Payload
import Chainweb.Payload.RestAPI
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.Singletons

import Servant.Client_
import Chainweb.BlockHeight (BlockHeight)

-- -------------------------------------------------------------------------- --
-- Payload API

payloadGetClient'
    :: HasVersion
    => ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM_ PayloadData
payloadGetClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(PayloadGetApi v c)

outputsGetClient'
    :: HasVersion
    => ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM_ PayloadWithOutputs
outputsGetClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(OutputsGetApi v c)

-- -------------------------------------------------------------------------- --
-- Cut API

cutGetClient'
    :: HasVersion
    => Maybe MaxRank
    -> ClientM_ CutHashes
cutGetClient' = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    return $ client_ @(CutGetApi v)

cutPutClient'
    :: HasVersion
    => CutHashes
    -> ClientM_ NoContent
cutPutClient' = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    return $ client_ @(CutPutApi v)

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API

hashesClient'
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM_ (Page (NextItem BlockHash) BlockHash)
hashesClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HashesApi v c)

headerClient' :: HasVersion => ChainId -> BlockHash -> ClientM_ BlockHeader
headerClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HeaderApi v c)

headersClient'
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM_ (Page (NextItem BlockHash) BlockHeader)
headersClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HeadersApi v c)

branchHashesClient'
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM_ (Page (NextItem BlockHash) BlockHash)
branchHashesClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BranchHashesApi v c)

branchHeadersClient'
    :: HasVersion
    => ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM_ (Page (NextItem BlockHash) BlockHeader)
branchHeadersClient' c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName implicitVersion)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BranchHeadersApi v c)
