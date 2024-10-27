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
, blocksClient'
, branchHashesClient'
, branchHeadersClient'
, branchBlocksClient'
) where

import Data.Functor.Identity

import Servant.API.ContentTypes

-- internal modules

import Chainweb.Block
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
    :: ChainwebVersion
    -> ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM_ PayloadData
payloadGetClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(PayloadGetApi v c)

outputsGetClient'
    :: ChainwebVersion
    -> ChainId
    -> BlockPayloadHash
    -> Maybe BlockHeight
    -> ClientM_ PayloadWithOutputs
outputsGetClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(OutputsGetApi v c)

-- -------------------------------------------------------------------------- --
-- Cut API

cutGetClient'
    :: ChainwebVersion
    -> Maybe MaxRank
    -> ClientM_ CutHashes
cutGetClient' v = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    return $ client_ @(CutGetApi v)

cutPutClient'
    :: ChainwebVersion
    -> CutHashes
    -> ClientM_ NoContent
cutPutClient' v = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    return $ client_ @(CutPutApi v)

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API

hashesClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM_ (Page (NextItem BlockHash) BlockHash)
hashesClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HashesApi v c)

headerClient' :: ChainwebVersion -> ChainId -> BlockHash -> ClientM_ BlockHeader
headerClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HeaderApi v c)

headersClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM_ (Page (NextItem BlockHash) BlockHeader)
headersClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(HeadersApi v c)

blocksClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> ClientM_ BlockPage
blocksClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BlocksApi v c)

branchHashesClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM_ (Page (NextItem BlockHash) BlockHash)
branchHashesClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BranchHashesApi v c)

branchHeadersClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM_ (Page (NextItem BlockHash) BlockHeader)
branchHeadersClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BranchHeadersApi v c)

branchBlocksClient'
    :: ChainwebVersion
    -> ChainId
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> ClientM_ (Page (NextItem BlockHash) Block)
branchBlocksClient' v c = runIdentity $ do
    (SomeSing (SChainwebVersion :: Sing v)) <- return $ toSing (_versionName v)
    (SomeSing (SChainId :: Sing c)) <- return $ toSing c
    return $ client_ @(BranchBlocksApi v c)
