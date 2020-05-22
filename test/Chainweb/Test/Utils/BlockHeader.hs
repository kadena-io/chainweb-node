{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Utils.BlockHeader
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Utils.BlockHeader
( testBlockHeader
, testBlockHeaders
, testBlockHeadersWithNonce
, testBlockPayload
, testGetNewAdjacentParentHeaders
) where

import Control.Arrow ((&&&))
import Control.Lens

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

import GHC.Stack

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainValue
import Chainweb.Time
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Testing

testBlockPayload :: BlockHeader -> BlockPayloadHash
testBlockPayload b = hashPayload (_blockChainwebVersion b) b "TEST PAYLOAD"

testGetNewAdjacentParentHeaders
    :: HasCallStack
    => Applicative m
    => ChainwebVersion
    -> (ChainValue BlockHash -> m BlockHeader)
    -> BlockHashRecord
    -> m (HM.HashMap ChainId (Either BlockHash ParentHeader))
testGetNewAdjacentParentHeaders v hdb = itraverse select . _getBlockHashRecord
  where
    select cid h
        | h == genesisParentBlockHash v cid = pure $ Left h
        | otherwise = Right . ParentHeader <$> hdb (ChainValue cid h)

testBlockHeader
    :: HM.HashMap ChainId (Either BlockHash ParentHeader)
        -- ^ Adjacent parent hashes
    -> Nonce
        -- ^ Randomness to affect the block hash
    -> ParentHeader
        -- ^ parent block header
    -> BlockHeader
testBlockHeader adj nonce p@(ParentHeader b) =
    newBlockHeader adj (testBlockPayload b) nonce (BlockCreationTime $ add second t) p
  where
    BlockCreationTime t = _blockCreationTime b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeaders :: ParentHeader -> [BlockHeader]
testBlockHeaders (ParentHeader p) = L.unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader mempty (_blockNonce b) $ ParentHeader b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeadersWithNonce :: Nonce -> ParentHeader -> [BlockHeader]
testBlockHeadersWithNonce n (ParentHeader p) = L.unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader mempty n $ ParentHeader b
