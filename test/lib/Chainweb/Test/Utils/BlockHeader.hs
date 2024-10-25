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
, testGetNewAdjacentParentHeaders

-- * Test Payloads
, testPayload
, testBlockPayloadFromParent
, testBlockPayload
, testBlockPayload_
) where

import Control.Arrow ((&&&))
import Control.Lens

import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Vector as V

import GHC.Stack

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainValue
import Chainweb.Payload
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Test Payloads

-- | Test PayloadWithOutputs that is parameterized with some bytes.
--
testPayload :: B8.ByteString -> PayloadWithOutputs
testPayload n = newPayloadWithOutputs
    (MinerData "TEST MINER DATA")
    (CoinbaseOutput "TEST COINBASE OUTPUT")
    (V.singleton
        ( Transaction $ "TEST TRANSACTION: " <> n
        , TransactionOutput $ "TEST OUTPUT: " <> n
        )
    )

-- | Generate a test payload for a given parent header. Includes the block
-- height of the block.
--
-- Payloads that are created with this function match respective payloads
-- that are created with 'testBlockPayload'.
--
testBlockPayloadFromParent :: ParentHeader -> PayloadWithOutputs
testBlockPayloadFromParent (ParentHeader b) = testPayload $ B8.intercalate ","
    [ sshow (_chainwebVersion b)
    , sshow (view blockHeight b + 1)
    ]

-- | Generate a test payload for a given header. Includes the block height of
-- the block.
--
-- Payloads that are created with this function match respective payloads
-- that are created with 'testBlockPayloadFromParent'.
--
testBlockPayload :: BlockHeader -> PayloadWithOutputs
testBlockPayload b = testPayload $ B8.intercalate ","
    [ sshow (_chainwebVersion b)
    , sshow (view blockHeight b)
    ]

-- | Generate a test payload for a given parent header. Includes the block
-- height of the block and the given Nonce.
--
-- Payloads that are created with this function match respective payloads
-- that are created with 'testBlockPayload_', assuming that the same nonce is
-- used.
--
testBlockPayloadFromParent_ :: Nonce -> ParentHeader -> PayloadWithOutputs
testBlockPayloadFromParent_ n (ParentHeader b) = testPayload $ B8.intercalate ","
    [ sshow (_chainwebVersion b)
    , sshow (view blockHeight b + 1)
    , sshow n
    ]

-- | Includes the nonce of the block.
--
-- Payloads that are created with this function match respective payloads
-- that are created with 'testBlockPayloadFromParent_', assuming that the same
-- nonce is used.
--
testBlockPayload_ :: BlockHeader -> PayloadWithOutputs
testBlockPayload_ b = testPayload $ B8.intercalate ","
    [ sshow (_chainwebVersion b)
    , sshow (view blockHeight b)
    , sshow (view blockNonce b)
    ]

-- -------------------------------------------------------------------------- --
-- Testing

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
    :: HM.HashMap ChainId ParentHeader
        -- ^ Adjacent parent hashes
    -> Nonce
        -- ^ Randomness to affect the block hash. It is also included into
        -- the payload
    -> ParentHeader
        -- ^ parent block header
    -> BlockHeader
testBlockHeader adj nonce p@(ParentHeader b) =
    newBlockHeader adj payload nonce (BlockCreationTime $ add second t) p
  where
    payload = _payloadWithOutputsPayloadHash $ testBlockPayloadFromParent_ nonce p
    BlockCreationTime t = view blockCreationTime b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeaders :: ParentHeader -> [BlockHeader]
testBlockHeaders (ParentHeader p) = L.unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader mempty (view blockNonce b) $ ParentHeader b

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
--
-- Should only be used for testing purposes.
--
testBlockHeadersWithNonce :: Nonce -> ParentHeader -> [BlockHeader]
testBlockHeadersWithNonce n (ParentHeader p) = L.unfoldr (Just . (id &&& id) . f) p
  where
    f b = testBlockHeader mempty n $ ParentHeader b
