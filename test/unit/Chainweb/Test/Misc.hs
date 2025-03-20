{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.Misc
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Miscellaneous tests.
--
module Chainweb.Test.Misc
  ( tests
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Scheduler (Comp(..), scheduleWork, terminateWith, withScheduler)
import Data.ByteString qualified as BS
import Data.DoubleWord
import Data.HashMap.Strict qualified as HM
import Data.Word

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Chainweb.Payload
import Chainweb.BlockHash
import Chainweb.ChainId
import Chainweb.Utils.Serialization
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
-- import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
-- import qualified Data.ByteString as BS
import Chainweb.Utils
import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
import Chainweb.Test.Orphans.Internal ()

---

tests :: TestTree
tests = testGroup "Misc. Unit Tests"
    [ testGroup "scheduler"
        [ testCase "early termination result order" terminateOrder
        ]
    , testGroup "binary encoding"
        [ testProperty "BlockPayload" propPayloadBinaryEncoding
        , testProperty "BlockTransactions" propBlockTransactionsEncoding
        , testProperty "BlockOutputs" propBlockOutputsEncoding
        , testProperty "PayloadData" propPayloadDataEncoding
        , testProperty "PayloadWithOutputs" propPayloadWithOutputsEncoding
        ]
    , testGroup "hashed adjacent block hash record"
        [ testCase "smoke test" hashedAdjacentBlockHashRecordSmokeTest
        , testCase "truncation" hashedAdjacentBlockHashRecordTruncation
        , testCase "bit flipping" hashedAdjacentBlockHashRecordBitFlipping
        ]
    ]

-- | Guarantee that `terminateWith` makes the scheduler's "head" return value be
-- the default that was given.
--
terminateOrder :: Assertion
terminateOrder = do
    r <- withScheduler Par' $ \sch -> do
        scheduleWork sch (threadDelay 5_000_000 >> pure 1)
        scheduleWork sch (terminateWith sch 10)
    head r @?= (10 :: Int)

propPayloadBinaryEncoding :: BlockPayload -> Bool
propPayloadBinaryEncoding bp
    | Right x <- decodeBlockPayloads (encodeBlockPayloads bp) = x == bp
    | otherwise = False

propBlockTransactionsEncoding :: BlockTransactions -> Bool
propBlockTransactionsEncoding bt
    | Right x <- decodeBlockTransactions (encodeBlockTransactions bt) = x == bt
    | otherwise = False

propBlockOutputsEncoding :: BlockOutputs -> Bool
propBlockOutputsEncoding bo
    | Right x <- decodeBlockOutputs (encodeBlockOutputs bo) = x == bo
    | otherwise = False

propPayloadDataEncoding :: PayloadData -> Bool
propPayloadDataEncoding pd
    | Right x <- decodePayloadData (encodePayloadData pd) = x == pd
    | otherwise = False

propPayloadWithOutputsEncoding :: PayloadWithOutputs -> Bool
propPayloadWithOutputsEncoding pwo
    | Right x <- decodePayloadWithOutputs (encodePayloadWithOutputs pwo) = x == pwo
    | otherwise = False

encodedLike :: Put -> P.Prop BlockHashRecord
encodedLike p =
    P.fun (encodeB64UrlNoPaddingText . runPutS . encodeAdjacentsHash . adjacentsHash)
    ? P.equals (encodeB64UrlNoPaddingText $ runPutS p)

encodedUnlike :: Put -> P.Prop BlockHashRecord
encodedUnlike p =
    P.fun (runPutS . encodeAdjacentsHash . adjacentsHash)
    ? P.notEquals (runPutS p)

hashedAdjacentBlockHashRecordSmokeTest :: IO ()
hashedAdjacentBlockHashRecordSmokeTest = do
    -- smoke test
    blockHashHash <- BlockHash <$> fromText "rxPASJkSJKXkxmREa2iKr0j7VFbbNilgGwDsFgx05VQ"
    BlockHashRecord (HM.fromList [(unsafeChainId 5, nullBlockHash)])
        & encodedLike
        ? do
            encodeWordLe @Word16 0x1
            encodeWordLe @Word32 5
            encodeBlockHash (blockHashHash :: BlockHash)

hashedAdjacentBlockHashRecordTruncation :: IO ()
hashedAdjacentBlockHashRecordTruncation = do
    -- check that more than 3 adjacent chains still hashes and encodes like 3
    -- adjacent chains
    blockHashHash <- BlockHash <$> fromText "tnka1yzuG3ury0o80ccea3EiVWDwOL0pYITwsA2du7Q"
    let nullBlockHashRecord = HM.fromList $ [(unsafeChainId cid, nullBlockHash) | cid <- [0..5]]
    BlockHashRecord nullBlockHashRecord
        & encodedLike
        ? do
            encodeWordLe @Word16 0x3
            encodeWordLe @Word32 0
            encodeBlockHash (blockHashHash :: BlockHash)
            encodeWordLe @Word32 1
            encodeWordLe @Word256 0
            encodeWordLe @Word32 2
            encodeWordLe @Word256 0

hashedAdjacentBlockHashRecordBitFlipping :: IO ()
hashedAdjacentBlockHashRecordBitFlipping = do
    -- these two must be the same as previous test
    blockHashHash <- BlockHash <$> fromText "tnka1yzuG3ury0o80ccea3EiVWDwOL0pYITwsA2du7Q"
    let nullBlockHashRecord = HM.fromList $ [(unsafeChainId cid, nullBlockHash) | cid <- [0..5]]
    -- check that flipping a bit relative to the previous test in *any* hash in
    -- the record changes the output hash
    let flippedBitNullBlockHash =
            BlockHash $ unsafeMerkleLogHash $ BS.replicate 31 0x00 <> BS.singleton 0x01
    -- the initial hash
    BlockHashRecord nullBlockHashRecord
        & encodedLike ? do
            encodeWordLe @Word16 0x3
            encodeWordLe @Word32 0
            encodeBlockHash (blockHashHash :: BlockHash)
            encodeWordLe @Word32 1
            encodeWordLe @Word256 0
            encodeWordLe @Word32 2
            encodeWordLe @Word256 0
    -- must be different from all of these
    forM_ [0..5] $ \flippedCid -> do
        let flippedRecord =
                BlockHashRecord (HM.insert (unsafeChainId flippedCid) flippedBitNullBlockHash nullBlockHashRecord)
        flippedRecord
            & encodedUnlike ? do
                encodeAdjacentsHash (adjacentsHash $ BlockHashRecord nullBlockHashRecord)
