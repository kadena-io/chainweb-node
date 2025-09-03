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

import Chainweb.Pact.Payload
import Chainweb.Test.Orphans.Internal ()

import Control.Concurrent (threadDelay)
import Control.Scheduler (Comp(..), scheduleWork, terminateWith, withScheduler)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Chainweb.BlockHash
import Data.HashMap.Strict qualified as HM
import Chainweb.ChainId

import PropertyMatchers ((?))
import PropertyMatchers qualified as P
-- import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
import Data.Function ((&))
-- import qualified Data.ByteString as BS
import Chainweb.Utils (HasTextRepresentation(..))
import Chainweb.MerkleLogHash (unsafeMerkleLogHash)
import qualified Data.ByteString as BS
import Control.Lens
import Control.Monad
import Chainweb.Parent
import Chainweb.Utils.Serialization
import Chainweb.MerkleUniverse

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
        -- , testCase "null padding" hashedAdjacentBlockHashRecordNullPadding
        -- , testCase "truncation" hashedAdjacentBlockHashRecordTruncation
        -- , testCase "bit flipping" hashedAdjacentBlockHashRecordBitFlipping
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

hashedAdjacentBlockHashRecordSmokeTest :: IO ()
hashedAdjacentBlockHashRecordSmokeTest = do
    -- smoke test
    blockHashHash <- fromText "rxPASJkSJKXkxmREa2iKr0j7VFbbNilgGwDsFgx05VQ"
    let hashRecord = BlockHashRecord (HM.fromList [(unsafeChainId 0, Parent nullBlockHash)])
    encodeAdjacentsHash (adjacentsHash hashRecord)
        & runPutS
        & P.equals (runPutS $ encodeBlockHash $ BlockHash @ChainwebMerkleHashAlgorithm blockHashHash)

-- hashedAdjacentBlockHashRecordNullPadding :: IO ()
-- hashedAdjacentBlockHashRecordNullPadding = do
--     -- check that the first chain has a null block hash with a multi-chain record
--     blockHashHash <- fromText "iu7PoLnyrHgYhjsTYiQeTzLQaxAK6dHA-8xO1huRsXo"
--     convertBlockHashRecordForMining
--         (BlockHashRecord (HM.fromList [(unsafeChainId 0, nullBlockHash), (unsafeChainId 1, nullBlockHash)]))
--         & P.equals
--         ? BlockHashRecord
--         ? HM.fromList [(unsafeChainId 0, nullBlockHash), (unsafeChainId 1, BlockHash blockHashHash)]

-- hashedAdjacentBlockHashRecordTruncation :: IO ()
-- hashedAdjacentBlockHashRecordTruncation = do
--     -- check that more than 3 chains gets truncated
--     blockHashHash <- fromText "tnka1yzuG3ury0o80ccea3EiVWDwOL0pYITwsA2du7Q"
--     let nullBlockHashRecord = HM.fromList $ [(unsafeChainId cid, nullBlockHash) | cid <- [0..5]]
--     convertBlockHashRecordForMining (BlockHashRecord nullBlockHashRecord)
--         & P.equals
--         ? BlockHashRecord
--         ? HM.fromList ([(unsafeChainId cid, nullBlockHash) | cid <- [0..1]] ++ [(unsafeChainId 2, blockHashHash)])

-- hashedAdjacentBlockHashRecordBitFlipping :: IO ()
-- hashedAdjacentBlockHashRecordBitFlipping = do
--     -- these two must be the same as previous test
--     blockHashHash <- fromText "tnka1yzuG3ury0o80ccea3EiVWDwOL0pYITwsA2du7Q"
--     let nullBlockHashRecord = HM.fromList $ [(unsafeChainId cid, nullBlockHash) | cid <- [0..5]]
--     -- check that flipping a bit relative to the previous test in *any* hash in
--     -- the record changes the output hash
--     let flippedBitNullBlockHash =
--             BlockHash $ unsafeMerkleLogHash $ BS.replicate 31 0x00 <> BS.singleton 0x01
--     forM_ [0..5] $ \flippedCid -> do
--         let flippedRecord =
--                 BlockHashRecord (HM.insert (unsafeChainId flippedCid) flippedBitNullBlockHash nullBlockHashRecord)
--         convertBlockHashRecordForMining flippedRecord
--             -- TODO: replace when property-matchers has notEquals check
--             & (\expected actual ->
--                 if expected /= actual
--                 then P.succeed actual
--                 else P.fail "equal, should not be" actual)
--             ? BlockHashRecord
--             ? HM.fromList ([(unsafeChainId cid, nullBlockHash) | cid <- [0..1]] ++ [(unsafeChainId 2, blockHashHash)])
