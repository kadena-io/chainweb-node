{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.SPV.EventProof
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.SPV.EventProof
( properties
) where

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Bifunctor
import Data.MerkleLog
import qualified Data.Vector as V

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.Test.Orphans.Internal

-- -------------------------------------------------------------------------- --
-- Properties

properties :: TestTree
properties = testGroup "Chainweb.Test.SPV.EventProof"
    [ testGroup "int256"
        [ testProperty "int256_maxBound" prop_int256_maxBound
        , testProperty "int256_minBound" prop_int256_minBound
        , testProperty "int256" prop_int256
        , testProperty "int256_outOfBounds_maxBound" prop_int256_outOfBounds_maxBound
        , testProperty "int256_outOfBounds_minBound" prop_int256_outOfBounds_minBound
        ]
    , proof_properties
    ]

-- -------------------------------------------------------------------------- --
-- Int256

prop_int256_maxBound :: Property
prop_int256_maxBound = once
    $ unsafeInt256 (2^(255 :: Int) - 1) === maxBound

prop_int256_minBound :: Property
prop_int256_minBound = once
    $ unsafeInt256 (- (2^(255 :: Int))) === minBound

prop_int256 :: Int256 -> Property
prop_int256 i = first show (int256 (int256ToInteger i)) === Right i

prop_int256_outOfBounds_maxBound :: Property
prop_int256_outOfBounds_maxBound =
    first fromException (int256 x) === Left (Just (OutOfBoundsException x))
  where
    x = int256ToInteger maxBound + 1

prop_int256_outOfBounds_minBound :: Property
prop_int256_outOfBounds_minBound =
    first fromException (int256 x) === Left (Just (OutOfBoundsException x))
  where
    x = int256ToInteger minBound - 1

-- -------------------------------------------------------------------------- --
-- Proofs

proof_properties :: TestTree
proof_properties = testGroup "merkle proof properties"
    [ testGroup "ChainwebMerklehashAlgorithm"
        [ testProperty "prop_merkleProof_run" $ prop_merkleProof_run @ChainwebMerkleHashAlgorithm
        , testProperty "prop_eventsProof_run" $ prop_eventsProof_run @ChainwebMerkleHashAlgorithm
        , testProperty "prop_eventsProof_run2" $ prop_eventsProof_run2 @ChainwebMerkleHashAlgorithm
        , testProperty "fail_eventsProof_run2" $ fail_eventsProof_run2 @ChainwebMerkleHashAlgorithm
        , testProperty "prop_eventsProof_subject" $ prop_eventsProof_subject @ChainwebMerkleHashAlgorithm
        , testProperty "prop_eventsProof_valid" $ prop_eventsProof_valid @ChainwebMerkleHashAlgorithm
        ]
    , testGroup "ChainwebMerklehashAlgorithm"
        [ testProperty "prop_merkleProof_run" $ prop_merkleProof_run @Keccak_256
        , testProperty "prop_eventsProof_run" $ prop_eventsProof_run @Keccak_256
        , testProperty "prop_eventsProof_run2" $ prop_eventsProof_run2 @Keccak_256
        , testProperty "fail_eventsProof_run2" $ fail_eventsProof_run2 @Keccak_256
        , testProperty "prop_eventsProof_subject" $ prop_eventsProof_subject @Keccak_256
        , testProperty "prop_eventsProof_valid" $ prop_eventsProof_valid @Keccak_256
        ]
    ]

prop_merkleProof_run :: MerkleHashAlgorithm a => MerkleProof a -> Bool
prop_merkleProof_run p = case runMerkleProof p of !_ -> True

prop_eventsProof_run
    :: forall a
    . MerkleHashAlgorithm a
    => Property
prop_eventsProof_run = forAll (arbitraryEventsProof @a) $ \p ->
    case runMerkleProof (_payloadProofBlob p) of !_ -> True

prop_eventsProof_run2
    :: forall a
    . MerkleHashAlgorithm a
    => Property
prop_eventsProof_run2 = forAll (arbitraryEventsProof @a) $ \p ->
    case runEventsProof p of
        Left e -> counterexample ("failed to validate proof: " <> show e) False
        Right (!_, !_) -> property True

fail_eventsProof_run2
    :: forall a
    . MerkleHashAlgorithm a
    => Property
fail_eventsProof_run2 = once $ expectFailure $ forAll (arbitraryEventsProof @a) $ \p ->
    case runOutputProof p of
        Left e -> counterexample ("failed to validate proof: " <> show e) False
        Right (!_, !_) -> property True

prop_eventsProof_subject
    :: forall a
    . MerkleHashAlgorithm a
    => BlockHeader
    -> Property
prop_eventsProof_subject hdr = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx -> case run (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (subject, BlockEvents _eh outputEvents) ->
                subject === (outputEvents V.! idx)
      where
        s = V.length (_payloadWithOutputsTransactions p)
        run reqKey = do
            (!_, !subject) <- runEventsProof (mkTestEventsProof @a p hdr reqKey)
            events <- getBlockEvents @_ @a p
            return (subject, events)

prop_eventsProof_valid
    :: forall a
    . MerkleHashAlgorithm a
    => BlockHeader
    -> Property
prop_eventsProof_valid hdr = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx -> case run (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (rootHash, subject, BlockEvents eh outputEvents) ->
                subject === (outputEvents V.! idx) .&. rootHash === eh
      where
        s = V.length (_payloadWithOutputsTransactions p)
        run reqKey = do
            (!rootHash, !subject) <- runEventsProof (mkTestEventsProof @a p hdr reqKey)
            events <- getBlockEvents @_ @a p
            return (rootHash, subject, events)
