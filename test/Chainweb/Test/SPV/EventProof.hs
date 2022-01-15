{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
(
  tests

-- * Properties
, properties
, PactEventDecimal(..)
, PactEventModRef(..)
, isSupportedModRef

-- * Test Cases
, testCases
) where

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Aeson.QQ.Simple
import Data.Aeson.Types
import Data.Bifunctor
import Data.Decimal
import Data.MerkleLog
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Types.Info
import Pact.Types.Runtime hiding (fromText)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.SPV.EventProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.Test.Orphans.Internal
import Chainweb.Utils hiding ((==>))

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests =  testGroup "Chainweb.Test.SPV.EventProof"
    [ properties
    , testCases
    ]

-- -------------------------------------------------------------------------- --
-- Properties

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "int256"
        [ testProperty "int256_maxBound" prop_int256_maxBound
        , testProperty "int256_minBound" prop_int256_minBound
        , testProperty "int256" prop_int256
        , testProperty "int256_outOfBounds_maxBound" prop_int256_outOfBounds_maxBound
        , testProperty "int256_outOfBounds_minBound" prop_int256_outOfBounds_minBound
        ]
    , testGroup "decimal"
        [ testProperty "supportedDecimal" prop_supportedDecimal
        , testProperty "decimal" prop_decimal
        ]
    , proof_properties
    ]

-- -------------------------------------------------------------------------- --
-- Supported Decimals

newtype PactEventDecimal = PactEventDecimal { _getPactEventDecimal :: Decimal }
    deriving (Show, Eq, Ord)

instance Arbitrary PactEventDecimal where
    arbitrary = fmap PactEventDecimal $ Decimal
        <$> choose (0, 18)
        <*> arbitrary

prop_supportedDecimal :: Decimal -> Property
prop_supportedDecimal d
    | isSupportedDecimal d = roundtripped === Right d
    | otherwise = roundtripped === Left (DecimalOutOfBoundsException d)
  where
    roundtripped = case try $ integerToDecimal <$> decimalToInteger d of
        Left e -> error (show e)
        Right x -> x

prop_decimal :: PactEventDecimal -> Property
prop_decimal (PactEventDecimal d)
    = (integerToDecimal <$> decimalToInteger d) === Just d

-- -------------------------------------------------------------------------- --
-- Pact Event ModRef

newtype PactEventModRef = PactEventModRef { _getPactEventModRef :: ModRef }
    deriving (Show, Eq, Ord)

instance Arbitrary PactEventModRef where
    arbitrary = fmap PactEventModRef $ ModRef
        <$> arbitrary
        <*> pure Nothing
        <*> pure (Info Nothing)

isSupportedModRef :: ModRef -> Bool
isSupportedModRef (ModRef _ Nothing (Info Nothing)) = True
isSupportedModRef _ = False

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
    first fromException (int256 x) === Left (Just (IntegerOutOfBoundsException x))
  where
    x = int256ToInteger maxBound + 1

prop_int256_outOfBounds_minBound :: Property
prop_int256_outOfBounds_minBound =
    first fromException (int256 x) === Left (Just (IntegerOutOfBoundsException x))
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
    , testGroup "Keccak_256"
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
    => Property
prop_eventsProof_subject = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx -> case run (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (subject, BlockEvents _eh outputEvents) ->
                subject === (outputEvents V.! idx)
      where
        s = V.length (_payloadWithOutputsTransactions p)
        run reqKey = do
            (!_, !subject) <- runEventsProof (mkTestEventsProof @a p reqKey)
            events <- getBlockEvents @_ @a p
            return (subject, events)

prop_eventsProof_valid
    :: forall a
    . MerkleHashAlgorithm a
    => Property
prop_eventsProof_valid = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx -> case run (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (rootHash, subject, BlockEvents eh outputEvents) ->
                subject === (outputEvents V.! idx) .&. rootHash === eh
      where
        s = V.length (_payloadWithOutputsTransactions p)
        run reqKey = do
            (!rootHash, !subject) <- runEventsProof (mkTestEventsProof @a p reqKey)
            events <- getBlockEvents @_ @a p
            return (rootHash, subject, events)

-- -------------------------------------------------------------------------- --
-- Test Cases

testCases :: TestTree
testCases = testGroup "Test Cases"
    [ test0
    ]

test0 :: TestTree
test0 = testCase "eventProof_0" $ do
    proof <- fromValue @_ @(PayloadProof Keccak_256) eventProof_0
    (root, events) <- runEventsProof proof

    -- Check proof properties
    assertEqual "proof root equals expected value" expectedRoot root
    assertEqual "proof subject has correct number of events" expectedEventCount
        $ V.length (_outputEventsEvents events)
    assertEqual "proof subject has correct requestKey" expectedReqKey
        $ _outputEventsRequestKey events
    assertEqual "event 0 has correct event name" expectedEventName
        $ _eventName (_outputEventsEvents events V.! 0)
    assertEqual "event 0 has correct module name" expectedEventModule
        $ _eventModule (_outputEventsEvents events V.! 0)
    assertEqual "event 0 has correct module hash" expectedEventModuleHash
        $ _eventModuleHash (_outputEventsEvents events V.! 0)
    assertEqual "event 0 has correct number of parameters" expectedParamsCount
        $ length (_eventParams (_outputEventsEvents events V.! 0))
  where
    expectedRoot = fromJuste $ fromText "YjoxLRFXZcxOPRBX3cbDgvYiTriWSzfU4cxvtKDbjek"
    expectedReqKey = fromJuste $ decode "\"QiX2nIzoq21HoA7Hiq2Rjkx5Nl7jD-l4nUGOjsz8aPA\""
    expectedEventCount = 4
    expectedEventName = "TRANSFER"
    expectedEventModule = ModuleName "kpenny" (Just "kswap")
    expectedEventModuleHash = fromJuste $ decode "\"2VPgtQUE8OoIVNgNPa_sLVmWXaYac1pZrLwd4t1YwUM\""
    expectedParamsCount = 3

fromValue :: MonadThrow m => FromJSON a => Value -> m a
fromValue v = case parseEither parseJSON v of
    Left e -> throwM $ JsonDecodeException (T.pack e)
    Right x -> return x

eventProof_0 :: Value
eventProof_0 = [aesonQQ|
    {
      "subject": {
        "input": "ADAgAAAAQiX2nIzoq21HoA7Hiq2Rjkx5Nl7jD-l4nUGOjsz8aPAEAAAACAAAAFRSQU5TRkVSDAAAAGtzd2FwLmtwZW5ueSAAAADZU-C1BQTw6ghU2A09r-wtWZZdphpzWlmsvB3i3VjBQwMAAAAAQAAAADZmNTJjZTBkMWRmYzJmNzNlY2M4ZjE2MmFkODE1ZWZjMmM5NzY2YWI3NmIxYTQ4YzlhNjc4ZDhjZDc5ZGQ0MDIAKwAAAFRvWXViX1ZoeldHdWVEWVVwODBnVmVWTnZiVGdoaDA4dU9kVUJtMDZkcFkCAABAe6XwY4GWCgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAVFJBTlNGRVIJAAAAa3N3YXAueHl6IAAAAP3AV3e1ornnygmVlZcEYY61dPGffrku_zZpklQUCx75AwAAAABAAAAANmY1MmNlMGQxZGZjMmY3M2VjYzhmMTYyYWQ4MTVlZmMyYzk3NjZhYjc2YjFhNDhjOWE2NzhkOGNkNzlkZDQwMgArAAAAVG9ZdWJfVmh6V0d1ZURZVXA4MGdWZVZOdmJUZ2hoMDh1T2RVQm0wNmRwWQKAZ5fVFxAj49sAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAABNSU5UDAAAAGtzd2FwLnRva2VucyAAAAB6vJFqfTyW0fbYYSu1VSkKPDE8a-VkGeN67sH_V9xlJwMAAAAAFgAAAGtzd2FwLmtwZW5ueTprc3dhcC54eXoAQAAAADZmNTJjZTBkMWRmYzJmNzNlY2M4ZjE2MmFkODE1ZWZjMmM5NzY2YWI3NmIxYTQ4YzlhNjc4ZDhjZDc5ZGQ0MDICgOV-ibaj994DAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAAAAVVBEQVRFDgAAAGtzd2FwLmV4Y2hhbmdlIAAAACvNLqkjDRI58v4xbqbQJN-YFuqwTim7Mq2x9LUm_ZJFAwAAAAAWAAAAa3N3YXAua3Blbm55Omtzd2FwLnh5egJw0jUOpI8jq0lWwksAAAAAAAAAAAAAAAAAAAAAAAAAAAKgAuLs2Q3nkQNZJQYAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      },
      "algorithm": "Keccak_256",
      "object": "AAAAAAAAAAAAAAAA",
      "rootType": "blockEvents"
    }
|]

