{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.BlockHeader.Validation
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Unit tests for BlockHeader validation rules
--
module Chainweb.Test.BlockHeader.Validation
( tests
) where

import Control.Exception (throw)
import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader.Internal
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.ForkState
import Chainweb.Graph hiding (AdjacentChainMismatch)
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils.TestHeader
import Chainweb.Time
import Chainweb.Utils hiding ((==>))
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Testnet04
import Control.Lens hiding ((.=), elements)
import Control.Monad.Catch
import Data.Aeson
import Data.Bits
import Data.ByteString qualified as B
import Data.DoubleWord
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.List qualified as L
import Data.Ratio
import Data.Text qualified as T
import Numeric.AffineSpace
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- -------------------------------------------------------------------------- --
-- Properties

tests :: TestTree
tests = testGroup "Chainweb.Test.Blockheader.Validation"
    [ prop_validateMainnet
    , prop_validateTestnet04
    , prop_fail_validate
    , prop_forkstate_validate
    , prop_da_validate
    , prop_legacy_da_validate
    , prop_forkVotesReset (barebonesTestVersion petersenChainGraph) 0
    , prop_forkVotesReset (barebonesTestVersion petersenChainGraph) (int forkEpochLength)
    , testProperty "validate arbitrary test header" prop_validateArbitrary
    , testProperty "validate arbitrary test header for mainnet" $ prop_validateArbitrary Mainnet01
    , testProperty "validate arbitrary test header for testnet04" $ prop_validateArbitrary Testnet04
    , testProperty "validate arbitrary test header for devnet" $ prop_validateArbitrary RecapDevelopment
    ]

-- -------------------------------------------------------------------------- --
-- Rules are not trivial
--
-- There is an input for which the rule fails.
--

prop_forkVotesReset :: ChainwebVersion -> BlockHeight -> TestTree
prop_forkVotesReset v h = testCase ("Invalid fork votes fail validation for " <> sshow v) $ do
    hdr <- (blockHeight .~ h)
        . (blockForkVotes .~ fromJuste (decode "2"))
        . (blockChainwebVersion .~ _versionCode v)
        <$> generate arbitrary
    let r = prop_block_forkVotesReset hdr
    assertBool
        ("fork votes validation succeeded unexpectedly: " <> sshow  hdr)
        (not r)

-- -------------------------------------------------------------------------- --
-- Rules are sound
--
-- The rule implements the correct property.
--
-- This is hard to test, since the rules themself are the respetive definition for
-- soundness of block headers. Testing this essentially means to to implementing
-- the rule in anther way.

-- -------------------------------------------------------------------------- --
-- Rules are consistent
--
-- The rule succeeds for existing block headers from immutable chainweb versions.
--
-- Test rules with a representative set of existing block headers.
--
-- * Geneiss blocks
-- * mainnet01 history
-- * New minded blocks

prop_validateMainnet :: TestTree
prop_validateMainnet = prop_validateHeaders "validate Mainnet01 BlockHeaders" mainnet01Headers

prop_validateTestnet04 :: TestTree
prop_validateTestnet04 = prop_validateHeaders "validate Testnet04 BlockHeaders" testnet04Headers

prop_validateHeaders :: String -> [TestHeader] -> TestTree
prop_validateHeaders msg hdrs = testGroup msg $ do
    [ prop_validateHeader ("header " <> show @Int i) h | h <- hdrs | i <- [0..] ]

prop_validateHeader :: String -> TestHeader -> TestTree
prop_validateHeader msg h = testCase msg $ do
    now <- getCurrentTimeIntegral
    case validateBlockHeaderM now (testHeaderChainLookup h) (_testHeaderHdr h) of
        Right _ -> return ()
        Left errs -> assertFailure $ "Validation failed for BlockHeader: " <> sshow errs

-- -------------------------------------------------------------------------- --
-- Rules are applied
--
-- The rule is actually used.
--
-- Like “non-trivial” but called via higher level. This is non trivial, since
-- we have to create headers that are correct up to properites relevant to a
-- partiular rule. In paritcular the hashes (including proof of work hashes)
-- would have to be valid.
--

prop_fail_validate :: TestTree
prop_fail_validate = validate_cases "validate invalid BlockHeaders" validationFailures

prop_forkstate_validate :: TestTree
prop_forkstate_validate = validate_cases "fork state validation" forkValidation

prop_da_validate :: TestTree
prop_da_validate = validate_cases "difficulty adjustment validation" daValidation

prop_legacy_da_validate :: TestTree
prop_legacy_da_validate = validate_cases "legacy difficulty adjustment validation" legacyDaValidation

validate_cases :: String -> [(TestHeader, [ValidationFailureType])] -> TestTree
validate_cases msg testCases = testCase msg $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) $ zip [0 :: Int ..] testCases
  where
    f now (i, (h, expectedErrs))
        = try (validateBlockHeaderM now (testHeaderChainLookup h) (_testHeaderHdr h)) >>= \case
            Right _
                | null expectedErrs -> return ()
                | otherwise -> assertFailure $ "Validation of test case " <> sshow i <> " succeeded unexpectedly for validationFailures BlockHeader"
            Left ValidationFailure{ _validationFailureFailures = errs }
                | sort errs /= sort expectedErrs -> assertFailure
                    $ "Validation of test case " <> sshow i <> " failed with unexpected errors for BlockHeader"
                    <> ", expected: " <> sshow expectedErrs
                    <> ", actual: " <> sshow errs
                    <> ", hash: " <> sshow (view blockHash $ _testHeaderHdr h)
                    <> ", height: " <> sshow (view blockHeight $ _testHeaderHdr h)
                    <> ", header: " <> T.unpack (encodeToText $ ObjectEncoded $ _testHeaderHdr h)
                    <> ", epochStart: " <> sshow (view blockEpochStart $ _testHeaderHdr h)
                    <> ", forkNumber: " <> sshow (view blockForkNumber $ _testHeaderHdr h)
                    <> ", forkVotes: " <> sshow (view blockForkVotes $ _testHeaderHdr h)
                    <> ", isForkEpochStart: " <> sshow (isForkEpochStart (_testHeaderHdr h))
                    <> ", isForkCountBlock: " <> sshow (isForkCountBlock (_testHeaderHdr h))
                    <> ", parent header: " <> T.unpack (encodeToText $ ObjectEncoded $ view parentHeader $ _testHeaderParent h)
                    <> ", parent forkNumber: " <> sshow (view blockForkNumber $ view parentHeader $ _testHeaderParent h)
                    <> ", parent forkVotes: " <> sshow (view blockForkVotes $ view parentHeader $ _testHeaderParent h)
                | otherwise -> return ()

-- -------------------------------------------------------------------------- --
-- Tests for Rule Guards:
--
-- Required: there is an example in mainnet history where the guard is needed,
-- i.e. the guarded rule fails.
--
-- Triggers: rule is applied, consistent, and soud after guard triggered
-- (Equivalent to checking applied, consistent, sound + showing that trigger is
-- effective)

-- -------------------------------------------------------------------------- --
-- Validation of Arbitrary Test Headers

prop_validateArbitrary :: ChainwebVersion -> Property
prop_validateArbitrary v =
    forAll (elements $ toList $ chainIds v) $ \cid ->
        forAll (arbitraryTestHeader v cid) validateTestHeader

validateTestHeader :: TestHeader -> Property
validateTestHeader h = case try val of
    Right (Left ValidationFailure{ _validationFailureFailures = errs }) -> verify errs
    Right _ -> property True
    Left err -> throw err
  where
    now = add second $ _bct $ view blockCreationTime $ _testHeaderHdr h
    val = validateBlockHeaderM now (testHeaderChainLookup h) (_testHeaderHdr h)
    verify :: [ValidationFailureType] -> Property
    verify es = L.delete IncorrectPow es === []

-- -------------------------------------------------------------------------- --
-- Invalid Headers

validationFailures :: [(TestHeader, [ValidationFailureType])]
validationFailures =
    [ ( hdr & testHeaderHdr . blockCreationTime .~ BlockCreationTime (Time maxBound)
      , [IncorrectHash, IncorrectPow, BlockInTheFuture]
      )
    , ( hdr & testHeaderHdr . blockCreationTime %~ add second
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr & testHeaderHdr . blockHash .~ nullBlockHash
      , [IncorrectHash]
      )
    , ( hdr & testHeaderHdr . blockCreationTime .~ (view blockCreationTime . _parentHeader $ _testHeaderParent hdr)
      , [IncorrectHash, IncorrectPow, CreatedBeforeParent]
      )
    , ( hdr & testHeaderHdr . blockHash %~ messWords encodeBlockHash decodeBlockHash (flip complementBit 0)
      , [IncorrectHash]
      )
    , ( hdr & testHeaderHdr . blockTarget %~ messWords encodeHashTarget decodeHashTarget (flip complementBit 0)
      , [IncorrectHash, IncorrectPow, IncorrectTarget]
      )
    , ( hdr & testHeaderParent . coerced . blockHeight .~ 318359
      , [IncorrectHeight, IncorrectEpoch, IncorrectTarget]
      )
    , ( hdr & testHeaderHdr . blockParent %~ messWords encodeBlockHash decodeBlockHash (flip complementBit 0)
      , [MissingParent]
      )
    , ( hdr & testHeaderHdr . blockPayloadHash %~ messWords encodeBlockPayloadHash decodeBlockPayloadHash (flip complementBit 0)
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr & testHeaderHdr . blockEpochStart %~ add second
      , [IncorrectHash, IncorrectPow, IncorrectEpoch]
      )
    -- Nr. 10
    , ( hdr & testHeaderHdr . blockChainId .~ unsafeChainId 1
      , [IncorrectHash, IncorrectPow, ChainMismatch, AdjacentChainMismatch]
      )
    , ( hdr & testHeaderHdr . blockChainwebVersion .~ _versionCode RecapDevelopment
      , [IncorrectHash, IncorrectPow, VersionMismatch, InvalidForkVotes, IncorrectForkNumber, CreatedBeforeParent, AdjacentChainMismatch, InvalidAdjacentVersion, InvalidForkVotes]
      )
    , ( hdr & testHeaderHdr . blockWeight .~ 10
      , [IncorrectHash, IncorrectPow, IncorrectWeight]
      )
    , ( hdr & testHeaderHdr . blockWeight %~ (+ 1)
      , [IncorrectHash, IncorrectPow, IncorrectWeight]
      )
    , ( hdr & testHeaderHdr . blockHeight .~ 10
      , [IncorrectHash, IncorrectPow, IncorrectHeight]
      )
    , ( hdr & testHeaderHdr . blockHeight %~ (+ 1)
      , [IncorrectHash, IncorrectPow, IncorrectHeight]
      )
    , ( hdr & testHeaderHdr . blockNonce .~ Nonce 0
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr & testHeaderHdr . blockNonce %~ Nonce . (+1) . encodeNonceToWord64
      , [IncorrectHash, IncorrectPow]
      )
    -- NOTE: The magic numbers in the following tests are directly related to
    -- the constant set in `skipFeatureFlagValidationGuard`.
    , ( hdr & testHeaderHdr . blockFlags .~ fromJuste (runGetS decodeForkState badFlags)
            & testHeaderHdr . blockHeight .~ 530499
      , [IncorrectHash, IncorrectPow, IncorrectHeight]
      )
    , ( hdr & testHeaderHdr . blockFlags .~ fromJuste (runGetS decodeForkState badFlags)
            & testHeaderHdr . blockHeight .~ 530500
      , [IncorrectHash, IncorrectPow, InvalidForkVotes, IncorrectForkNumber, IncorrectHeight, InvalidForkVotes]
      )
    -- Nr. 20
    , ( hdr & testHeaderHdr . blockAdjacentHashes .~ BlockHashRecord mempty
      , [IncorrectHash, IncorrectPow, AdjacentChainMismatch]
      )
    , ( hdr & testHeaderAdjs . each . parentHeader . blockChainwebVersion .~ _versionCode RecapDevelopment
      , [InvalidAdjacentVersion]
      )
    , ( hdr & testHeaderAdjs . ix 0 . parentHeader . blockChainId .~ unsafeChainId 0
      , [AdjacentParentChainMismatch]
      )
    ]
  where
    -- From mainnet
    hdr = testHeader
        [ "parent" .= t "AFHBANxHkLyt2kf7v54FAByxfFrR-pBP8iMLDNKO0SSt-ntTEh1IVT2E4mSPkq02AwACAAAAfaGIEe7a-wGT8OdEXz9RvlzJVkJgmEPmzk42bzjQOi0GAAAAjFsgdB2riCtIs0j40vovGGfcFIZmKPnxEXEekcV28eUIAAAAQcKA2py0L5t1Z1u833Z93V5N4hoKv_7-ZejC_QKTCzTtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAALFMJ1gcC8oKW90MW2xY07gN10bM2-GvdC7fDvKDDwAPBwAAAJkPwMVeS7ZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAT3hhzb-eBQAAAGFSDbQAAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-"
        , "header" .=  t "AEbpAIzqpiins1r8v54FAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-AwACAAAAy7QSAHoIeFj0JXide_co-OaEzzYWbeZhAfphXI8-IR0GAAAAa-PzO_zUmk1yLOyt2kD3iI6cehKqQ_KdK8D6qZ-X6X4IAAAA79Vw2kqbVDHm9WDzksFwxZcmx5OJJNW-ge7jVa3HiHbtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAAL701u70FOrdivm6quNUsKgfi2L8zYHeyOI0j2gfP16jBwAAANz0ZdfSwLZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOtsEAAAAAAAFAAAAT3hhzb-eBQAAAPvI7fkAAFFuYkCHZRcNl1k3-A1EZvyPxhiFKdHZwZRTqos57aiO"
        , "adjacents" .=
            [ t "ACcMAPA_ii9z0Ez7v54FAEHCgNqctC-bdWdbvN92fd1eTeIaCr_-_mXowv0Ckws0AwADAAAAxnGpa89fzxURJdpCA92MZmlDtgG9AZFVPCsCwNyDly8HAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTYJAAAAzmf29gDZjNcpxkw3EP9JgnU3-ARNJ14NisscofzzARCjTKbuwLbdyjay0MQ3l7xPGULH_yLMDPh4LQIAAAAAADbjm8GoWvx_3YNJ47vz54_LXV95MTKI4drB2fk5AdPlCAAAADS2qD13VlhnAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA0wwnzb-eBQAAAAnWry0AAO_VcNpKm1Qx5vVg85LBcMWXJseTiSTVvoHu41Wtx4h2"
            , t "AAARACjupkPfZFz8v54FAIxbIHQdq4grSLNI-NL6Lxhn3BSGZij58RFxHpHFdvHlAwABAAAAfdHDK_Q8xoD-W0nBPPBPMOgs1VukuCImYwCNnaBUwOMFAAAA4eefM0SUltzJ0Qszo3N0R9B4w_ap2_M2e6nlKEqJmkoHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbiMNKcS7VzGITdCwrGSYWrFNQvGP7KAzjbLQIAAAAAABlK0LefdM1J4t_Qeg6xAVNNDKEOhiEmNKe6SK9N6TAZBgAAALFBPU7YDgRoAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA2eJ0y7-eBQAAAORH5OUAAGvj8zv81JpNcizsrdpA94iOnHoSqkPynSvA-qmfl-l-"
            , t "AABPAIw5kEeHUtD7v54FAH2hiBHu2vsBk_DnRF8_Ub5cyVZCYJhD5s5ONm840DotAwAAAAAAPYZZ2yg5iXsMOyKqKKUhrGaboexUhUVK8e-fhn3FzNkEAAAAu_A9WCeRoLM17g_jc0A2UnhvCQFe5LCtTnaze9LqajQHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbP1aVtUvTRaiRyg9hCVSPXuIpf3IjuwHaBKgIAAAAAABQWMBli4UbscIslyPPH2ItcNaY2_Fm7yFucQM86oqojAgAAAFy5ttLGN19tAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAddFMzL-eBQAAALuSPmYAAMu0EgB6CHhY9CV4nXv3KPjmhM82Fm3mYQH6YVyPPiEd"
            ]
        ]

    badFlags = B.pack [0,0,0,0,0,0,0,1]

    messByteString :: (a -> Put) -> Get a -> (B.ByteString -> B.ByteString) -> a -> a
    messByteString enc dec f x = fromJuste $ runGetS dec $ f $ runPutS $ enc x

    messWords :: (a -> Put) -> Get a -> (Word256 -> Word256) -> a -> a
    messWords enc dec f = messByteString enc dec g
      where
        g bytes = runPutS (encodeWordLe $ f $ fromJuste $ runGetS decodeWordLe bytes)

-- -------------------------------------------------------------------------- --
-- Fork Validation


forkValidation :: [(TestHeader, [ValidationFailureType])]
forkValidation =
    -- while skipFeatureFlagValidationGuard is active (validation is trivially
    -- true)
    [ ( hdr0 & h . blockForkNumber %~ (+1)
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr0 & h . blockForkVotes %~ addVote
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr0 & h . blockForkVotes %~ (addVote . addVote)
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr0 & h . blockForkVotes %~ (+1)
      , [IncorrectHash, IncorrectPow]
      )

    -- after skipFeatureFlagValidationGuard is deactivated
    , ( hdr & h . blockForkNumber %~ (+1)
      , [IncorrectHash, IncorrectPow, IncorrectForkNumber]
      )
    , ( hdr & h . blockForkVotes %~ addVote
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr & h . blockForkVotes %~ (+1)
      , [IncorrectHash, IncorrectPow, InvalidForkVotes, InvalidForkVotes]
      )
    , ( hdr & h . blockForkVotes %~ (addVote . addVote)
      , [IncorrectHash, IncorrectPow, InvalidForkVotes, InvalidForkVotes]
      )
    , ( hdr
        & p . blockForkVotes .~ addVote resetVotes
        & h . blockForkVotes .~ addVote resetVotes
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr
        & p . blockForkVotes .~ addVote resetVotes
        & h . blockForkVotes .~ addVote (addVote resetVotes)
      , [IncorrectHash, IncorrectPow]
      )
    -- test 10:
    , ( hdr
        & p . blockForkVotes .~ addVote (addVote resetVotes)
        & h . blockForkVotes .~ resetVotes
      , [ {- no change to h -} InvalidForkVotes, InvalidForkVotes ]
      )
    , ( hdr
        & p . blockForkVotes .~ addVote (addVote resetVotes)
        & h . blockForkVotes .~ addVote (addVote resetVotes) - 1
      , [IncorrectHash, IncorrectPow, InvalidForkVotes, InvalidForkVotes]
      )
    , ( hdr
        & p . blockForkNumber .~ 10
        & h . blockForkNumber .~ 10
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr
        & p . blockForkNumber .~ 10
        & h . blockForkNumber .~ 10 - 1
      , [IncorrectHash, IncorrectPow, IncorrectForkNumber]
      )

    -- fork vote count, fork epoch end
    , ( hdr1
            & h . blockForkVotes %~ addVote
      , [ IncorrectHash
        , IncorrectPow
        , InvalidForkVotes -- invalid vote count in DAG indicution
        ]
      )
    , ( hdr1
            & p . blockForkVotes .~ addVote resetVotes
            & h . blockForkVotes .~ addVote resetVotes
      , [ IncorrectHash
        , IncorrectPow
        , InvalidForkVotes -- invalid vote count in DAG indicution
        ]
      )
    , ( hdr1
            & p . blockForkVotes .~ addVote resetVotes
            & a . blockForkVotes .~ addVote resetVotes
            & h . blockForkVotes .~ addVote resetVotes
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr1
            & p . blockForkVotes .~ addVote resetVotes
            & testHeaderAdjs . ix 0 . parentHeader . blockForkVotes .~ addVote resetVotes
            & h . blockForkVotes .~ addVote resetVotes `quot` 2
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr1
            & p . blockForkVotes .~ addVote resetVotes
            & testHeaderAdjs . ix 0 . parentHeader . blockForkVotes .~ addVote resetVotes
            & testHeaderAdjs . ix 1 . parentHeader . blockForkVotes .~ addVote resetVotes
            & h . blockForkVotes .~ addVote resetVotes `quot` 2
      , [ IncorrectHash
        , IncorrectPow
        , InvalidForkVotes -- invalid vote count in DAG indicution
        ]
      )

    -- check fork vote reset at fork epoch start
    , ( hdr2
            & p . blockForkVotes %~ (addVote . addVote)
            & h . blockForkVotes .~ resetVotes
      , [{- votes are already zero in hdr1-}]
      )
    -- Test 20
    , ( hdr2
            & p . blockForkVotes %~ (addVote . addVote)
            & h . blockForkVotes .~ addVote resetVotes
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr2
            & p . blockForkVotes %~ (addVote . addVote)
            & h . blockForkVotes .~ addVote (addVote resetVotes)
      , [ IncorrectHash
        , IncorrectPow
        , InvalidForkVotes -- invalid reset at fork epoch start
        , InvalidForkVotes -- invalid vote count in linear induction
        , InvalidForkVotes -- invalid vote count in DAG indicution
        ]
      )
    , ( hdr2
            & p . blockForkVotes %~ (addVote . addVote)
            & h . blockForkVotes .~ addVote (addVote (addVote resetVotes))
      , [ IncorrectHash
        , IncorrectPow
        , InvalidForkVotes -- invalid reset at fork epoch start
        , InvalidForkVotes -- invalid vote count in linear induction
        , InvalidForkVotes -- invalid vote count in DAG indicution
        ]
      )

    -- test correct fork number increment at fork epoch start
    , ( hdr2
        & p . blockForkVotes .~ int forkEpochLength * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2
      , [IncorrectForkNumber]
      )
    , ( hdr2
        & p . blockForkVotes .~ int forkEpochLength * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2 + 1
      , [IncorrectHash, IncorrectPow]
      )
    -- Test 25
    , ( hdr2
        & p . blockForkVotes .~ (voteLength * 2 `quot` 3 - 1) * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2
      , []
      )
    , ( hdr2
        & p . blockForkVotes .~ (voteLength * 2 `quot` 3 - 1) * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2 + 1
      , [IncorrectHash, IncorrectPow, IncorrectForkNumber]
      )
    , ( hdr2
        & p . blockForkVotes .~ (voteLength * 2 `quot` 3 + 1) * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2 + 1
      , [IncorrectHash, IncorrectPow]
      )
    , ( hdr2
        & p . blockForkVotes .~ (voteLength * 2 `quot` 3 + 1) * voteStep
        & h . blockForkVotes .~ resetVotes
        & h . blockForkNumber .~ view (p . blockForkNumber) hdr2
      , [IncorrectForkNumber]
      )
    ]

  where
    h, p :: Lens' TestHeader BlockHeader
    h = testHeaderHdr
    p = testHeaderParent . parentHeader
    a = testHeaderAdjs . each . parentHeader

    -- The number of block heights within an fork epoch where voting happens.
    -- The remaining blocks are used to count votes.
    voteLength = int $ forkEpochLength - voteCountLength

    hdr0 = mainnet01Headers !! 21

    -- fork epoch end
    hdr1 = mainnet01Headers !! 22

    -- fork epoch start
    hdr2 = mainnet01Headers !! 23

    -- vote block
    hdr = mainnet01Headers !! 24

-- -------------------------------------------------------------------------- --
-- DA Validation

daValidation :: [(TestHeader, [ValidationFailureType])]
daValidation =
    -- test corret epoch transition
    [ ( hdr, expected)

    -- epoch transition with wrong epoch start time
    , ( hdr & h . blockEpochStart .~ EpochStartTime (second ^+. (hour ^+. epoch))
      , IncorrectEpoch : expected
      )
    -- test epoch transition with correct target adjustment (*2)
    , ( hdr & p . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 2 hour ^+. epoch)
            & a . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 2 hour ^+. epoch)
            & h . blockEpochStart .~ EpochStartTime (scaleTimeSpan @Int 2 hour ^+. epoch)
            & h . blockTarget . hashTarget .~ (view (p . blockTarget . hashTarget) hdr * 2)
            & h . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 3 hour ^+. epoch)
      , IncorrectWeight : expected
      )
    -- test epoch transition with correct target adjustment (/ 2)
    , ( hdr & p . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 30 minute  ^+. epoch)
            & a . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 30 minute ^+. epoch)
            & h . blockEpochStart .~ EpochStartTime (scaleTimeSpan @Int 30 minute ^+. epoch)
            & h . blockTarget . hashTarget .~ ceiling (view (p . blockTarget . hashTarget) hdr % 2)
            & h . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 3 hour ^+. epoch)
      , IncorrectWeight : expected
      )
    -- test epoch transition with incorrect target adjustment
    , ( hdr & p . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 30 minute  ^+. epoch)
            & h . blockEpochStart .~ EpochStartTime (scaleTimeSpan @Int 30 minute ^+. epoch)
            & h . blockTarget . hashTarget .~ view (p . blockTarget . hashTarget) hdr
            & h . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 3 hour ^+. epoch)
      , IncorrectTarget : expected
      )
    -- test epoch transition with incorrect target adjustment
    , ( hdr & p . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 30 minute  ^+. epoch)
            & h . blockEpochStart .~ EpochStartTime (scaleTimeSpan @Int 30 minute ^+. epoch)
            & h . blockTarget . hashTarget .~ (view (p . blockTarget . hashTarget) hdr * 2)
            & h . blockCreationTime .~ BlockCreationTime (scaleTimeSpan @Int 3 hour ^+. epoch)
      , IncorrectWeight : IncorrectTarget : expected
      )
    ]
  where
    h, p :: Lens' TestHeader BlockHeader
    h = testHeaderHdr
    p = testHeaderParent . parentHeader

    a = testHeaderAdjs . each . parentHeader

    expected = [IncorrectHash, IncorrectPow, AdjacentChainMismatch]

    -- From mainnet
    hdr = set (h . blockChainwebVersion) (_versionCode RecapDevelopment)
        $ set (h . blockFlags) (newForkState hdrAdjs (view testHeaderParent hdr') parentFork)
        $ set (h . blockHeight) 600000
        $ set (h . blockEpochStart) (EpochStartTime (hour ^+. epoch))
        $ set (h . blockTarget) (view (p . blockTarget) hdr')
        $ set (h . blockCreationTime) (BlockCreationTime (scaleTimeSpan @Int 2 hour ^+. epoch))

        $ set (p . blockChainwebVersion) (_versionCode RecapDevelopment)
        $ set (p . blockCreationTime) (BlockCreationTime (hour ^+. epoch))
        $ set (p . blockEpochStart) (EpochStartTime epoch)
        $ set (p . blockHeight) 599999

        $ set (a . blockChainwebVersion) (_versionCode RecapDevelopment)
        $ set (a . blockCreationTime) (BlockCreationTime (hour ^+. epoch))
        $ set (a . blockTarget) (view (p . blockTarget) hdr')
        $ set (a . blockEpochStart) (EpochStartTime epoch)
        $ hdr'

    hdrAdjs = HM.fromList
        $ (\x -> (view (parentHeader . blockChainId) x, x))
        <$> view testHeaderAdjs hdr'
    parentFork = view (p . blockForkNumber) hdr'

    -- it almost doesn't matter what is used here, because most of it is overwritten above
    --
    hdr' = testHeader
        [ "parent" .= t "AFHBANxHkLyt2kf7v54FAByxfFrR-pBP8iMLDNKO0SSt-ntTEh1IVT2E4mSPkq02AwACAAAAfaGIEe7a-wGT8OdEXz9RvlzJVkJgmEPmzk42bzjQOi0GAAAAjFsgdB2riCtIs0j40vovGGfcFIZmKPnxEXEekcV28eUIAAAAQcKA2py0L5t1Z1u833Z93V5N4hoKv_7-ZejC_QKTCzTtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAALFMJ1gcC8oKW90MW2xY07gN10bM2-GvdC7fDvKDDwAPBwAAAJkPwMVeS7ZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAT3hhzb-eBQAAAGFSDbQAAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-"
        , "header" .=  t "AEbpAIzqpiins1r8v54FAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-AwACAAAAy7QSAHoIeFj0JXide_co-OaEzzYWbeZhAfphXI8-IR0GAAAAa-PzO_zUmk1yLOyt2kD3iI6cehKqQ_KdK8D6qZ-X6X4IAAAA79Vw2kqbVDHm9WDzksFwxZcmx5OJJNW-ge7jVa3HiHbtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAAL701u70FOrdivm6quNUsKgfi2L8zYHeyOI0j2gfP16jBwAAANz0ZdfSwLZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOtsEAAAAAAAFAAAAT3hhzb-eBQAAAPvI7fkAAFFuYkCHZRcNl1k3-A1EZvyPxhiFKdHZwZRTqos57aiO"
        , "adjacents" .=
            [ t "ACcMAPA_ii9z0Ez7v54FAEHCgNqctC-bdWdbvN92fd1eTeIaCr_-_mXowv0Ckws0AwADAAAAxnGpa89fzxURJdpCA92MZmlDtgG9AZFVPCsCwNyDly8HAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTYJAAAAzmf29gDZjNcpxkw3EP9JgnU3-ARNJ14NisscofzzARCjTKbuwLbdyjay0MQ3l7xPGULH_yLMDPh4LQIAAAAAADbjm8GoWvx_3YNJ47vz54_LXV95MTKI4drB2fk5AdPlCAAAADS2qD13VlhnAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA0wwnzb-eBQAAAAnWry0AAO_VcNpKm1Qx5vVg85LBcMWXJseTiSTVvoHu41Wtx4h2"
            , t "AAARACjupkPfZFz8v54FAIxbIHQdq4grSLNI-NL6Lxhn3BSGZij58RFxHpHFdvHlAwABAAAAfdHDK_Q8xoD-W0nBPPBPMOgs1VukuCImYwCNnaBUwOMFAAAA4eefM0SUltzJ0Qszo3N0R9B4w_ap2_M2e6nlKEqJmkoHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbiMNKcS7VzGITdCwrGSYWrFNQvGP7KAzjbLQIAAAAAABlK0LefdM1J4t_Qeg6xAVNNDKEOhiEmNKe6SK9N6TAZBgAAALFBPU7YDgRoAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA2eJ0y7-eBQAAAORH5OUAAGvj8zv81JpNcizsrdpA94iOnHoSqkPynSvA-qmfl-l-"
            , t "AABPAIw5kEeHUtD7v54FAH2hiBHu2vsBk_DnRF8_Ub5cyVZCYJhD5s5ONm840DotAwAAAAAAPYZZ2yg5iXsMOyKqKKUhrGaboexUhUVK8e-fhn3FzNkEAAAAu_A9WCeRoLM17g_jc0A2UnhvCQFe5LCtTnaze9LqajQHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbP1aVtUvTRaiRyg9hCVSPXuIpf3IjuwHaBKgIAAAAAABQWMBli4UbscIslyPPH2ItcNaY2_Fm7yFucQM86oqojAgAAAFy5ttLGN19tAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAddFMzL-eBQAAALuSPmYAAMu0EgB6CHhY9CV4nXv3KPjmhM82Fm3mYQH6YVyPPiEd"
            ]
        ]

legacyDaValidation :: [(TestHeader, [ValidationFailureType])]
legacyDaValidation =
    [ (hdr, [])
    , ( hdr & h . blockTarget . hashTarget %~ succ
      , [IncorrectHash, IncorrectPow, IncorrectWeight, IncorrectTarget]
      )
    , ( hdr & p . blockEpochStart .~ EpochStartTime epoch
      , [IncorrectTarget]
      )
    , ( hdr & p . blockCreationTime .~ BlockCreationTime epoch
      , [IncorrectTarget, IncorrectEpoch]
      )
    ]
  where
    h, p :: Lens' TestHeader BlockHeader
    h = testHeaderHdr
    p = testHeaderParent . parentHeader

    -- From mainnet height 600000
    hdr = testHeader
        [ "header" .= t "AAAAAAAAAAA_z6O3cKYFALWkfYX5u1mHlFtc6m2WzkGyfpxOiulPgdj213nd8hNCAwACAAAAKNL9BjFylWotmGlC3rla-zHjLWZCDGG47may6L1ae9kDAAAA7_-rMA7UiQz_XeraRer-t8P2e_8-Re9HI1RaWMmAyQAFAAAArdgCPhV4s40pYFtd33MfIvT_aNmSQC8BAl7Kszbi_m3ompSQKQSQuVbBqJHsmwTQvJlyxtaXrHyzZAAAAAAAAOtnvFu0Pe4bQU_3jXijQkKuxRQkwpmROWLrEcU4EmB3AAAAAP4UmaUADZnXBwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwCcJAAAAAAAFAAAAomkWtXCmBQABfTg_apMwAEiTnEtTDSfsw05RrfAUyla41lBEhgeIvc01oE_HRwWw"
        , "parent" .= t "AAAAAAAAAACiaRa1cKYFAFmK5kokRr2Fr1ThRJgyGZMDeYdjtR39rDLQCJTCCRLAAwACAAAAqop56-rmaZlP3cJ8ovBMFh0b13YKJKusxDQ6e-rY5qYDAAAA6OdCZnblItB_EHfWRU8HoVTTa8JCG3bcsfHqSCPuLXkFAAAAiSc1iYB55Z_WBNNNbXbBYxorn4UE5e9To3pkGAyFVg1nyi1HTRbA2OW66_x5xU7Jgj8tiqTiDb5WZAAAAAAAAPoy4IlkdUrT13QLpkmjs-O_Tfcdin9_XxN5p5QYNzDzAAAAAC6_SZU0gpbXBwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAvycJAAAAAAAFAAAAim-83W-mBQAQDFzNxfwJALWkfYX5u1mHlFtc6m2WzkGyfpxOiulPgdj213nd8hNC"
        , "adjacents" .=
            [ t "AAAAAAAAAADGdEe1cKYFAIknNYmAeeWf1gTTTW12wWMaK5-FBOXvU6N6ZBgMhVYNAwAAAAAAWYrmSiRGvYWvVOFEmDIZkwN5h2O1Hf2sMtAIlMIJEsAGAAAAoMmMNHSLx2wpeeJn8lXRAQZub-WogHiPaGYjgy0qIiYJAAAAWxaxAJCLTuupkbNvOep8LDFEGk-6m6w9C-BSUX3aMztULiyLjgVYj-tJ3mh0jTliCJ8-ae90BDymawAAAAAAAL2hV48ZKfa-SbrWH9tHcDW9Oj70qbf2d1LPyqhX13K0BQAAACvhOnkx9qxiBwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAvycJAAAAAAAFAAAA2_xh3G-mBQAAVugAKTcyAK3YAj4VeLONKWBbXd9zHyL0_2jZkkAvAQJeyrM24v5t"
            , t "AAAAAAAAAAB9T2i0cKYFAKqKeevq5mmZT93CfKLwTBYdG9d2CiSrrMQ0Onvq2OamAwAAAAAAWYrmSiRGvYWvVOFEmDIZkwN5h2O1Hf2sMtAIlMIJEsAEAAAAh7BgJ4wxjG-OKGOgKPajOT5ywh1l29syB19A9B-mdbsHAAAAKZlUzTfZQBCLI5MTAWzW0mvOORYiQ_btd49N3CWomCBUJiHnZBlRe74hC19btnFxBdWan7D-C04UYgAAAAAAAIyzoodCtQ9Kdczg9pzXeqVPncAn6WgmscfupIb-n-bbAgAAAIiXRcBcsif7BwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAvycJAAAAAAAFAAAAUjDf3G-mBQADAeXeuu4hAyjS_QYxcpVqLZhpQt65Wvsx4y1mQgxhuO5msui9WnvZ"
            , t "AAAAAAAAAAAaeyy1cKYFAOjnQmZ25SLQfxB31kVPB6FU02vCQht23LHx6kgj7i15AwAAAAAAWYrmSiRGvYWvVOFEmDIZkwN5h2O1Hf2sMtAIlMIJEsABAAAAV6d6LbiVwJ28s-N5uMmVpXxGEA-CwWMBCgiH16XGZcQIAAAAYB68abZbinHjE2RW-F_imNz8YelXj5KRsKy0CoX3bJWPPdqLJ59WXprzBoRWm317ioRu1dW-vP9ZYgAAAAAAAI5y7btLk5ykQLaB9x0OGNEvcScbtsagoPN2F7bgbx23AwAAAJ3kLThfYhvcBwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAvycJAAAAAAAFAAAAeMMA3G-mBQAJgYTO0ebsAO__qzAO1IkM_13q2kXq_rfD9nv_PkXvRyNUWljJgMkA"
            ]
        ]

-- -------------------------------------------------------------------------- --
-- A representative collection of Mainnet01 Headers pairs

-- | A representative collection of BlockHeader from the existing mainnet01
-- history.
--
-- The first 20 headers are the genesis headers for mainnet01.
--
-- On mainnet the skipFeatureFlagValidationGuard was turned off at height
-- 530_500.
--
mainnet01Headers :: [TestHeader]
mainnet01Headers = genesisTestHeaders Mainnet01 <>

    -- 20: height 318266, skipFeatureFlagValidationGuard active, features used as
    -- nonce
    [ testHeader
        [ "parent" .= t "AFHBANxHkLyt2kf7v54FAByxfFrR-pBP8iMLDNKO0SSt-ntTEh1IVT2E4mSPkq02AwACAAAAfaGIEe7a-wGT8OdEXz9RvlzJVkJgmEPmzk42bzjQOi0GAAAAjFsgdB2riCtIs0j40vovGGfcFIZmKPnxEXEekcV28eUIAAAAQcKA2py0L5t1Z1u833Z93V5N4hoKv_7-ZejC_QKTCzTtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAALFMJ1gcC8oKW90MW2xY07gN10bM2-GvdC7fDvKDDwAPBwAAAJkPwMVeS7ZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAT3hhzb-eBQAAAGFSDbQAAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-"
        , "header" .=  t "AEbpAIzqpiins1r8v54FAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-AwACAAAAy7QSAHoIeFj0JXide_co-OaEzzYWbeZhAfphXI8-IR0GAAAAa-PzO_zUmk1yLOyt2kD3iI6cehKqQ_KdK8D6qZ-X6X4IAAAA79Vw2kqbVDHm9WDzksFwxZcmx5OJJNW-ge7jVa3HiHbtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAAL701u70FOrdivm6quNUsKgfi2L8zYHeyOI0j2gfP16jBwAAANz0ZdfSwLZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOtsEAAAAAAAFAAAAT3hhzb-eBQAAAPvI7fkAAFFuYkCHZRcNl1k3-A1EZvyPxhiFKdHZwZRTqos57aiO"
        , "adjacents" .=
            [ t "ACcMAPA_ii9z0Ez7v54FAEHCgNqctC-bdWdbvN92fd1eTeIaCr_-_mXowv0Ckws0AwADAAAAxnGpa89fzxURJdpCA92MZmlDtgG9AZFVPCsCwNyDly8HAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTYJAAAAzmf29gDZjNcpxkw3EP9JgnU3-ARNJ14NisscofzzARCjTKbuwLbdyjay0MQ3l7xPGULH_yLMDPh4LQIAAAAAADbjm8GoWvx_3YNJ47vz54_LXV95MTKI4drB2fk5AdPlCAAAADS2qD13VlhnAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA0wwnzb-eBQAAAAnWry0AAO_VcNpKm1Qx5vVg85LBcMWXJseTiSTVvoHu41Wtx4h2"
            , t "AAARACjupkPfZFz8v54FAIxbIHQdq4grSLNI-NL6Lxhn3BSGZij58RFxHpHFdvHlAwABAAAAfdHDK_Q8xoD-W0nBPPBPMOgs1VukuCImYwCNnaBUwOMFAAAA4eefM0SUltzJ0Qszo3N0R9B4w_ap2_M2e6nlKEqJmkoHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbiMNKcS7VzGITdCwrGSYWrFNQvGP7KAzjbLQIAAAAAABlK0LefdM1J4t_Qeg6xAVNNDKEOhiEmNKe6SK9N6TAZBgAAALFBPU7YDgRoAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA2eJ0y7-eBQAAAORH5OUAAGvj8zv81JpNcizsrdpA94iOnHoSqkPynSvA-qmfl-l-"
            , t "AABPAIw5kEeHUtD7v54FAH2hiBHu2vsBk_DnRF8_Ub5cyVZCYJhD5s5ONm840DotAwAAAAAAPYZZ2yg5iXsMOyKqKKUhrGaboexUhUVK8e-fhn3FzNkEAAAAu_A9WCeRoLM17g_jc0A2UnhvCQFe5LCtTnaze9LqajQHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbP1aVtUvTRaiRyg9hCVSPXuIpf3IjuwHaBKgIAAAAAABQWMBli4UbscIslyPPH2ItcNaY2_Fm7yFucQM86oqojAgAAAFy5ttLGN19tAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAddFMzL-eBQAAALuSPmYAAMu0EgB6CHhY9CV4nXv3KPjmhM82Fm3mYQH6YVyPPiEd"
            ]
        ]

    -- 21: height 475492, skipFeatureFlagValidationGuard active, features flags 0
    , testHeader
        [ "parent" .= t "AAAAAAAAAABEtTTKCqMFAEjmr_NPBprFiWD_WhyvMzQCiHGxnE1sYKeKGTOPjvD5AwACAAAAMrWEo-w-oixyUqELYmgOvRU7Z7FTjPzNLJCYXu26OuIDAAAA4c_RjHJ0N4gH6uN3TLpowhFIUGFRUe1celP6BwyFBwAFAAAAF2p4lSHRyyPrQ8GF1akfJM9EfzYSSsx5NI5IHtavNXEYhLsC8eqcLrj_VXvO_p1n4hz5QVR4ul3FpwAAAAAAAPDGI9fJxK4qRkWzcPv64tL1wNu0j76Vws5_oXrHsrPxAAAAAGJcPGAFgqQkBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAx9MnbwqjBQAAybvnn0-KAPLMEAsBh7vLnOh9053meOcuahQ_ezuUWynN1sMwSWqQ"
        , "header" .= t "AAAAAAAAAACnKMTLCqMFAPLMEAsBh7vLnOh9053meOcuahQ_ezuUWynN1sMwSWqQAwACAAAA_5afAofRZMA5Lz_ZG1Y0l6PJFTWueyU_4GbGWGtt_aoDAAAAs4MViuWgDm1nCgvyE5kzgG-_eZhCJupijIoh2z9cy0MFAAAA760lZUnDPEzHB6SKPbfOhFjpNNYDCuUXfxjvO3W4AckYhLsC8eqcLrj_VXvO_p1n4hz5QVR4ul3FpwAAAAAAAEQxBl7xgdFiynPSwT5ZNOcH8fiWKTdX9j09CUDn2irbAAAAAGHDHxemCKYkBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAZEEHAAAAAAAFAAAAx9MnbwqjBQAdLDW8JkF8ALVfqYGwKAkKf4NHKGRn1autmwMCCgXM-ZHa2zwgRk3R"
        , "adjacents" .=
            [ t "AAAAAAAAAADjy2TKCqMFABdqeJUh0csj60PBhdWpHyTPRH82EkrMeTSOSB7WrzVxAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkGAAAAR8k4YCs8tUNx_xN-6pBWwK6ZsQM4aVqNdChpN59IDoQJAAAAHq0_GIZUoec-dF3DHYqzMyJmaOdGdnaw4FtuQuO2LOvjgZy5BaDXFN80S7hBeqGZ2cWTwRs5c-2BtgAAAAAAAMYc1v2Uw5BO6zQXlJ5l5oGsLo-rGkpweUWRftVgIkeHBQAAAP8ehKwH-DP2AwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAuHnmcQqjBQAAfJOEnJ3EAO-tJWVJwzxMxwekij23zoRY6TTWAwrlF38Y7zt1uAHJ"
            , t "AAAAAAAAAABpbsjKCqMFADK1hKPsPqIsclKhC2JoDr0VO2exU4z8zSyQmF7tujriAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkEAAAA9wHcfWI2MNTrJuoPxllY3pm7BVa4Zgr8Ojdufj4oLw4HAAAAe1EOiqoneVZjUWfvqItXpXBais2UiUeoqcvc32prVo4q7OJWUH7TcdN9wcJMzNuUmuZtySQqALShowAAAAAAACNm7rsELSjBjDerkGAB1JSp3Ink0NG0t7Wqupyi3cqBAgAAAHjeCa0pNJQ0BAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAf6OvbgqjBQAGddYi31ibAP-WnwKH0WTAOS8_2RtWNJejyRU1rnslP-Bmxlhrbf2q"
            , t "AAAAAAAAAABF8PnKCqMFAOHP0YxydDeIB-rjd0y6aMIRSFBhUVHtXHpT-gcMhQcAAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkBAAAA5fRcXeTuKnS5Q4DuydXASBIX29tMsytHjEP21Vf2Ah0IAAAAJvJibit6A2QiqrOZmRe9zJRjW7PtmLJ51hDMUh1UyX4MFPyyFPku-007fa_t4ax7z5uSCQnTRLz1pAAAAAAAAHkJOGZlsei0ZFfkYdc0bBM7I39OmrLZlJTegBfLHNSYAwAAAH1mw3DMvaAaBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAhQs5cAqjBQAAKCAqaKcvALODFYrloA5tZwoL8hOZM4Bvv3mYQibqYoyKIds_XMtD"
            ]
        ]

    -- 22: height 6321599, vote count block, fork epoch end
    , testHeader
        [ "parent" .= t "AAAAAAAAAABslEi7qUIGACj7AZW9Vkte1AgKrjuqRBTRcr3Mvy_FV0YkTSgz0N6zAwAFAAAAXEd0PM6l6XoBtNBTxhTNRsJbLn8L1MJ0e8IcG8h6TvkKAAAAp1YlI2HcbSlssAYZZTxJtIafQV2rSTdbkOzGPX_fRzQPAAAAvL2kzVpBlD2Kdzm9RRUUy5-ZqUTZO-gKiR_E_tFUyb-Mxa3IHfpOHmRmcuwyTdgPkOgvMYYryqVeAAAAAAAAAK229tCQqWjy2oLNSFGbjrcYanCcBmU5vlRDaSp5UwASAAAAAHcmyzVydPF-LFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAvnVgAAAAAAAFAAAAIU3u6KhCBgBEIkGRyb41k0rqtdJ81GDCMMFgxeIwMew9tO3hTljEOCKMNYLw1Vke"
        , "header" .= t "AAAAAAAAAADkbOK8qUIGAErqtdJ81GDCMMFgxeIwMew9tO3hTljEOCKMNYLw1VkeAwAFAAAAwcy63dPjpUIuDOxd9wo5xVSiZikajymn6a075O5LRn8KAAAAOwAo3wpfLlSGMyTIiHQDPWl66RL3PAl5Ocv7CalKAF0PAAAAzhn6nhsIZedkTXHI2OsdGX2ByWuP6zDH53nbYuhNGgqMxa3IHfpOHmRmcuwyTdgPkOgvMYYryqVeAAAAAAAAADZyExanIQE5DJE9ZB0F4nE0G2J7gY3V74d4QwAK2q_lAAAAAO4ITfE64KWBLFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3VgAAAAAAAFAAAAIU3u6KhCBgDWUeflZRYKoD2ha_3IfZl3nhigrGZG5PFKYt5vsGh6TY6bO10cZ6iV"
        , "adjacents" .=
            [ t "AAAAAAAAAAATjoy8qUIGAFxHdDzOpel6AbTQU8YUzUbCWy5_C9TCdHvCHBvIek75AwAAAAAAKPsBlb1WS17UCAquO6pEFNFyvcy_L8VXRiRNKDPQ3rMHAAAAtu24rFjWNvDdtSjeitACnjr2mmhbxhmU4VfGZ23d1VMIAAAAMkQooEWfaXJw1liX87P_kXeUuHmbgfJITV66WjnMZO3wbCKrZg5RLzyBTYJuIWukp4y3M4hN19peAAAAAAAAAGHafKXb-s3k5GEQRXNia_38QsY9joXVHh-5W2BUSpXbBQAAAA4Eb5aYPU65K1ICAAAAAAAAAAAAAAAAAAAAAAAAAAAAvnVgAAAAAAAFAAAAY5W_6ahCBgD_bA_L6YjLZ8HMut3T46VCLgzsXfcKOcVUomYpGo8pp-mtO-TuS0Z_"
            , t "AAAAAAAAAAAzq0W7qUIGAKdWJSNh3G0pbLAGGWU8SbSGn0Fdq0k3W5Dsxj1_30c0AwAAAAAAKPsBlb1WS17UCAquO6pEFNFyvcy_L8VXRiRNKDPQ3rMLAAAAClFapQDLHLRD3aAFqb2V7gzM0h1RmRud-4DpSr4JxkQTAAAAsh5AfwMyDvvAPDQ2jFEkX8ErSUtiFzclBV5XBYY2TyeU8z0Qi5oYmuyx_jEsazhgqZ5QzQNQq_heAAAAAAAAAAR6NoCC_zgY_CcrpVh6wmEIzMfY2h7zkST26Y8sKHY5CgAAAFUY6iybXTKPFFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAvnVgAAAAAAAFAAAAs64d6ahCBgA4piV-WN0XkjsAKN8KXy5UhjMkyIh0Az1peukS9zwJeTnL-wmpSgBd"
            , t "AAAAAAAAAADz6Sm7qUIGALy9pM1aQZQ9inc5vUUVFMufmalE2TvoCokfxP7RVMm_AwAAAAAAKPsBlb1WS17UCAquO6pEFNFyvcy_L8VXRiRNKDPQ3rMOAAAA_a6mCUpjlh-0u_ylDCgwZRTmoHge5Pt3Bn-8MK36S_cQAAAASLYNLxbMDBGNuSn_afQDcKUdg2N8d96jUxnZZR54HcoEoyymQsM6uk4gnlLMs6s1zUepHIh4685eAAAAAAAAAD5JuLycrXXJGsmc1KpNq-asCPXJQ8KsqYUIse4ZTreLDwAAAN60c_FW7eMGFFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAvnVgAAAAAAAFAAAA-B-U6KhCBgAzMCfK3uQOYs4Z-p4bCGXnZE1xyNjrHRl9gclrj-swx-d522LoTRoK"
            ]
        ]

    -- 23: height 6321600, vote block, fork epoch start
    , testHeader
        [ "parent" .= t "AAAAAAAAAADkbOK8qUIGAErqtdJ81GDCMMFgxeIwMew9tO3hTljEOCKMNYLw1VkeAwAFAAAAwcy63dPjpUIuDOxd9wo5xVSiZikajymn6a075O5LRn8KAAAAOwAo3wpfLlSGMyTIiHQDPWl66RL3PAl5Ocv7CalKAF0PAAAAzhn6nhsIZedkTXHI2OsdGX2ByWuP6zDH53nbYuhNGgqMxa3IHfpOHmRmcuwyTdgPkOgvMYYryqVeAAAAAAAAADZyExanIQE5DJE9ZB0F4nE0G2J7gY3V74d4QwAK2q_lAAAAAO4ITfE64KWBLFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3VgAAAAAAAFAAAAIU3u6KhCBgDWUeflZRYKoD2ha_3IfZl3nhigrGZG5PFKYt5vsGh6TY6bO10cZ6iV"
        , "header" .= t "AAAAAAAAAAANZku_qUIGAD2ha_3IfZl3nhigrGZG5PFKYt5vsGh6TY6bO10cZ6iVAwAFAAAAcsFm3g5DIqDtQb2mWPOAH_oInypK_Czr_TRCrp-2qS4KAAAAzMnWf0KX3FjRJN_iUaHZx2Yv7gv_UQxsJtOvIBVHpvEPAAAAXBFz4-qe0cj3a5zwYY1aSR2O783dbdfNpHqFcbh-PJ6B05Rjv8-pzEkaEvR-uVo2kh8UrTxnvfZdAAAAAAAAADYR5b0EHyNPZ7PGKPQUx-NoXscUeucl83_6Vs1L99HdAAAAAF8YsrX1VV-ELFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAwHVgAAAAAAAFAAAA5GzivKlCBgB2CSV3HSKdfPtROsut_7VjkhCH-E6QYcEUk1aJsUEC8C8RaG0DBY2_"
        , "adjacents" .=
            [ t "AAAAAAAAAABv_k69qUIGAMHMut3T46VCLgzsXfcKOcVUomYpGo8pp-mtO-TuS0Z_AwAAAAAASuq10nzUYMIwwWDF4jAx7D207eFOWMQ4Iow1gvDVWR4HAAAAg0lPrNDq4gGyqneJYv-cptgp-CW3etrxL_NdlVURGrMIAAAA2IuXh5kMcspjp4TvSRdNf5gEwbUYCXNqGImNV3p29nvwbCKrZg5RLzyBTYJuIWukp4y3M4hN19peAAAAAAAAAIo7s6lgENppqdsqxYgvNqgEBIvAVoHmOryceo4xuk-VBQAAAKPSYtIdJgG8K1ICAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3VgAAAAAAAFAAAAY5W_6ahCBgD9Zi-ZXW9KsnLBZt4OQyKg7UG9pljzgB_6CJ8qSvws6_00Qq6ftqku"
            , t "AAAAAAAAAACJGvO-qUIGADsAKN8KXy5UhjMkyIh0Az1peukS9zwJeTnL-wmpSgBdAwAAAAAASuq10nzUYMIwwWDF4jAx7D207eFOWMQ4Iow1gvDVWR4LAAAAL27uWSy88TsqeJ0ktSsvV6mMIk6e4AsgLL1ABku4XV8TAAAAm_UMpdSy3Ye0Ahk4dvFO7wa_MWiPDH0k7IjcICROMK-U8z0Qi5oYmuyx_jEsazhgqZ5QzQNQq_heAAAAAAAAAMq9Yfa1UfbrtlmJ1_IED4uads9BCDwJI-cqceenaNarCgAAACxwGh0hbeSRFFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3VgAAAAAAAFAAAAs64d6ahCBgAMpVdM4gBxeMzJ1n9Cl9xY0STf4lGh2cdmL-4L_1EMbCbTryAVR6bx"
            , t "AAAAAAAAAADN0se9qUIGAM4Z-p4bCGXnZE1xyNjrHRl9gclrj-swx-d522LoTRoKAwAAAAAASuq10nzUYMIwwWDF4jAx7D207eFOWMQ4Iow1gvDVWR4OAAAAkLFCpU1MfxXYVxNGyxGkoPvL9r76KH0GDqGrPFyV8TcQAAAAekjQVjq4DuPGDcB4X0EKOnP1XUMF-HWD5OPUn-rySwIEoyymQsM6uk4gnlLMs6s1zUepHIh4685eAAAAAAAAAHzCrp7_9mBJVUofOGGkHN09ZM8tdCV8eRzgixtNLsRfDwAAANyRDi28LJcJFFICAAAAAAAAAAAAAAAAAAAAAAAAAAAAv3VgAAAAAAAFAAAA-B-U6KhCBgDPdD2Lc4VTIlwRc-PqntHI92uc8GGNWkkdju_N3W3XzaR6hXG4fjye"
            ]
        ]

    -- 24: height 6326122, vote block
    , testHeader
        [ "parent" .= t "AAAAAAAAAAChAupjyUIGANiFdvMIWp_ju7l-_R_O-SOpHLGVNTYnbM7HgtZVecCpAwAFAAAATN-wBWveMrd2_NquzbvIufU0EvplxGDw10PU-s-32lYKAAAA46Z3xj6yOvP0Tzg50mQT9NC65sQwiyyOrbG72RjxqIMPAAAAZ2w6RhnbUVf5PCL_d4hy1f-8Xt2-baJ3lVj3ISgwVrRWVIRpxqk3nMbCaAs1N3MiJP4pbsS24FJmAAAAAAAAAKQE1ZBSu-cGZOYycv11Z00VVhINXQAzNwOD_3BHhwmgAAAAADBR1syGVX17WlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaYdgAAAAAAAFAAAAY-dC18hCBgBQJWBZhTZGPkrkjXAZksxOrJJ9_yWsGXmR2-7PLQXj-egE2MGADlVu"
        , "header" .= t "AAAAAAAAAAAqEbRkyUIGAErkjXAZksxOrJJ9_yWsGXmR2-7PLQXj-egE2MGADlVuAwAFAAAAvY7ZzQ25m2OF1xo_u4PdplvSNAyA7tD23JK80KEQyRoKAAAABG_JSLkgpikZcGUXKrgnra_mH6NtbOKLXAyY1SZtdEgPAAAAJj85k10xzWVEih9kGpvUp1cjxqXCGqbXXbRC4ftSnGlWVIRpxqk3nMbCaAs1N3MiJP4pbsS24FJmAAAAAAAAAFVRDdzh8cwftq1OCLznnEXtAWs23No8ieHfP_K9BS-KAAAAAAmv7Z2hz_19WlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaodgAAAAAAAFAAAAY-dC18hCBgBBIDIJPAHEKE8w1sP7q3j8x2w0NI3FVqqZdKa0DYQL6D3Qmn2M0jGt"
        , "adjacents" .=
            [ t "AAAAAAAAAAAwFJhkyUIGAEzfsAVr3jK3dvzars27yLn1NBL6ZcRg8NdD1PrPt9pWAwAAAAAA2IV28whan-O7uX79H875I6kcsZU1NidszseC1lV5wKkHAAAAL-fZuds4DKn039O6ZXTxXffmvDAgmouWwi3idWyJWwAIAAAABGjPCyThBGHt7newXOUaFOVC0KeM0FlXAukb6UQ3pS1h4c2sReizxZqlwtzy1Ke1pHYxpJy_WChmAAAAAAAAAKgz8jols_l9CsAqlNHCxUvHzm6frQfkE9fzyD3gXvb5BQAAAI2N6IFYWcq2WVICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaYdgAAAAAAAFAAAAKz1U18hCBgAlEE8VUX1LIr2O2c0NuZtjhdcaP7uD3aZb0jQMgO7Q9tySvNChEMka"
            , t "AAAAAAAAAABrauZiyUIGAOOmd8Y-sjrz9E84OdJkE_TQuubEMIssjq2xu9kY8aiDAwAAAAAA2IV28whan-O7uX79H875I6kcsZU1NidszseC1lV5wKkLAAAAP3-Mje9H9iv9FPFxdG2aXRtLmEBe-2mrJi2A4AwJxSkTAAAAzC_KPaKRpTZXuANLM-u6PmH0bnPYpdmn6pG6CmY5GJ_D7ZlqTba36lnJsTsIancoww7-80DB8kVmAAAAAAAAALDIcUfna6BtbTWuF5s2ildbsB3UGLcVWQG2UsJNbs5rCgAAACJ-sl77Nm2NQlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaYdgAAAAAAAFAAAAANBY2MhCBgB4IFWFDr2KnwRvyUi5IKYpGXBlFyq4J62v5h-jbWzii1wMmNUmbXRI"
            , t "AAAAAAAAAABeGaljyUIGAGdsOkYZ21FX-Twi_3eIctX_vF7dvm2id5VY9yEoMFa0AwAAAAAA2IV28whan-O7uX79H875I6kcsZU1NidszseC1lV5wKkOAAAAXZHxkQPHqwm4tYpBSKtc068oJPydEhjp_kentfvc49sQAAAA7x84Ch3gCLUY7osydZf9mNRzUPgfYITtd_KGZNwwkJA-B1a-ePDkmTuHTUlk5YeNf7clt73uz8BlAAAAAAAAAMnuCE80e-a4v564Ru86UnMFv34tFZVUaeEWJoByKMYMDwAAAPqsyZPVHeUCQlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaYdgAAAAAAAFAAAAagET1shCBgA7KC14h640jCY_OZNdMc1lRIofZBqb1KdXI8alwhqm1120QuH7Upxp"
            ]
        ]

    -- 25: height 6326123, vote block
    , testHeader
        [ "parent" .= t "AAAAAAAAAAAqEbRkyUIGAErkjXAZksxOrJJ9_yWsGXmR2-7PLQXj-egE2MGADlVuAwAFAAAAvY7ZzQ25m2OF1xo_u4PdplvSNAyA7tD23JK80KEQyRoKAAAABG_JSLkgpikZcGUXKrgnra_mH6NtbOKLXAyY1SZtdEgPAAAAJj85k10xzWVEih9kGpvUp1cjxqXCGqbXXbRC4ftSnGlWVIRpxqk3nMbCaAs1N3MiJP4pbsS24FJmAAAAAAAAAFVRDdzh8cwftq1OCLznnEXtAWs23No8ieHfP_K9BS-KAAAAAAmv7Z2hz_19WlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaodgAAAAAAAFAAAAY-dC18hCBgBBIDIJPAHEKE8w1sP7q3j8x2w0NI3FVqqZdKa0DYQL6D3Qmn2M0jGt"
        , "header" .= t "AAAAAAAAAAAGZmBnyUIGAE8w1sP7q3j8x2w0NI3FVqqZdKa0DYQL6D3Qmn2M0jGtAwAFAAAAaKJ8NbpwJLO9Y6Fw-BiSUa8FOsx_dQD8v9SW6tOrbHYKAAAATRwvZd-7F-HR4k98kMPncHsrhFMUPD0wH_Pi228v00APAAAAkEh_PW2APnFclf6FmFhS821y4adwnEFosG7L8_5LVYtWVIRpxqk3nMbCaAs1N3MiJP4pbsS24FJmAAAAAAAAAFzn3j7jZsY4kC4qg85hL3J-P-z7ayqopscHc5kmRru2AAAAAOIMBW-8SX6AWlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAa4dgAAAAAAAFAAAAY-dC18hCBgAFdTEo0KuoiKBa9IgIWTpPcM24qflo3ZTkzfJmX1Z-bu1bJ-4Za3XK"
        , "adjacents" .=
            [ t "AAAAAAAAAACz7Y9lyUIGAL2O2c0NuZtjhdcaP7uD3aZb0jQMgO7Q9tySvNChEMkaAwAAAAAASuSNcBmSzE6skn3_JawZeZHb7s8tBeP56ATYwYAOVW4HAAAAFdKo9zn5ql2pN5O3tZp0JWpbktfy6zojur-Q6VVCIvkIAAAA1TsrO72AI8U4TPjaQim713Qet9x8Rb8rqboJq3Gv0qlh4c2sReizxZqlwtzy1Ke1pHYxpJy_WChmAAAAAAAAAE97E83uN_ixiRiwt_E9Sj6yYbNLsxOHV3TbFfIR_zyPBQAAAPYf9WgZ3ku5WVICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaodgAAAAAAAFAAAAKz1U18hCBgA7KAlhBSHqRGiifDW6cCSzvWOhcPgYklGvBTrMf3UA_L_UlurTq2x2"
            , t "AAAAAAAAAAA4u3BlyUIGAARvyUi5IKYpGXBlFyq4J62v5h-jbWzii1wMmNUmbXRIAwAAAAAASuSNcBmSzE6skn3_JawZeZHb7s8tBeP56ATYwYAOVW4LAAAAmb8fwIXBTIazxlpIQFPytsVehpp84GRQOgj8PjlvGi8TAAAAZ70hnqbhX8hft05VvQV9N8iEYmnbriBrcefeQiHDkM3D7ZlqTba36lnJsTsIancoww7-80DB8kVmAAAAAAAAAO4MEV4v6lPPzg1h1TF7Nmd69n2o1GhpF5x8I89dmPZoCgAAABUhL4IOAu6PQlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaodgAAAAAAAFAAAAANBY2MhCBgDONC-15wdGgE0cL2Xfuxfh0eJPfJDD53B7K4RTFDw9MB_z4ttvL9NA"
            , t "AAAAAAAAAACBN_tkyUIGACY_OZNdMc1lRIofZBqb1KdXI8alwhqm1120QuH7UpxpAwAAAAAASuSNcBmSzE6skn3_JawZeZHb7s8tBeP56ATYwYAOVW4OAAAAfoLIDaWE5pbRBv9Y287JswmhksqwsJGs5a_KVWsVOpwQAAAAp62_wP5swyl6EVw_AUP3B6uf5dQ0ToiShK68bRlRI-Y-B1a-ePDkmTuHTUlk5YeNf7clt73uz8BlAAAAAAAAAIxiIpzB44QiabdOzLONhTENWBpFv3jmfnMy4VWIRrm_DwAAACF9xC1WL2kFQlICAAAAAAAAAAAAAAAAAAAAAAAAAAAAaodgAAAAAAAFAAAAagET1shCBgBIzBJfaqYQR5BIfz1tgD5xXJX-hZhYUvNtcuGncJxBaLBuy_P-S1WL"
            ]
        ]
    ]

testnet04Headers :: [TestHeader]
testnet04Headers = genesisTestHeaders Testnet04

-- TODO replace these by "interesting headers" form mainnet (e.g. fork block heights)
_testnet04InvalidHeaders :: [(ParentHeader, BlockHeader, [ValidationFailureType])]
_testnet04InvalidHeaders = some
  where
    f (p, h, e) = (,,)
        <$> (fmap ParentHeader . decode . wrap) p
        <*> (decode . wrap) h
        <*> pure e

    wrap x = "\"" <> x <> "\""

    some = fromJuste $ traverse f
        [
            ( "UV8kAAAAAADFZ2HwFKIFAJ8hgfl2LV_uh846M6v-KHp6ZI3zNOpeObct-K0miL3cAwACAAAAPtJw9_WRladZJhKDhIEx1WgjkofJISyqBnMNNvDfV84DAAAAWWbE_Gg8T20vfB1m5mLM0NllvjvgMRWL8xhCocLZY8YFAAAA7Xlzumy_WTzT1ROEHzzgWeO8n0s2Gf9zUUZCTFhy3qcdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAL6LzuBDgWv0dp05UPLapBb2H4Y7BjA_tIiHRTAiEvLsAAAAABvmlwZDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFkMEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzON"
            , "ajsXAAAAAAC-IonwFKIFALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzONAwACAAAAyGznxkEiOfo6OHftd86_8aA8B55QaQSiD7f5insUMB4DAAAAPKSjwRZc6V_SJW1AbBwZcnkZ1WbRdKrtN4BnJ0q1RpkFAAAAJv6AJtHEaBuFZpIpdePX7s2uN8n7QhFvNPkX70uZduMdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAAFCpo3PF6aH1bHhB27DlDGt8aiyip06XzMWRcgUEHi2AAAAACMLrgdDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAF0MEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALl0CaPBMj5a0QhuIKtJD6zYj1BxWuoKg-X8ASzosri7"
            , [IncorrectHash, IncorrectPow, InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyoa"
            , [IncorrectHash, InvalidFeatureFlags]
            )
        ,
            ( "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHeight, CreatedBeforeParent, IncorrectWeight, InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectEpoch, InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectEpoch, IncorrectWeight, InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfIMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectHeight, IncorrectEpoch, IncorrectWeight, InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51ZNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfIMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectHeight, IncorrectTarget, IncorrectEpoch, IncorrectWeight, InvalidFeatureFlags]
            )
        ,
            ( "MPvsAAAAAACKKxE6z6EFAGI7Fu8Y-KmbbIZBxgzKLKa-pX44pSmoDbwTstXtIJ-RAwADAAAA09LAEcELZfLO09B7VfVABLW56BWRD886vi1IAinGEsIHAAAACMGUL-wlgOWFtvBHXAZmVd6U4pItNZG9BiWBuw0QABoJAAAA8Q4oypLEvyHwvs5NEKcPjQHxAgzfIZubmd1UbVVX0nJMpF18Jv-bDRwIIBtaimVVekk-s8dh3hHJ0x7J0gAAAD_5drSYsNay16suHvfzcBbNhNtefBfMewcYZn6ulH9ZCAAAANuI0dfWBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJxwEAAAAAAAHAAAATHNnts6hBQAAAAAAAAAAAHp38ZzIgDr0I1S8ipHT8-BnBebBGHmonuziGlltoNOu"
            , "AAAAAAAAAAB0MCA7z6EFAHp38ZzIgDr0I1S8ipHT8-BnBebBGHmonuziGlltoNOuAwADAAAAZZ58qvVe7bwjKHrHq1dBcVW3t2ZRvYyf10M-3lGuN_oHAAAAKj6YRU8nAhm0BuR7oVLRCFMpNVHkCMOeptIFw7A9kSkJAAAAU2vZiW03yoYkRWxH5jPhUC0vA499Epn9NaFYnjBOqOJMpF18Jv-bDRwIIBtaimVVekk-s8dh3hHJ0x7J0gAAAGp1lopFxzV-I-hHeHYpH6dReXZF3Iov3ZKr9qnspvmoCAAAAJhyCNnWBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKBwEAAAAAAAHAAAATHNnts6hBQAAF5EAAAAAAHSiqpk_KbwY5lZofgsTCuUFE2EBAiHPaHBcEb56QXTB"
            , [IncorrectHash]
            )
        ,
            ( "UV8kAAAAAADFZ2HwFKIFAJ8hgfl2LV_uh846M6v-KHp6ZI3zNOpeObct-K0miL3cAwACAAAAPtJw9_WRladZJhKDhIEx1WgjkofJISyqBnMNNvDfV84DAAAAWWbE_Gg8T20vfB1m5mLM0NllvjvgMRWL8xhCocLZY8YFAAAA7Xlzumy_WTzT1ROEHzzgWeO8n0s2Gf9zUUZCTFhy3qcdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAL6LzuBDgWv0dp05UPLapBb2H4Y7BjA_tIiHRTAiEvLsAAAAABvmlwZDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFkMEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzON"
            , "tjsXAAAAAAC-IonwFKIFALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzONAwACAAAAyGznxkEiOfo6OHftd86_8aA8B55QaQSiD7f5insUMB4DAAAAPKSjwRZc6V_SJW1AbBwZcnkZ1WbRdKrtN4BnJ0q1RpkFAAAAJv6AJtHEaBuFZpIpdePX7s2uN8n7QhFvNPkX70uZduMdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAAFCpo3PF6aH1bHhB27DlDGt8aiyip06XzMWRcgUEHi2AAAAACMLrgdDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAF0MEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALl0CaPBMj5a0QhuIKtJD6zYj1BxWuoKg-X8ASzosri7"
            , [InvalidFeatureFlags]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [InvalidFeatureFlags]
            )
        ]

-- -------------------------------------------------------------------------- --
-- Misc

t :: T.Text -> T.Text
t = id
