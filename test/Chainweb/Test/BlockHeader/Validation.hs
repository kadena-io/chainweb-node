{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens hiding ((.=))
import Control.Monad.Catch

import Data.Aeson
import Data.Bits
import qualified Data.ByteString as B
import Data.DoubleWord
import Data.Foldable
import Data.List (sort)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import qualified Data.Text as T

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.Difficulty
import Chainweb.Graph hiding (AdjacentChainMismatch)
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Test.Utils.TestHeader
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Data.Word.Encoding

-- -------------------------------------------------------------------------- --
-- Properties

tests :: TestTree
tests = testGroup "Chainweb.Test.Blockheader.Validation"
    [ prop_validateMainnet
    , prop_validateTestnet04
    , prop_fail_validate
    , prop_featureFlag (Test petersonChainGraph) 10
    ]

-- -------------------------------------------------------------------------- --
-- Rules are not trivial
--
-- There is an input for which the rule fails.
--

prop_featureFlag :: ChainwebVersion -> BlockHeight -> TestTree
prop_featureFlag v h = testCase ("Invalid feature flags fail validation for " <> sshow v) $ do
    hdr <- (blockHeight .~ h)
        . (blockFlags .~ fromJuste (decode "1"))
        . (blockChainwebVersion .~ v)
        <$> generate arbitrary
    let r = prop_block_featureFlags hdr
    assertBool
        ("feature flag validation succeeded unexpectedly: " <> sshow  hdr)
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
prop_validateHeaders msg hdrs = testCase msg $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) hdrs
  where
    f now h = case validateBlockHeaderM now (testHeaderChainLookup h) (_testHeaderHdr h) of
        Right _ -> return ()
        Left errs -> assertFailure $ "Validation failed for mainnet BlockHeader: " <> sshow errs

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
prop_fail_validate = testCase "validate invalid BlockHeaders" $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) validationFailures
  where
    f now (h, expectedErrs)
        = try (validateBlockHeaderM now (testHeaderChainLookup h) (_testHeaderHdr h)) >>= \case
            Right _ -> assertFailure $ "Validation succeeded unexpectedly for validationFailures BlockHeader"
            Left ValidationFailure{ _validationFailureFailures = errs }
                | sort errs /= sort expectedErrs -> assertFailure
                    $ "Validation failed with unexpected errors for BlockHeader"
                    <> ", expected: " <> sshow expectedErrs
                    <> ", actual: " <> sshow errs
                    <> ", header: " <> sshow (_blockHash $ _testHeaderHdr h)
            _ -> return () -- FIXME be more specific

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
    , ( hdr & testHeaderHdr . blockHash %~ messWords encodeBlockHash decodeBlockHash (flip complementBit 0)
      , [IncorrectHash]
      )
    , ( hdr & testHeaderHdr . blockTarget %~ messWords encodeHashTarget decodeHashTarget (flip complementBit 0)
      , [IncorrectHash, IncorrectPow, IncorrectTarget]
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
    , ( hdr & testHeaderHdr . blockChainId .~ unsafeChainId 1
      , [IncorrectHash, IncorrectPow, ChainMismatch, AdjacentChainMismatch]
      )
    , ( hdr & testHeaderHdr . blockChainwebVersion .~ Development
      , [IncorrectHash, IncorrectPow, VersionMismatch]
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
    , ( hdr & testHeaderHdr . blockFlags .~ fromJuste (runGet decodeFeatureFlags badFlags)
            & testHeaderHdr . blockHeight .~ 530499
      , [IncorrectHash, IncorrectPow, IncorrectHeight]
      )
    , ( hdr & testHeaderHdr . blockFlags .~ fromJuste (runGet decodeFeatureFlags badFlags)
            & testHeaderHdr . blockHeight .~ 530500
      , [IncorrectHash, IncorrectPow, InvalidFeatureFlags, IncorrectHeight]
      )
    , ( hdr & testHeaderHdr . blockAdjacentHashes .~ BlockHashRecord mempty
      , [IncorrectHash, IncorrectPow]
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
    messByteString enc dec f x = fromJuste $ runGet dec $ f $ runPut $ enc x

    messWords :: (a -> Put) -> Get a -> (Word256 -> Word256) -> a -> a
    messWords enc dec f x = messByteString enc dec g x
      where
        g bytes = runPut (encodeWordLe $ f $ fromJuste $ runGet decodeWordLe bytes)

-- -------------------------------------------------------------------------- --
-- A representative collection of Mainnet01 Headers pairs

-- | A representative collection of BlockHeader from the existing mainnet01
-- history
--
mainnet01Headers :: [TestHeader]
mainnet01Headers = genesisTestHeaders Mainnet01 <>
    [ testHeader
        [ "parent" .= t "AFHBANxHkLyt2kf7v54FAByxfFrR-pBP8iMLDNKO0SSt-ntTEh1IVT2E4mSPkq02AwACAAAAfaGIEe7a-wGT8OdEXz9RvlzJVkJgmEPmzk42bzjQOi0GAAAAjFsgdB2riCtIs0j40vovGGfcFIZmKPnxEXEekcV28eUIAAAAQcKA2py0L5t1Z1u833Z93V5N4hoKv_7-ZejC_QKTCzTtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAALFMJ1gcC8oKW90MW2xY07gN10bM2-GvdC7fDvKDDwAPBwAAAJkPwMVeS7ZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAT3hhzb-eBQAAAGFSDbQAAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-"
        , "header" .=  t "AEbpAIzqpiins1r8v54FAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-AwACAAAAy7QSAHoIeFj0JXide_co-OaEzzYWbeZhAfphXI8-IR0GAAAAa-PzO_zUmk1yLOyt2kD3iI6cehKqQ_KdK8D6qZ-X6X4IAAAA79Vw2kqbVDHm9WDzksFwxZcmx5OJJNW-ge7jVa3HiHbtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAAL701u70FOrdivm6quNUsKgfi2L8zYHeyOI0j2gfP16jBwAAANz0ZdfSwLZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOtsEAAAAAAAFAAAAT3hhzb-eBQAAAPvI7fkAAFFuYkCHZRcNl1k3-A1EZvyPxhiFKdHZwZRTqos57aiO"
        , "adjacents" .=
            [ t "ACcMAPA_ii9z0Ez7v54FAEHCgNqctC-bdWdbvN92fd1eTeIaCr_-_mXowv0Ckws0AwADAAAAxnGpa89fzxURJdpCA92MZmlDtgG9AZFVPCsCwNyDly8HAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTYJAAAAzmf29gDZjNcpxkw3EP9JgnU3-ARNJ14NisscofzzARCjTKbuwLbdyjay0MQ3l7xPGULH_yLMDPh4LQIAAAAAADbjm8GoWvx_3YNJ47vz54_LXV95MTKI4drB2fk5AdPlCAAAADS2qD13VlhnAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA0wwnzb-eBQAAAAnWry0AAO_VcNpKm1Qx5vVg85LBcMWXJseTiSTVvoHu41Wtx4h2"
            , t "AAARACjupkPfZFz8v54FAIxbIHQdq4grSLNI-NL6Lxhn3BSGZij58RFxHpHFdvHlAwABAAAAfdHDK_Q8xoD-W0nBPPBPMOgs1VukuCImYwCNnaBUwOMFAAAA4eefM0SUltzJ0Qszo3N0R9B4w_ap2_M2e6nlKEqJmkoHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbiMNKcS7VzGITdCwrGSYWrFNQvGP7KAzjbLQIAAAAAABlK0LefdM1J4t_Qeg6xAVNNDKEOhiEmNKe6SK9N6TAZBgAAALFBPU7YDgRoAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAA2eJ0y7-eBQAAAORH5OUAAGvj8zv81JpNcizsrdpA94iOnHoSqkPynSvA-qmfl-l-"
            , t "AABPAIw5kEeHUtD7v54FAH2hiBHu2vsBk_DnRF8_Ub5cyVZCYJhD5s5ONm840DotAwAAAAAAPYZZ2yg5iXsMOyKqKKUhrGaboexUhUVK8e-fhn3FzNkEAAAAu_A9WCeRoLM17g_jc0A2UnhvCQFe5LCtTnaze9LqajQHAAAAHLF8WtH6kE_yIwsM0o7RJK36e1MSHUhVPYTiZI-SrTbP1aVtUvTRaiRyg9hCVSPXuIpf3IjuwHaBKgIAAAAAABQWMBli4UbscIslyPPH2ItcNaY2_Fm7yFucQM86oqojAgAAAFy5ttLGN19tAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAddFMzL-eBQAAALuSPmYAAMu0EgB6CHhY9CV4nXv3KPjmhM82Fm3mYQH6YVyPPiEd"
            ]
        ]
    , testHeader
        [ "parent" .= t "AAAAAAAAAABEtTTKCqMFAEjmr_NPBprFiWD_WhyvMzQCiHGxnE1sYKeKGTOPjvD5AwACAAAAMrWEo-w-oixyUqELYmgOvRU7Z7FTjPzNLJCYXu26OuIDAAAA4c_RjHJ0N4gH6uN3TLpowhFIUGFRUe1celP6BwyFBwAFAAAAF2p4lSHRyyPrQ8GF1akfJM9EfzYSSsx5NI5IHtavNXEYhLsC8eqcLrj_VXvO_p1n4hz5QVR4ul3FpwAAAAAAAPDGI9fJxK4qRkWzcPv64tL1wNu0j76Vws5_oXrHsrPxAAAAAGJcPGAFgqQkBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAx9MnbwqjBQAAybvnn0-KAPLMEAsBh7vLnOh9053meOcuahQ_ezuUWynN1sMwSWqQ"
        , "header" .= t "AAAAAAAAAACnKMTLCqMFAPLMEAsBh7vLnOh9053meOcuahQ_ezuUWynN1sMwSWqQAwACAAAA_5afAofRZMA5Lz_ZG1Y0l6PJFTWueyU_4GbGWGtt_aoDAAAAs4MViuWgDm1nCgvyE5kzgG-_eZhCJupijIoh2z9cy0MFAAAA760lZUnDPEzHB6SKPbfOhFjpNNYDCuUXfxjvO3W4AckYhLsC8eqcLrj_VXvO_p1n4hz5QVR4ul3FpwAAAAAAAEQxBl7xgdFiynPSwT5ZNOcH8fiWKTdX9j09CUDn2irbAAAAAGHDHxemCKYkBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAZEEHAAAAAAAFAAAAx9MnbwqjBQAdLDW8JkF8ALVfqYGwKAkKf4NHKGRn1autmwMCCgXM-ZHa2zwgRk3R"
        , "adjacents" .=
            [ t "AAAAAAAAAADjy2TKCqMFABdqeJUh0csj60PBhdWpHyTPRH82EkrMeTSOSB7WrzVxAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkGAAAAR8k4YCs8tUNx_xN-6pBWwK6ZsQM4aVqNdChpN59IDoQJAAAAHq0_GIZUoec-dF3DHYqzMyJmaOdGdnaw4FtuQuO2LOvjgZy5BaDXFN80S7hBeqGZ2cWTwRs5c-2BtgAAAAAAAMYc1v2Uw5BO6zQXlJ5l5oGsLo-rGkpweUWRftVgIkeHBQAAAP8ehKwH-DP2AwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAuHnmcQqjBQAAfJOEnJ3EAO-tJWVJwzxMxwekij23zoRY6TTWAwrlF38Y7zt1uAHJ"
            , t "AAAAAAAAAABpbsjKCqMFADK1hKPsPqIsclKhC2JoDr0VO2exU4z8zSyQmF7tujriAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkEAAAA9wHcfWI2MNTrJuoPxllY3pm7BVa4Zgr8Ojdufj4oLw4HAAAAe1EOiqoneVZjUWfvqItXpXBais2UiUeoqcvc32prVo4q7OJWUH7TcdN9wcJMzNuUmuZtySQqALShowAAAAAAACNm7rsELSjBjDerkGAB1JSp3Ink0NG0t7Wqupyi3cqBAgAAAHjeCa0pNJQ0BAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAf6OvbgqjBQAGddYi31ibAP-WnwKH0WTAOS8_2RtWNJejyRU1rnslP-Bmxlhrbf2q"
            , t "AAAAAAAAAABF8PnKCqMFAOHP0YxydDeIB-rjd0y6aMIRSFBhUVHtXHpT-gcMhQcAAwAAAAAASOav808GmsWJYP9aHK8zNAKIcbGcTWxgp4oZM4-O8PkBAAAA5fRcXeTuKnS5Q4DuydXASBIX29tMsytHjEP21Vf2Ah0IAAAAJvJibit6A2QiqrOZmRe9zJRjW7PtmLJ51hDMUh1UyX4MFPyyFPku-007fa_t4ax7z5uSCQnTRLz1pAAAAAAAAHkJOGZlsei0ZFfkYdc0bBM7I39OmrLZlJTegBfLHNSYAwAAAH1mw3DMvaAaBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAY0EHAAAAAAAFAAAAhQs5cAqjBQAAKCAqaKcvALODFYrloA5tZwoL8hOZM4Bvv3mYQibqYoyKIds_XMtD"
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
