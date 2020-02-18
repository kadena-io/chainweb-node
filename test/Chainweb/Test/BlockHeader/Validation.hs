{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Lens

import Data.Aeson
import Data.Bitraversable
import Data.Foldable

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeader.Validation
import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHeader -})
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Properties

tests :: TestTree
tests = testGroup "Chainweb.Test.Blockheader.Validation"
    [ prop_validateMainnet
    -- , prop_featureFlag Mainnet01
    -- , prop_featureFlag Testnet04
    -- , prop_featureFlag Development
    ]

-- -------------------------------------------------------------------------- --
-- Rules are not trivial
--
-- There is an input for which the rule fails.
--

{-
-- Feature flag validation is currently disabled on Mainnet. Uncomment this
-- code once it is enabled.
prop_featureFlag :: ChainwebVersion -> TestTree
prop_featureFlag v = testCase ("Invalid feature flags fail validation for " <> sshow v) $ do
    hdr <- (blockHeight .~ TODO)
        . (blockFlags .~ fromJuste (decode "1"))
        . (blockChainwebVersion .~ v)
        <$> generate arbitrary
    let r = prop_block_featureFlags hdr
    assertBool
        ("feature flag validation succeeded unexpectedly: " <> sshow  hdr)
        (not r)
-}

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
prop_validateMainnet = testCase "validate Mainnet01 BlockHeaders" $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) mainnet01Headers
  where
    f t (ParentHeader p, h) = case validateBlockHeader t p h of
        [] -> return ()
        errs -> assertFailure $ "Validation failed for mainnet BlockHeader: " <> sshow errs

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
-- A representative collection of Mainnet01 Headers pairs

-- | A representative collection of BlockHeader from the existing mainnet01
-- history
--
mainnet01Headers :: [(ParentHeader, BlockHeader)]
mainnet01Headers = gens <> some
  where
    v = Mainnet01
    f = bitraverse (fmap ParentHeader . decode . wrap) (decode . wrap)

    -- Some BlockHeader from the existing mainnet01 history
    some = fromJuste $ traverse f
        [
            ( "H3rGh41BFgB2dYQvypgFACg92VqS0_k716NKvao4RfW8a0Nyj1KUuoGqFV_3OXDCAwACAAAAeEynJU7e0IPfQXm53PPHDkjPq5J3ycf2SqJVxFcIXrcDAAAAyY-kayiK41qNuDkMN3p3mwE57QHm1Tcj8nd-lP2YyDwFAAAAQ4a9wEuRhggN59f0mgzljWW7tjfCQtqD68fZbmYuz-LEgt46tQRDXU98wWRlQl0BSVHI5X01nzE6NgcAAAAAAJt2iGmFIGBIqsZvGLpp2ajVU915fQ_Hxu2hZo9fEFEKAAAAAKkNF7J27u0RAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoIYBAAAAAAAFAAAAF5am6cmYBQAAAAAAAAAAAFu19jecoQhbBKnxAbWQLr2tMlBv82LIqL1tzsjD4HeJ"
            , "NZX2DMUA7v_au3swypgFAFu19jecoQhbBKnxAbWQLr2tMlBv82LIqL1tzsjD4HeJAwACAAAA1mtN-xxumd3H1HB3IHu-Baz4hPzhGIKJhXNZbXZ9zWsDAAAAuwkSBAu4bGfFnQgcr6bNbIbHsjtAbI3YZ2Whf9ek3PMFAAAAleDFg-L56CcqBd1Waek0nXQfuufTKQoJNosU4S8ol13Egt46tQRDXU98wWRlQl0BSVHI5X01nzE6NgcAAAAAAFG_dy06qOtH93eteXZnNrf7zXOZSefhxpAH1IZceu00AAAAAAzWH_71Ee4RAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoYYBAAAAAAAFAAAAF5am6cmYBQAAAAAAAAAAAPEQhg0G_mOQpv_381FvmUpHVqtekGgmczG5Thtqoigx"
            )
        ,
            ( "AFHBANxHkLyt2kf7v54FAByxfFrR-pBP8iMLDNKO0SSt-ntTEh1IVT2E4mSPkq02AwACAAAAfaGIEe7a-wGT8OdEXz9RvlzJVkJgmEPmzk42bzjQOi0GAAAAjFsgdB2riCtIs0j40vovGGfcFIZmKPnxEXEekcV28eUIAAAAQcKA2py0L5t1Z1u833Z93V5N4hoKv_7-ZejC_QKTCzTtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAALFMJ1gcC8oKW90MW2xY07gN10bM2-GvdC7fDvKDDwAPBwAAAJkPwMVeS7ZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOdsEAAAAAAAFAAAAT3hhzb-eBQAAAGFSDbQAAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-"
            , "AEbpAIzqpiins1r8v54FAJru7keLmw3rHfSVm9wkTHWQBBTwEPwEg8RA99vzMuj-AwACAAAAy7QSAHoIeFj0JXide_co-OaEzzYWbeZhAfphXI8-IR0GAAAAa-PzO_zUmk1yLOyt2kD3iI6cehKqQ_KdK8D6qZ-X6X4IAAAA79Vw2kqbVDHm9WDzksFwxZcmx5OJJNW-ge7jVa3HiHbtgKwxXj4Eovf97ELmo_iBruVLoK_Yann5LQIAAAAAAL701u70FOrdivm6quNUsKgfi2L8zYHeyOI0j2gfP16jBwAAANz0ZdfSwLZkAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOtsEAAAAAAAFAAAAT3hhzb-eBQAAAPvI7fkAAFFuYkCHZRcNl1k3-A1EZvyPxhiFKdHZwZRTqos57aiO"
            )
        ]

    -- All genesis headers
    gens = flip fmap (toList $ chainIds v) $ \cid ->
        ( ParentHeader $ genesisBlockHeader v cid
        , genesisBlockHeader v cid
        )

    wrap x = "\"" <> x <> "\""
