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
import Chainweb.BlockHeight
import Chainweb.Graph
import Chainweb.Test.Orphans.Internal ({- Arbitrary BlockHeader -})
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Properties

tests :: TestTree
tests = testGroup "Chainweb.Test.Blockheader.Validation"
    [ prop_validateMainnet
    , prop_validateTestnet04
    , prop_fail_validateTestnet04
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
prop_validateMainnet = testCase "validate Mainnet01 BlockHeaders" $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) mainnet01Headers
  where
    f t (p, h) = case validateBlockHeader t p h of
        [] -> return ()
        errs -> assertFailure $ "Validation failed for mainnet BlockHeader: " <> sshow errs

prop_validateTestnet04 :: TestTree
prop_validateTestnet04 = testCase "validate Testnet04 BlockHeaders" $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) testnet04Headers
  where
    f t (p, h) = case validateBlockHeader t p h of
        [] -> return ()
        errs -> assertFailure $ "Validation failed for testnet04 BlockHeader: " <> sshow errs

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

prop_fail_validateTestnet04 :: TestTree
prop_fail_validateTestnet04 = testCase "validate invalid Testnet04 BlockHeaders" $ do
    now <- getCurrentTimeIntegral
    traverse_ (f now) testnet04InvalidHeaders
  where
    f t (p, h, expectedErrs) = case validateBlockHeader t p h of
        [] -> assertFailure $ "Validation succeeded unexpectedly for testnet04 BlockHeader"
        errs
            | errs /= expectedErrs -> assertFailure
                $ "Validation failed with expectedly errors for testnet04 BlockHeader"
                <> ", expected: " <> sshow expectedErrs
                <> ", actual: " <> sshow errs
                <> ", parent: " <> sshow (_blockHash $ _parentHeader p)
                <> ", header: " <> sshow (_blockHash h)
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

testnet04Headers :: [(ParentHeader, BlockHeader)]
testnet04Headers = gens <> some
  where
    v = Testnet04
    f = bitraverse (fmap ParentHeader . decode . wrap) (decode . wrap)

    -- Some BlockHeader from the existing testnet04 history
    --
    some = fromJuste $ traverse f
        [
            ( "UV8kAAAAAADFZ2HwFKIFAJ8hgfl2LV_uh846M6v-KHp6ZI3zNOpeObct-K0miL3cAwACAAAAPtJw9_WRladZJhKDhIEx1WgjkofJISyqBnMNNvDfV84DAAAAWWbE_Gg8T20vfB1m5mLM0NllvjvgMRWL8xhCocLZY8YFAAAA7Xlzumy_WTzT1ROEHzzgWeO8n0s2Gf9zUUZCTFhy3qcdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAL6LzuBDgWv0dp05UPLapBb2H4Y7BjA_tIiHRTAiEvLsAAAAABvmlwZDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFkMEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzON"
            , "tjsXAAAAAAC-IonwFKIFALIlKMBoOzx3whNCrI2jyEJycC7wNqkYGZ90ZpAItzONAwACAAAAyGznxkEiOfo6OHftd86_8aA8B55QaQSiD7f5insUMB4DAAAAPKSjwRZc6V_SJW1AbBwZcnkZ1WbRdKrtN4BnJ0q1RpkFAAAAJv6AJtHEaBuFZpIpdePX7s2uN8n7QhFvNPkX70uZduMdkFWkKuxR9s23eB6QAmTYoypdZ7yQb1br40-e6wAAAAFCpo3PF6aH1bHhB27DlDGt8aiyip06XzMWRcgUEHi2AAAAACMLrgdDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAF0MEAAAAAAAHAAAA8vSPcRSiBQAAAAAAAAAAALl0CaPBMj5a0QhuIKtJD6zYj1BxWuoKg-X8ASzosri7"
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            )
        ]

    -- All genesis headers
    gens = flip fmap (toList $ chainIds v) $ \cid ->
        ( ParentHeader $ genesisBlockHeader v cid
        , genesisBlockHeader v cid
        )

    wrap x = "\"" <> x <> "\""

testnet04InvalidHeaders :: [(ParentHeader, BlockHeader, [ValidationFailureType])]
testnet04InvalidHeaders = some
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
            , [IncorrectHash, IncorrectPow]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyoa"
            , [IncorrectHash]
            )
        ,
            ( "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHeight, CreatedBeforeParent, IncorrectWeight]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectEpoch]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfEMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectEpoch, IncorrectWeight]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfIMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectHeight, IncorrectEpoch, IncorrectWeight]
            )
        ,
            ( "y4tiAAAAAAAHezeiFaIFACW5ko6NcfOLAWkPn_OvkFM96koE5tW3fWQwkCyT4afUAwACAAAAMakQZiB62qyknHkdlP0DLzjSpd1653qL8nB7rWJxHVwDAAAAm2G92tcXE2OzcjBFKspFjvArlZbl569wSOJzkUUF43sFAAAAvFp87JmhlHTDew_3ZdjnnC66wk3QyjMCU97aZu4NJlrks51YNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAHr2DqwHXi_8tQqe7DnKDwT402x49iKC60WmjSmNKfA1AAAAAL9h5XhDBgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAe0MEAAAAAAAHAAAAmgYYOhWiBQAAAAAAAAAAAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCt"
            , "l0PGAAAAAABmKWCkFaIFAFsrsuyzBE9_xH67B7uDI9kiraA0zLkkzH3mgLTKrQCtAwACAAAAfYN1Qs9Ua_QGfgpBcgeBZ0J6xhQ4XXlqv_gfYA-hOc0DAAAAsGNf8lbXT-HgPD-cFNdXSkD-ODvcY6ihMwvPbRjvj4cFAAAAzmducVsWbMyG6lvO8nQKcJnVh8cgrOS-gnbOMP9WxKvks51ZNN7Lha7sjUawYooNyn282AybfunTUycy3AAAAGWxPTtpx32AN0mr4Ee3fQErWyEwQytnF9-OP4O280R8AAAAAPgBD3pDBgAAAAAAAAXAAAAAAAAAAAAAAAAAAAAAAAAAfIMEAAAAAAAHAAAAmgYAOhWiBQAAAAAAAAAAAHYVlldEu5EIBSMHw-7F7xi8KYMv5SjvTjo1f1X-Gyol"
            , [IncorrectHash, IncorrectPow, IncorrectHeight, IncorrectTarget, IncorrectEpoch, IncorrectWeight]
            )
        ,
            ( "MPvsAAAAAACKKxE6z6EFAGI7Fu8Y-KmbbIZBxgzKLKa-pX44pSmoDbwTstXtIJ-RAwADAAAA09LAEcELZfLO09B7VfVABLW56BWRD886vi1IAinGEsIHAAAACMGUL-wlgOWFtvBHXAZmVd6U4pItNZG9BiWBuw0QABoJAAAA8Q4oypLEvyHwvs5NEKcPjQHxAgzfIZubmd1UbVVX0nJMpF18Jv-bDRwIIBtaimVVekk-s8dh3hHJ0x7J0gAAAD_5drSYsNay16suHvfzcBbNhNtefBfMewcYZn6ulH9ZCAAAANuI0dfWBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJxwEAAAAAAAHAAAATHNnts6hBQAAAAAAAAAAAHp38ZzIgDr0I1S8ipHT8-BnBebBGHmonuziGlltoNOu"
            , "AAAAAAAAAAB0MCA7z6EFAHp38ZzIgDr0I1S8ipHT8-BnBebBGHmonuziGlltoNOuAwADAAAAZZ58qvVe7bwjKHrHq1dBcVW3t2ZRvYyf10M-3lGuN_oHAAAAKj6YRU8nAhm0BuR7oVLRCFMpNVHkCMOeptIFw7A9kSkJAAAAU2vZiW03yoYkRWxH5jPhUC0vA499Epn9NaFYnjBOqOJMpF18Jv-bDRwIIBtaimVVekk-s8dh3hHJ0x7J0gAAAGp1lopFxzV-I-hHeHYpH6dReXZF3Iov3ZKr9qnspvmoCAAAAJhyCNnWBQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKBwEAAAAAAAHAAAATHNnts6hBQAAF5EAAAAAAHSiqpk_KbwY5lZofgsTCuUFE2EBAiHPaHBcEb56QXTB"
            , [IncorrectHash]
            )
        ]

