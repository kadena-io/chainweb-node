{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Version
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Version
( tests
) where

import qualified Data.List.NonEmpty as NE

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

-- internal modules

import Chainweb.Graph
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Version

tests :: TestTree
tests = testGroup "ChainwebVersion properties"
    [ testProperty "chainwebGraphs are sorted" $ prop_chainGraphs_sorted
    , testProperty "chainwebGraphs is sorted for mainnet" $ prop_chainGraphs_sorted Mainnet01
    , testProperty "chainwebGraphs is sorted for testnet" $ prop_chainGraphs_sorted Testnet04
    , testProperty "chainwebGraphs is sorted for devnet" $ prop_chainGraphs_sorted Development

    , testProperty "chainGraphs history starts at 0" $ prop_chainGraphs_0
    , testProperty "chainGraphs history starts at 0 for mainnet" $ prop_chainGraphs_0 Mainnet01
    , testProperty "chainGraphs history starts at 0 for testnet" $ prop_chainGraphs_0 Testnet04
    , testProperty "chainGraphs history starts at 0 for devenet" $ prop_chainGraphs_0 Development

    , testProperty "gensisHeight is greater or equal than 0 for all chains" prop_genesisHeight
    , testProperty "gensisHeight is greater or equal than 0 for all chains on mainnet" $ prop_genesisHeight Mainnet01
    , testProperty "gensisHeight is greater or equal than 0 for all chains on testnet" $ prop_genesisHeight Testnet04
    , testProperty "gensisHeight is greater or equal than 0 for all chains on devnet" $ prop_genesisHeight Development

    , testProperty "chain graphs order ordered by order" prop_chainGraphs_order
    , testProperty "chain graphs order ordered by order on mainnet" $ prop_chainGraphs_order Mainnet01
    , testProperty "chain graphs order ordered by order on testnet" $ prop_chainGraphs_order Testnet04
    , testProperty "chain graphs order ordered by order on devnet" $ prop_chainGraphs_order Development

    , testProperty "chainIds are chains of latest graph" prop_chainIds
    , testProperty "chainIds are chains of latest graph on mainnet" $ prop_chainIds Mainnet01
    , testProperty "chainIds are chains of latest graph on testnet" $ prop_chainIds Testnet04
    , testProperty "chainIds are chains of latest graph on devnet" $ prop_chainIds Development
    ]

prop_chainGraphs_sorted :: ChainwebVersion -> Property
prop_chainGraphs_sorted v
    = (NE.reverse $ NE.sort $ chainwebGraphs v) === chainwebGraphs v

prop_chainGraphs_0 :: ChainwebVersion -> Property
prop_chainGraphs_0 = (===) 0 . fst . NE.last . chainwebGraphs

prop_chainGraphs_order :: ChainwebVersion -> Property
prop_chainGraphs_order v = orders === NE.reverse (NE.sort orders)
  where
    orders = fmap (order . snd) $ chainwebGraphs v

prop_genesisHeight :: ChainwebVersion -> Bool
prop_genesisHeight v = all ((>= 0) . genesisHeight v) $ chainIds v

prop_chainIds :: ChainwebVersion -> Property
prop_chainIds v = chainIds v === graphChainIds (snd $ NE.head $ chainwebGraphs v)

