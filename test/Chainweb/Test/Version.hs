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

import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.List.NonEmpty as NE

import Test.QuickCheck
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.Graph
import Chainweb.Test.Orphans.Internal
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

tests :: TestTree
tests = testGroup "ChainwebVersion properties"
    [ graphTests
    , headerSizeTests
    ]

-- -------------------------------------------------------------------------- --
-- Utils

propForVersions :: String -> (ChainwebVersion -> Property) -> TestTree
propForVersions desc prop = testGroup desc
    [ testProperty "arbitrary versions" $ prop
    , testProperty "mainnet" $ prop Mainnet01
    , testProperty "testnet" $ prop Testnet04
    , testProperty "devnet" $ prop Development
    ]

-- -------------------------------------------------------------------------- --
-- Graphs

graphTests :: TestTree
graphTests = testGroup "Graphs"
    [ propForVersions "chainwebGraphs are sorted" prop_chainGraphs_sorted
    , propForVersions "chainGraphs history starts at 0" $ prop_chainGraphs_0
    , propForVersions "gensisHeight is greater or equal than 0 for all chains" prop_genesisHeight
    , propForVersions "chain graphs order ordered by order" prop_chainGraphs_order
    , propForVersions "chainIds are chains of latest graph" prop_chainIds
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

prop_genesisHeight :: ChainwebVersion -> Property
prop_genesisHeight v = property $ all ((>= 0) . genesisHeight v) $ chainIds v

prop_chainIds :: ChainwebVersion -> Property
prop_chainIds v = chainIds v === graphChainIds (snd $ NE.head $ chainwebGraphs v)

-- -------------------------------------------------------------------------- --
--  Header Sizes

headerSizeTests :: TestTree
headerSizeTests = testGroup "HeaderSize"
    [ propForVersions "base size golden" prop_headerBaseSizeBytes_golden
    , propForVersions "base size" prop_headerBaseSizeBytes
    , propForVersions "sizes sorted" prop_headerSizes_sorted
    , propForVersions "sizes 0" prop_headerSizes_0
    , propForVersions "sizes order" prop_headerSizes_order
    , propForVersions "genesis header size bytes" prop_headerSizeBytes_gen
    , propForVersions "header size bytes" prop_headerSizeBytes
    , propForVersions "work size bytes" prop_workSizeBytes
    ]

-- | A "golden" test property. If the value changes the test will fails and must
-- be manually updated. This protectes against accidentally changing this value.
--
prop_headerBaseSizeBytes_golden :: ChainwebVersion -> Property
prop_headerBaseSizeBytes_golden v = headerBaseSizeBytes v === 208

prop_headerBaseSizeBytes :: ChainwebVersion -> Property
prop_headerBaseSizeBytes v = property $ do
    cid <- elements $ toList $ chainIds v
    let genHdr = genesisBlockHeader v cid
        gen = runPutS $ encodeBlockHeader genHdr
        as = runPutS $ encodeBlockHashRecord (_blockAdjacentHashes genHdr)
    return $ headerBaseSizeBytes v === int (B.length gen - B.length as)

prop_headerSizes_sorted :: ChainwebVersion -> Property
prop_headerSizes_sorted v
    = (NE.reverse $ NE.sort $ headerSizes v) === headerSizes v

prop_headerSizes_0 :: ChainwebVersion -> Property
prop_headerSizes_0 = (===) 0 . fst . NE.last . headerSizes

prop_headerSizes_order :: ChainwebVersion -> Property
prop_headerSizes_order v = orders === NE.reverse (NE.sort orders)
  where
    orders = fmap (order . snd) $ chainwebGraphs v

prop_headerSizeBytes_gen :: ChainwebVersion -> Property
prop_headerSizeBytes_gen v = property $ do
    cid <- elements $ toList $ chainIds v
    let hdr = genesisBlockHeader v cid
        l = int $ B.length $ runPutS $ encodeBlockHeader $ hdr
    return
        $ counterexample ("chain: " <> sshow cid)
        $ headerSizeBytes v cid (_blockHeight hdr) === l

prop_headerSizeBytes :: ChainwebVersion -> Property
prop_headerSizeBytes v = property $ do
    h <- arbitraryBlockHeaderVersion v
    let l = int $ B.length $ runPutS $ encodeBlockHeader h
    return
        $ counterexample ("header: " <> sshow h)
        $ headerSizeBytes (_chainwebVersion h) (_blockChainId h) (_blockHeight h) === l

prop_workSizeBytes :: ChainwebVersion -> Property
prop_workSizeBytes v = property $ do
    h <- arbitraryBlockHeaderVersion v
    if (_blockHeight h == genesisHeight v (_chainId h))
      then discard
      else do
        let l = int $ B.length $ runPutS $ encodeBlockHeaderWithoutHash h
        return
            $ counterexample ("header: " <> sshow h)
            $ workSizeBytes (_chainwebVersion h) (_blockHeight h) === l

