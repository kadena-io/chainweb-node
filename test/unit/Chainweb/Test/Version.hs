{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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

import Control.Lens (view)
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.List.NonEmpty as NE

import Test.QuickCheck
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Graph
import Chainweb.Test.Orphans.Internal
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04

tests :: TestTree
tests = testGroup "ChainwebVersion properties"
    [ graphTests
    , headerSizeTests
    ]

-- -------------------------------------------------------------------------- --
-- Utils

propForVersions :: String -> (HasVersion => Property) -> TestTree
propForVersions desc prop = testGroup desc
    [ testProperty "arbitrary versions" $ \v -> withVersion v prop
    , testProperty "mainnet" $ withVersion Mainnet01 prop
    , testProperty "testnet04" $ withVersion Testnet04 prop
    , testProperty "recapDevnet" $ withVersion RecapDevelopment prop
    ]

-- -------------------------------------------------------------------------- --
-- Graphs

graphTests :: TestTree
graphTests = testGroup "Graphs"
    [ propForVersions "versionGraphs are sorted" prop_chainGraphs_sorted
    , propForVersions "genesisHeight is greater or equal than 0 for all chains" prop_genesisHeight
    , propForVersions "chain graphs order ordered by order" prop_chainGraphs_order
    , propForVersions "chainIds are chains of latest graph" prop_chainIds
    ]

prop_chainGraphs_sorted :: HasVersion => Property
prop_chainGraphs_sorted
    = property (ruleValid (_versionGraphs implicitVersion))

prop_chainGraphs_order :: HasVersion => Property
prop_chainGraphs_order = orders === NE.reverse (NE.sort orders)
  where
    orders = ruleElems $ fmap order $ _versionGraphs implicitVersion

prop_genesisHeight :: HasVersion => Property
prop_genesisHeight = property $ all ((>= 0) . genesisHeight) chainIds

prop_chainIds :: HasVersion => Property
prop_chainIds = chainIds === graphChainIds (snd $ ruleHead $ _versionGraphs implicitVersion)

-- -------------------------------------------------------------------------- --
--  Header Sizes

headerSizeTests :: TestTree
headerSizeTests = testGroup "HeaderSize"
    [ propForVersions "base size golden" prop_headerBaseSizeBytes_golden
    , propForVersions "base size" prop_headerBaseSizeBytes
    , propForVersions "sizes sorted" prop_headerSizes_sorted
    , propForVersions "sizes order" prop_headerSizes_order
    , propForVersions "genesis header size bytes" prop_headerSizeBytes_gen
    , propForVersions "header size bytes" prop_headerSizeBytes
    , propForVersions "work size bytes" prop_workSizeBytes
    ]

-- | A "golden" test property. If the value changes the test will fails and must
-- be manually updated. This protectes against accidentally changing this value.
--
prop_headerBaseSizeBytes_golden :: HasVersion => Property
prop_headerBaseSizeBytes_golden = _versionHeaderBaseSizeBytes implicitVersion === 208

prop_headerBaseSizeBytes :: HasVersion => Property
prop_headerBaseSizeBytes = property $ do
    cid <- elements $ toList $ chainIds
    let genHdr = genesisBlockHeader cid
        gen = runPutS $ encodeBlockHeader genHdr
        as = runPutS $ encodeBlockHashRecord (view blockAdjacentHashes genHdr)
    return $ _versionHeaderBaseSizeBytes implicitVersion === int (B.length gen - B.length as)

prop_headerSizes_sorted :: HasVersion => Property
prop_headerSizes_sorted
    = NE.reverse (NE.sort (ruleElems headerSizes)) === ruleElems headerSizes

prop_headerSizes_order :: HasVersion => Property
prop_headerSizes_order = orders === NE.reverse (NE.sort orders)
  where
    orders = ruleElems $ fmap order $ _versionGraphs implicitVersion

prop_headerSizeBytes_gen :: HasVersion => Property
prop_headerSizeBytes_gen = property $ do
    cid <- elements $ toList chainIds
    let hdr = genesisBlockHeader cid
        l = int $ B.length $ runPutS $ encodeBlockHeader $ hdr
    return
        $ counterexample ("chain: " <> sshow cid)
        $ headerSizeBytes cid (view blockHeight hdr) === l

prop_headerSizeBytes :: HasVersion => Property
prop_headerSizeBytes = property $ do
    h <- arbitraryBlockHeaderVersion
    let l = int $ B.length $ runPutS $ encodeBlockHeader h
    return
        $ counterexample ("header: " <> sshow h)
        $ headerSizeBytes (view blockChainId h) (view blockHeight h) === l

prop_workSizeBytes :: HasVersion => Property
prop_workSizeBytes = property $ do
    h <- arbitraryBlockHeaderVersion
    if (view blockHeight h == genesisHeight (_chainId h))
      then discard
      else do
        let l = int $ B.length $ runPutS $ encodeBlockHeaderWithoutHash h
        return
            $ counterexample ("header: " <> sshow h)
            $ workSizeBytes (view blockHeight h) === l
