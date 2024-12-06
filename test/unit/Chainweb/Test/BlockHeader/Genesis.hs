{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Test.BlockHeader.Genesis
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Test.BlockHeader.Genesis ( tests ) where

import Control.Lens

import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, testProperties)

-- internal modules

import Chainweb.BlockHash (encodeBlockHash)
import Chainweb.BlockHeader
import Chainweb.Difficulty
import Chainweb.Test.Utils (golden)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04

---

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
tests :: TestTree
tests = testGroup "Chainweb.Test.BlockHeader.Genesis"
    [ testGroup "genesis header golden tests" $ blockHashTest <$>
        [ RecapDevelopment
        , Testnet04
        , Mainnet01
        ]
    , graphTransitionTargetTests
    ]

blockHashes :: HM.HashMap ChainId BlockHeader -> BL.ByteString
blockHashes =
    BB.toLazyByteString . foldMap (hash . snd) . sortBy (compare `on` fst) . HM.toList
  where
    hash :: BlockHeader -> BB.Builder
    hash = BB.byteString . B64U.encode . runPutS . encodeBlockHash . view blockHash

blockHashTest :: ChainwebVersion -> TestTree
blockHashTest v = golden (sshow v <> "-block-hashes") $
    pure $ blockHashes $ genesisBlockHeaders v

-- -------------------------------------------------------------------------- --
-- Graph Transition Targets

graphTransitionTargetTests :: TestTree
graphTransitionTargetTests = testGroup "graph transition genesis targets"
    -- mainnet20InitialHashTarget properties
    [ testProperty "mainnet20InitialHashTarget deserialization" $
        Just (Mainnet01 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) === (HashTarget . (4 *) <$> decodePowHashNat64 "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
    , testProperty "mainnet20InitialHashTarget json deserialization" $
        Just (Mainnet01 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) === (HashTarget . (4 *) <$> decodePowHashNatJson "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
    , testProperties "mainnet old chains" $
        forChain Mainnet01 maxTarget . unsafeChainId <$> [0..9]
    , testProperties "mainnet new chains" $
        forChain Mainnet01 (Mainnet01 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) . unsafeChainId <$> [10..19]

    -- testnet20InitialHashTarget properties
    , testProperty "testnet20InitialHashTarget deserialization" $
        Just (Testnet04 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) === (HashTarget <$> decodePowHashNat64 "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
    , testProperty "testnet20InitialHashTarget json deserialization" $
        Just (Testnet04 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) === (HashTarget <$> decodePowHashNatJson "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
    , testProperties "testnet04 old chains" $
        forChain Testnet04 maxTarget . unsafeChainId <$> [0..9]
    , testProperties "testnet04 new chains" $
        forChain Testnet04 (Testnet04 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) . unsafeChainId <$> [10..19]

    -- Cross check targets to ensure that the values are as expected
    , testProperty "cross check testnet20InitialHashTarget and mainnet20InitialHashTarget" $
        _hashTarget (Testnet04 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) `div`
            _hashTarget (Mainnet01 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10))
        === PowHashNat 8893
    , testProperty "cross check development and testnet20InitialHashTarget" $
        _hashTarget (RecapDevelopment ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10)) `div`
            _hashTarget (Testnet04 ^?! versionGenesis . genesisBlockTarget . atChain (unsafeChainId 10))
        === PowHashNat 20321
    ]

  where
    forChain v target cid = (show cid, v ^?! versionGenesis . genesisBlockTarget . atChain cid === target)
    decodePowHashNat64 t = runGetS decodePowHashNat =<< decodeB64UrlNoPaddingText t
    decodePowHashNatJson t = decodeStrictOrThrow' @_ @PowHashNat $ "\"" <> t <> "\""
