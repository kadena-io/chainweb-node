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
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId (ChainId, unsafeChainId)
import Chainweb.Difficulty
import Chainweb.Test.Utils (golden)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version (ChainwebVersion(..))

---

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
tests :: TestTree
tests = testGroup "Chainweb.Test.BlockHeader.Genesis" $
    [ testGroup "genesis header golden tests" $ blockHash <$>
        [ Development
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
    hash = BB.byteString . B64U.encode . runPutS . encodeBlockHash . _blockHash

blockHash :: ChainwebVersion -> TestTree
blockHash v = golden (sshow v <> "-block-hashes") $
    pure $ blockHashes $ genesisBlockHeaders v

-- -------------------------------------------------------------------------- --
-- Graph Transition Targets

graphTransitionTargetTests :: TestTree
graphTransitionTargetTests = testGroup "graph transition genesis targets"
    -- mainnet20InitialHashTarget properties
    [ testProperty "mainnet20InitialHashTarget deserialization" $
        Just mainnet20InitialHashTarget === (HashTarget . (4 *) <$> decodePowHashNat64 "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
    , testProperty "mainnet20InitialHashTarget json deserialization" $
        Just mainnet20InitialHashTarget === (HashTarget . (4 *) <$> decodePowHashNatJson "DOordl9cgfs4ZTBdFnbjRW5th-hW-pL33DIAAAAAAAA")
    , testProperties "mainnet old chains" $
        forChain Mainnet01 maxTarget . unsafeChainId <$> [0..9]
    , testProperties "mainnet new chains" $
        forChain Mainnet01 mainnet20InitialHashTarget . unsafeChainId <$> [10..19]

    -- testnet20InitialHashTarget properties
    , testProperty "testnet20InitialHashTarget deserialization" $
        Just testnet20InitialHashTarget === (HashTarget <$> decodePowHashNat64 "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
    , testProperty "testnet20InitialHashTarget json deserialization" $
        Just testnet20InitialHashTarget === (HashTarget <$> decodePowHashNatJson "NZIklpW6xujSPrX3gyhXInfxxOS6JDjkW_GbGwAAAAA")
    , testProperties "testnet old chains" $
        forChain Testnet04 maxTarget . unsafeChainId <$> [0..9]
    , testProperties "testnet new chains" $
        forChain Testnet04 testnet20InitialHashTarget . unsafeChainId <$> [10..19]

    -- Cross check targets to ensure that the values are as expected
    , testProperty "cross check testnet20InitialHashTarget and mainnet20InitialHashTarget" $
        _hashTarget testnet20InitialHashTarget `div` _hashTarget mainnet20InitialHashTarget === PowHashNat 8893
    , testProperty "cross check development and testnet20InitialHashTarget" $
        _hashTarget (genesisBlockTarget Development (unsafeChainId 10)) `div` _hashTarget testnet20InitialHashTarget === PowHashNat 20321
    ]

  where
    forChain v target cid = (show cid, genesisBlockTarget v cid === target)
    decodePowHashNat64 t = runGetS decodePowHashNat =<< decodeB64UrlNoPaddingText t
    decodePowHashNatJson t = decodeStrictOrThrow' @_ @PowHashNat $ "\"" <> t <> "\""

