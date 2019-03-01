-- |
-- Module: Chainweb.Test.BlockHeader.Genesis
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.BlockHeader.Genesis
  ( tests
  ) where

import Control.Monad (void, zipWithM)

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHeader (BlockHeader(..), Nonce(..), genesisTime)
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId (testChainId)
import Chainweb.Miner.Genesis (mineGenesis)
import Chainweb.Version (ChainwebVersion(..))

---

tests :: TestTree
tests = testGroup "Genesis Blocks"
    [ testGroup "Testnet00"
          [ testCase "All blocks parse" allBlocksParse
          , testCase "Regeneration" $ regeneration Testnet00 testnet00Chains
          ]
    ]

testnet00Chains :: [BlockHeader]
testnet00Chains =
    [ testnet00C0
    , testnet00C1
    , testnet00C2
    , testnet00C3
    , testnet00C4
    , testnet00C5
    , testnet00C6
    , testnet00C7
    , testnet00C8
    , testnet00C9 ]


allBlocksParse :: Assertion
allBlocksParse = map _blockHeight testnet00Chains @?= replicate 10 0

-- | Does the Genesis Block mining logic continue to create blocks identical to
-- what was hardcoded?
--
regeneration :: ChainwebVersion -> [BlockHeader] -> Assertion
regeneration v bs = void $ zipWithM (\cid chain -> mine cid @?= chain) [0..] bs
  where
    mine c = mineGenesis v (testChainId c) (genesisTime v $ testChainId c) (Nonce 0)
