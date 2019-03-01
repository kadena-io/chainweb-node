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

import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHeader (BlockHeader(..), Nonce(..))
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
testnet00Chains = map snd . sortBy (compare `on` fst) $ HM.toList testnet00Geneses

allBlocksParse :: Assertion
allBlocksParse = map _blockHeight testnet00Chains @?= replicate 10 0

-- | Does the Genesis Block mining logic continue to create blocks identical to
-- what was hardcoded?
--
regeneration :: ChainwebVersion -> [BlockHeader] -> Assertion
regeneration v bs = void $ zipWithM (\cid chain -> mine cid @?= chain) [0..] bs
  where
    mine c = mineGenesis v (testChainId c) (genesisTime v $ testChainId c) (Nonce 0)
