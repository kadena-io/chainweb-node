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

import Control.Monad (zipWithM_)

import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sort, sortBy)

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHeader (BlockHeader(..), Nonce(..))
import Chainweb.BlockHeader.Genesis
import Chainweb.Miner.Genesis (mineGenesis)
import Chainweb.Version (ChainwebVersion(..), chainIds)

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
regeneration v bs = zipWithM_ (\cid chain -> mine cid @?= chain) cids bs
  where
    cids = sort $ toList $ chainIds v
    mine c = mineGenesis v c (genesisTime v) (Nonce 0)
