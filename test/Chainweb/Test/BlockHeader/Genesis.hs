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

import Test.Tasty
import Test.Tasty.HUnit

-- internal imports

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeader.Genesis

---

tests :: TestTree
tests = testGroup "Genesis Blocks"
    [ testGroup "Testnet00"
          [ testCase "All blocks parse" allBlocksParse
          ]
    ]

allBlocksParse :: Assertion
allBlocksParse = map _blockHeight bs @?= replicate 10 0
  where
    bs = [ testnet00C0
         , testnet00C1
         , testnet00C2
         , testnet00C3
         , testnet00C4
         , testnet00C5
         , testnet00C6
         , testnet00C7
         , testnet00C8
         , testnet00C9
         ]
