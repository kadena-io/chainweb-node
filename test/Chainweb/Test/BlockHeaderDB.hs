{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Test the `BlockHeaderDb` API.
--
module Chainweb.Test.BlockHeaderDB
( tests
) where


import Data.Semigroup (Min(..))

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules


import Chainweb.BlockHeader (BlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.TreeDB (RunStyle(..), treeDbInvariants)
import Chainweb.Test.Utils (insertN, toyBlockHeaderDb, withDB)
import Chainweb.TreeDB


tests :: TestTree
tests = testGroup "Unit Tests"
    [ testGroup "Basic Interaction"
      [ testCase "Initialization + Shutdown" $ toyBlockHeaderDb chainId0 >>= closeBlockHeaderDb . snd
      ]
    , testGroup "Insertion"
      [ testCase "10 Insertions" insertItems
      ]
    , testGroup "TreeDb Instance"
      [ testCase "rank filtering" rankFiltering
      ]
    , testGroup "Misc."
      [ testCase "height" correctHeight
      , testCase "copy" copyTest
      ]
    , treeDbInvariants withDb Parallel
    ]

withDb :: BlockHeader -> (BlockHeaderDb -> IO Bool) -> IO Bool
withDb h f = initBlockHeaderDb (Configuration h) >>= \db -> f db <* closeBlockHeaderDb db

chainId0 :: ChainId
chainId0 = testChainId 0

insertItems :: Assertion
insertItems = withDB chainId0 $ \g db -> insertN 10 g db

correctHeight :: Assertion
correctHeight = withDB chainId0 $ \g db -> do
    maxRank db >>= \r -> r @?= 0
    insertN 10 g db
    maxRank db >>= \r -> r @?= 10

copyTest :: Assertion
copyTest = withDB chainId0 $ \g db -> do
    db' <- copy db
    maxRank db  >>= \r -> r @?= 0
    maxRank db' >>= \r -> r @?= 0
    insertN 10 g db'
    maxRank db  >>= \r -> r @?= 0
    maxRank db' >>= \r -> r @?= 10
    insertN 20 g db
    maxRank db  >>= \r -> r @?= 20
    maxRank db' >>= \r -> r @?= 10

rankFiltering :: Assertion
rankFiltering = withDB chainId0 $ \g db -> do
    insertN 100 g db
    l <- S.length_ $ entries db Nothing Nothing (Just . MinRank $ Min 90) Nothing
    l @?= 11
