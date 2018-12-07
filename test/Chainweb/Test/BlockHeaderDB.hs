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

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (insertN, toyBlockHeaderDb, withDB)
import Chainweb.TreeDB


tests :: TestTree
tests = testGroup "Basic Interaction"
    [ testCase "Initialization + Shutdown" $ toyBlockHeaderDb chainId0 >>= closeBlockHeaderDb . snd
    , testCase "10 Insertions + Sync" insertItems
    , testCase "Reinserting the Genesis Block is a no-op" reinsertGenesis
    , testCase "height" correctHeight
    , testCase "copy" copyTest
    ]

chainId0 :: ChainId
chainId0 = testChainId 0

insertItems :: Assertion
insertItems = withDB chainId0 $ \g db -> insertN 10 g db

-- | This test represents a critical invariant: that reinserting the genesis block
-- has no effect on the Database. In particular, the persistence function
-- `restore` assumes this to be true, and likewise `persist` will also write
-- the genesis block to file, assuming `restore` will ignore it upon read.
--
reinsertGenesis :: Assertion
reinsertGenesis = withDB chainId0 $ \g db -> do
    insert db g
    l <- S.length_ $ entries db Nothing Nothing Nothing Nothing
    l @?= 1

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
