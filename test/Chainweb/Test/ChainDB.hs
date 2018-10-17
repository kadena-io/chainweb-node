-- |
-- Module: Chainweb.Test.ChainDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Test the `ChainDb` API.
--
module Chainweb.Test.ChainDB
( tests
) where

import Control.Monad (void)

import qualified Streaming.Prelude as S

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import qualified Chainweb.ChainDB as DB
import Chainweb.ChainDB.Persist (dbEntries)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (toyChainDB, withDB, insertN)


tests :: TestTree
tests = testGroup "Basic Interaction"
    [ testCase "Initialization + Shutdown" $ (toyChainDB chainId0) >>= DB.closeChainDb . snd
    , testCase "10 Insertions + Sync" insertItems
    , testCase "Reinserting the Genesis Block is a no-op" reinsertGenesis
    , testCase "height" correctHeight
    , testCase "copy" copyTest
    ]

chainId0 :: ChainId
chainId0 = testChainId 0

insertItems :: Assertion
insertItems = withDB chainId0 $ \g db -> void (insertN 10 g db)

-- | This test represents a critical invariant: that reinserting the genesis block
-- has no effect on the Database. In particular, the persistence function
-- `restore` assumes this to be true, and likewise `persist` will also write
-- the genesis block to file, assuming `restore` will ignore it upon read.
--
reinsertGenesis :: Assertion
reinsertGenesis = withDB chainId0 $ \g db -> do
    ss <- DB.snapshot db
    ss' <- DB.insert (DB.entry g) ss
    void $ DB.syncSnapshot ss'
    l <- S.length_ $ dbEntries db
    l @?= 1

correctHeight :: Assertion
correctHeight = withDB chainId0 $ \g db -> do
    ss  <- DB.snapshot db
    DB.height ss @?= 0
    ss' <- insertN 10 g db
    DB.height ss' @?= 10

copyTest :: Assertion
copyTest = withDB chainId0 $ \g db -> do
    db' <- DB.copy db
    DB.snapshot db  >>= \ss -> DB.height ss @?= 0
    DB.snapshot db' >>= \ss -> DB.height ss @?= 0
    ss' <- insertN 10 g db'
    DB.snapshot db  >>= \ss -> DB.height ss @?= 0
    DB.height ss' @?= 10
    ss'' <- insertN 20 g db
    DB.height ss'' @?= 20
    DB.snapshot db' >>= \ss -> DB.height ss @?= 10
