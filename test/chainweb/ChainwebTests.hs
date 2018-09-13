{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: ChainwebTests
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--

module ChainwebTests ( main ) where

-- import           Chainweb.ChainDB (persist)
-- import           System.Path (Path, Absolute, toFilePath)

import           Chainweb.BlockHeader (genesisBlockHeader)
import qualified Chainweb.ChainDB as DB
import           Chainweb.ChainId (testChainId)
import           Chainweb.Graph (toChainGraph)
import           Chainweb.Version (ChainwebVersion(..))
import           Control.Monad (void)
import           Data.DiGraph (singleton)
import           Orphans ()
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "ChainDB"
    [ testGroup "Basic Interaction"
      [ testCase "Initialization + Shutdown" $ chaindb >>= DB.closeChainDb
      , testCase "Single Insertion + Sync" insertItem
      ]
    , testGroup "Encoding round-trips"
      [ testCase "Empty ChainDb" empty
      -- , testCase "Singleton ChainDb" $ undefined
      -- , testCase "Multiple Entries"  $ undefined
      ]
    ]
  ]

empty :: Assertion
empty = do
  db <- chaindb
  1 @?= 1

chaindb :: IO DB.ChainDb
chaindb = DB.initChainDb . DB.Configuration $ genesisBlockHeader Test graph cid
  where graph = toChainGraph (const cid) singleton
        cid   = testChainId 0

insertItem :: Assertion
insertItem = do
  db  <- chaindb
  ss  <- DB.snapshot db
  bh  <- DB.entry <$> generate arbitrary
  DB.insert bh ss >>= void . DB.syncSnapshot
  DB.closeChainDb db
