{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

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

import           Chainweb.BlockHeader (BlockHeader(..), genesisBlockHeader, testBlockHeaders)
import qualified Chainweb.ChainDB as DB
import           Chainweb.ChainId (ChainId, testChainId)
import           Chainweb.Graph (toChainGraph)
import           Chainweb.Version (ChainwebVersion(..))
import           Control.Monad (void)
import           Data.DiGraph (singleton)
import           Data.Foldable (foldlM)
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Text.Pretty.Simple (pPrintNoColor)

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "ChainDB"
    [ testGroup "Basic Interaction"
      [ testCase "Initialization + Shutdown" $ chaindb >>= DB.closeChainDb . snd
      , testCase "10 Insertions + Sync" insertItems
      ]
    , testGroup "Encoding round-trips"
      [ -- testCase "Empty ChainDb" empty
      -- , testCase "Singleton ChainDb" $ undefined
      -- , testCase "Multiple Entries"  $ undefined
      ]
    ]
  ]

-- empty :: Assertion
-- empty = do
--   db <- chaindb
--   1 @?= 1

chainId :: ChainId
chainId = testChainId 0

-- | Borrowed from TrivialSync.hs
chaindb :: IO (BlockHeader, DB.ChainDb)
chaindb = (genesis,) <$> DB.initChainDb (DB.Configuration genesis)
  where graph   = toChainGraph (const chainId) singleton
        genesis = genesisBlockHeader Test graph chainId

insertItems :: Assertion
insertItems = do
  (g, db) <- chaindb
  ss <- DB.snapshot db
  let bhs = map DB.entry . take 10 $ testBlockHeaders g
  -- pPrintNoColor bhs
  foldlM (\ss' bh -> DB.insert bh ss') ss bhs >>= void . DB.syncSnapshot
  DB.closeChainDb db
