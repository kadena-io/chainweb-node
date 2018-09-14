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

import           Chainweb.BlockHash (BlockHash)
import           Chainweb.BlockHeader (BlockHeader(..), genesisBlockHeader)
import qualified Chainweb.ChainDB as DB
import           Chainweb.ChainId (ChainId, testChainId)
import           Chainweb.Graph (toChainGraph)
import           Chainweb.Version (ChainwebVersion(..))
import           Control.Lens ((#), (&), (.~))
import           Control.Monad (void)
import           Data.DiGraph (singleton)
import           Data.Generics.Product (field, typed)
import           Data.Generics.Sum (_Ctor)
import           Orphans ()
import           Test.QuickCheck
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
      , testCase "Single Insertion + Sync" insertItem
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

insertItem :: Assertion
insertItem = do
  (g, db) <- chaindb
  ss <- DB.snapshot db
  bh <- DB.entry . f (_blockHash g) <$> generate arbitrary
  -- pPrintNoColor bh
  DB.insert bh ss >>= void . DB.syncSnapshot
  DB.closeChainDb db
  where f :: BlockHash -> BlockHeader -> BlockHeader
        f g e = e & field @"_blockHeight" .~ (_Ctor @"BlockHeight" # 1)
                  & field @"_blockParent" .~ g
                  & field @"_blockChainId" .~ chainId
                  & field @"_blockMiner" . field @"_nodeIdChain" .~ chainId
                  & field @"_blockHash" . typed @ChainId .~ chainId

-- | Given a `BlockHeader` of some initial parent, generate an infinite stream
-- of `BlockHeader`s which form a legal chain.
headers :: BlockHeader -> IO [BlockHeader]
headers h = tail . scanl f h <$> generate infiniteList
  where f :: BlockHeader -> BlockHeader -> BlockHeader
        f prev curr = curr & field @"_blockHeight" .~ (_Ctor @"BlockHeight" # 1)
                           & field @"_blockParent" .~ _blockHash prev
                           & field @"_blockChainId" .~ _blockChainId prev
                           & field @"_blockMiner" . field @"_nodeIdChain" .~ _blockChainId prev
                           & field @"_blockHash" . typed @ChainId .~ _blockChainId prev
