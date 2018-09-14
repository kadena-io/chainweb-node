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

import           Chainweb.BlockHeader (BlockHeader(..), genesisBlockHeader, testBlockHeaders)
import qualified Chainweb.ChainDB as DB
import           Chainweb.ChainDB.Persist (persist, dbEntries, restore, fileEntries)
import           Chainweb.ChainId (ChainId, testChainId)
import           Chainweb.Graph (toChainGraph)
import           Chainweb.Version (ChainwebVersion(..))
import           Control.Monad (void)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.DiGraph (singleton)
import           Data.Foldable (foldlM)
import qualified Streaming.Prelude as S
import           System.Path (fromAbsoluteFilePath)
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
      , testCase "Reinserting the Genesis Block is a no-op" reinsertGenesis
      ]
    , testGroup "Encoding round-trips"
      [ testCase "Fresh ChainDb (only genesis)" onlyGenesis
      -- , testCase "Singleton ChainDb" $ undefined
      -- , testCase "Multiple Entries"  $ undefined
      ]
    ]
  ]

chainId :: ChainId
chainId = testChainId 0

-- | Borrowed from TrivialSync.hs
chaindb :: IO (BlockHeader, DB.ChainDb)
chaindb = (genesis,) <$> DB.initChainDb (DB.Configuration genesis)
  where graph   = toChainGraph (const chainId) singleton
        genesis = genesisBlockHeader Test graph chainId

-- | Given a function that accepts a Genesis Block and
-- an initialized `DB.ChainDb`, perform some action
-- and cleanly close the DB.
withDB :: (BlockHeader -> DB.ChainDb -> IO ()) -> IO ()
withDB f = chaindb >>= \(g, db) -> f g db >> DB.closeChainDb db

insertItems :: Assertion
insertItems = withDB $ \g db -> do
  ss <- DB.snapshot db
  let bhs = map DB.entry . take 10 $ testBlockHeaders g
  -- pPrintNoColor bhs
  foldlM (\ss' bh -> DB.insert bh ss') ss bhs >>= void . DB.syncSnapshot

-- | This test represents a critical invariant: that reinserting the genesis block
-- has no effect on the Database. In particular, the persistence function
-- `restore` assumes this to be true, and likewise `persist` will also write
-- the genesis block to file, assuming `restore` will ignore it upon read.
reinsertGenesis :: Assertion
reinsertGenesis = withDB $ \g db -> do
  ss  <- DB.snapshot db
  ss' <- DB.insert (DB.entry g) ss
  void $ DB.syncSnapshot ss'
  l   <- S.length_ $ dbEntries db
  l @?= 1

onlyGenesis :: Assertion
onlyGenesis = withDB $ \g db -> do
  persist fp db
  g' <- runResourceT . S.head_ $ fileEntries @(ResourceT IO) fp
  g' @?= Just (DB.entry g)
  where fp = fromAbsoluteFilePath "/tmp/only-genesis"
