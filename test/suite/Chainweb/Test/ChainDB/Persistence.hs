{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.ChainDB.Persistence
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.ChainDB.Persistence
( tests
) where

import Control.Monad (void)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Data.Align (padZipWith)
import Data.DiGraph (singleton)
import Data.Foldable (foldlM)

import qualified Streaming.Prelude as S

import System.Path (fromAbsoluteFilePath)

import Test.Tasty
import Test.Tasty.HUnit

import UnliftIO.Exception (bracket)

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), genesisBlockHeader, testBlockHeaders)
import qualified Chainweb.ChainDB as DB
import Chainweb.ChainDB.Persist (persist, dbEntries, fileEntries)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Graph (toChainGraph)
import Chainweb.Version (ChainwebVersion(..))

---

tests :: TestTree
tests = testGroup "ChainDB/Persistence"
    [ testGroup "Basic Interaction"
        [ testCase "Initialization + Shutdown" $ chaindb >>= DB.closeChainDb . snd
        , testCase "10 Insertions + Sync" insertItems
        , testCase "Reinserting the Genesis Block is a no-op" reinsertGenesis
        ]
    , testGroup "Encoding round-trips"
        [ testCase "Fresh ChainDb (only genesis)" onlyGenesis
        , testCase "Multiple Entries" manyBlocksWritten
        ]
    ]

chainId :: ChainId
chainId = testChainId 0

-- | Borrowed from TrivialSync.hs
chaindb :: IO (BlockHeader, DB.ChainDb)
chaindb = (genesis,) <$> DB.initChainDb (DB.Configuration genesis)
  where
    graph = toChainGraph (const chainId) singleton
    genesis = genesisBlockHeader Test graph chainId

-- | Given a function that accepts a Genesis Block and
-- an initialized `DB.ChainDb`, perform some action
-- and cleanly close the DB.
withDB :: (BlockHeader -> DB.ChainDb -> IO ()) -> IO ()
withDB = bracket chaindb (DB.closeChainDb . snd) . uncurry

insertN :: Int -> BlockHeader -> DB.ChainDb -> IO DB.Snapshot
insertN n g db = do
    ss <- DB.snapshot db
    let bhs = map DB.entry . take n $ testBlockHeaders g
    foldlM (\ss' bh -> DB.insert bh ss') ss bhs >>= DB.syncSnapshot

insertItems :: Assertion
insertItems = withDB $ \g db -> void (insertN 10 g db)

-- | This test represents a critical invariant: that reinserting the genesis block
-- has no effect on the Database. In particular, the persistence function
-- `restore` assumes this to be true, and likewise `persist` will also write
-- the genesis block to file, assuming `restore` will ignore it upon read.
reinsertGenesis :: Assertion
reinsertGenesis = withDB $ \g db -> do
    ss <- DB.snapshot db
    ss' <- DB.insert (DB.entry g) ss
    void $ DB.syncSnapshot ss'
    l <- S.length_ $ dbEntries db
    l @?= 1

-- | Persisting a freshly initialized `DB.ChainDb` will successfully read and
-- write its only block, the genesis block.
onlyGenesis :: Assertion
onlyGenesis = withDB $ \g db -> do
    persist fp db
    g' <- runResourceT . S.head_ $ fileEntries @(ResourceT IO) fp
    g' @?= Just (DB.entry g)
  where
    fp = fromAbsoluteFilePath "/tmp/only-genesis"

-- | Write a number of block headers to a database, persist that database,
-- reread it, and compare. Guarantees:
--
--  * The DB and its persistence will have the same contents in the same order.
--  * The first block streamed from both the DB and the file will be the genesis.
manyBlocksWritten :: Assertion
manyBlocksWritten = withDB $ \g db -> do
    void $ insertN len g db
    persist fp db
    fromDB <- S.toList_ . S.map DB.dbEntry $ dbEntries db
    fromFi <- runResourceT . S.toList_ . S.map DB.dbEntry $ fileEntries fp
    length fromDB @?= len + 1
    length fromFi @?= len + 1
    head fromDB @?= g
    head fromFi @?= g
    let b = and $ padZipWith (==) fromDB fromFi
    assertBool "Couldn't write many blocks" b
  where
    fp = fromAbsoluteFilePath "/tmp/many-blocks-written"
    len = 10
