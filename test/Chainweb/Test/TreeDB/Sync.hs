{-# LANGUAGE TypeApplications #-}
module Chainweb.Test.TreeDB.Sync ( tests ) where

import Control.Monad (void)

import Data.Semigroup (Min(..))

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), BlockHeight(..))
import Chainweb.BlockHeaderDB (BlockHeaderDb, copy)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Mempool.Mempool (MockTx)
import Chainweb.RestAPI
import Chainweb.Test.Utils (insertN, withDB, withChainServer)
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB
import Chainweb.TreeDB.Sync

import Data.CAS.HashMap
import Data.LogMessage

tests :: TestTree
tests = testGroup "Single-Chain Sync"
    [ testGroup "Syncing from a RemoteDb"
      [ testCase "Two identical length-N chains" noopLongSync
      , testCase "Two identical length-1 chains" noopSingletonSync
      , testCase "Syncing a newer node (no-op)" noopNewerNode
      , testCase "Syncing a fresh node" newNode
      , testCase "Syncing an old node" oldNode
      ]
    , testGroup "minHeight"
      [ testCase "Just Genesis" $ minHeight (BlockHeight 0) diam @?= MinRank (Min 0)
      , testCase "Short Chain"  $ minHeight (BlockHeight 5) diam @?= MinRank (Min 0)
      , testCase "Long Chain"   $ minHeight (BlockHeight 1000) diam @?= MinRank (Min 988)
      ]
    ]

diam :: Depth
diam = Depth 6

cid :: ChainId
cid = testChainId 0

blockHeaderDbs :: [(ChainId, BlockHeaderDb)] -> ChainwebServerDbs MockTx HashMapCas
blockHeaderDbs chainDbs = emptyChainwebServerDbs
    { _chainwebServerBlockHeaderDbs = chainDbs
    }

-- | Syncing a length-1 chain to another length-1 chain should have no effect.
--
noopSingletonSync :: Assertion
noopSingletonSync = withDB cid $ \g db -> do
    withChainServer (blockHeaderDbs [(cid, db)]) $ \env -> do
        linearSync diam db . PeerTree $ RemoteDb env aNoLog (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 0)

-- | Simulates an up-to-date node querying another for updates,
-- and finding none.
--
noopLongSync :: Assertion
noopLongSync = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    withChainServer (blockHeaderDbs [(cid, peer)]) $ \env -> do
        linearSync diam db . PeerTree $ RemoteDb env aNoLog (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 10)

-- | Simulates a node that queries an /older/ node for updates.
--
noopNewerNode :: Assertion
noopNewerNode = withDB cid $ \g peer -> do
    void $ insertN 10 g peer
    db <- copy peer
    h <- maxHeader db
    void $ insertN 90 h db
    withChainServer (blockHeaderDbs [(cid, peer)]) $ \env -> do
        let remote = PeerTree $ RemoteDb env aNoLog (_blockChainwebVersion g) (_blockChainId g)
        linearSync diam db remote
        maxRank db >>= (@?= 100)
        maxRank remote >>= (@?= 10)

-- | Simulates a brand new node syncing everything from a peer.
--
newNode :: Assertion
newNode = withDB cid $ \g db -> do
    peer <- copy db
    void $ insertN 10 g peer
    maxRank db >>= (@?= 0)
    withChainServer (blockHeaderDbs [(cid, peer)]) $ \env -> do
        linearSync diam db . PeerTree $ RemoteDb env aNoLog (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 10)

-- | Simulates an older node that hasn't been sync'd in a while.
--
oldNode :: Assertion
oldNode = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    h <- maxHeader peer
    void $ insertN 90 h peer
    maxRank db >>= (@?= 10)
    maxRank peer >>= (@?= 100)
    withChainServer (blockHeaderDbs [(cid, peer)]) $ \env -> do
        linearSync diam db . PeerTree $ RemoteDb env aNoLog (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 100)
