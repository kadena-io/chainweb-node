{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Test.TreeDB.Sync ( tests ) where

import Control.Monad (void)

import Refined (refineTH)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeaderDB (copy)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (insertN, withDB, withSingleChainServer)
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB
import Chainweb.TreeDB.Sync

tests :: TestTree
tests = testGroup "Single-Chain Sync"
    [ testCase "Two identical length-1 chains" noopSingletonSync
    , testCase "Two identical length-N chains" noopLongSync
    , testCase "Syncing a newer node (no-op)" noopNewerNode
    , testCase "Syncing a fresh node" newNode
    , testCase "Syncing an old node" oldNode
    ]

diam :: Diameter
diam = Diameter $$(refineTH 6)

cid :: ChainId
cid = testChainId 0

-- | Syncing a length-1 chain to another length-1 chain should have no effect.
--
noopSingletonSync :: Assertion
noopSingletonSync = withDB cid $ \g db -> withSingleChainServer [(cid, db)] [] $ \env -> do
    sync diam db $ RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
    maxRank db >>= (@?= 0)

-- | Simulates an up-to-date node querying another for updates,
-- and finding none.
--
noopLongSync :: Assertion
noopLongSync = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    withSingleChainServer [(cid, peer)] [] $ \env -> do
        sync diam db $ RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 10)

-- | Simulates a node that queries an /older/ node for updates.
--
noopNewerNode :: Assertion
noopNewerNode = withDB cid $ \g peer -> do
    void $ insertN 10 g peer
    db <- copy peer
    h <- maxHeader db
    void $ insertN 90 h db
    withSingleChainServer [(cid, peer)] [] $ \env -> do
        let remote = RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
        sync diam db remote
        maxRank db >>= (@?= 100)
        maxRank remote >>= (@?= 10)

-- | Simulates a brand new node syncing everything from a peer.
--
newNode :: Assertion
newNode = withDB cid $ \g db -> do
    peer <- copy db
    void $ insertN 10 g peer
    maxRank db >>= (@?= 0)
    withSingleChainServer [(cid, peer)] [] $ \env -> do
        sync diam db $ RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 10)

-- | Simulates an older node that hasn't been sync'd in a while.
--
oldNode :: Assertion
oldNode = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    h <- maxHeader peer
    void $ insertN 90 h peer
    withSingleChainServer [(cid, peer)] [] $ \env -> do
        sync diam db $ RemoteDb env (_blockChainwebVersion g) (_blockChainId g)
        maxRank db >>= (@?= 100)
