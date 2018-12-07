{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Test.TreeDB.Sync ( tests ) where

import Control.Monad (void)

import Refined (refineTH)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeaderDB (copy)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (insertN, withDB, withServer)
import Chainweb.TreeDB
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
noopSingletonSync = withDB cid $ \_ db -> withServer [(cid, db)] [] $ \env -> do
    sync diam env db
    maxRank db >>= (@?= 0)

-- | Simulates an up-to-date node querying another for updates,
-- and finding none.
--
noopLongSync :: Assertion
noopLongSync = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    withServer [(cid, peer)] [] $ \env -> do
        sync diam env db
        maxRank db >>= (@?= 10)

-- | Simulates a node that queries an /older/ node for updates.
--
noopNewerNode :: Assertion
noopNewerNode = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    h <- maxHeader peer
    void $ insertN 90 h peer
    withServer [(cid, db)] [] $ \env -> do
        sync diam env peer
        maxRank peer >>= (@?= 100)
        maxRank db   >>= (@?= 10)

-- | Simulates a brand new node syncing everything from a peer.
--
newNode :: Assertion
newNode = withDB cid $ \g db -> do
    peer <- copy db
    void $ insertN 10 g peer
    maxRank db >>= (@?= 0)
    withServer [(cid, peer)] [] $ \env -> do
        sync diam env db
        maxRank db >>= (@?= 10)

-- | Simulates an older node that hasn't been sync'd in a while.
--
oldNode :: Assertion
oldNode = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    h <- maxHeader peer
    void $ insertN 90 h peer
    withServer [(cid, peer)] [] $ \env -> do
        sync diam env db
        maxRank db >>= (@?= 100)
