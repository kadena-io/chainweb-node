{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Test.ChainDB.Sync ( tests ) where

import Chainweb.ChainDB (snapshot, height, copy, highest)
import Chainweb.ChainDB.Sync
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (withServer, withDB, insertN)
import Control.Monad (void)
import Refined (refineTH)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Single-Chain Sync"
    [ testCase "Two length-1 chains" noopSync
    , testCase "Syncing a fresh node" newNode
    , testCase "Syncing an old node" oldNode
    ]

diam :: Diameter
diam = Diameter $$(refineTH 6)

cid :: ChainId
cid = testChainId 0

-- | Syncing a length-1 chain to another length-1 chain should have no effect.
--
noopSync :: Assertion
noopSync = withDB cid $ \_ db -> withServer [(cid, db)] $ \env -> do
    sync diam env db
    s <- snapshot db
    height s @?= 0

-- | Simulates a brand new node syncing everything from a peer.
--
newNode :: Assertion
newNode = withDB cid $ \g db -> do
    peer <- copy db
    void $ insertN 10 g peer
    snapshot db >>= \ss -> height ss @?= 0
    withServer [(cid, peer)] $ \env -> do
        sync diam env db
        snapshot db >>= \ss -> height ss @?= 10

-- | Simulates an older node that hasn't been sync'd in a while.
--
oldNode :: Assertion
oldNode = withDB cid $ \g db -> do
    void $ insertN 10 g db
    peer <- copy db
    h <- highest <$> snapshot peer
    void $ insertN 90 h peer
    withServer [(cid, peer)] $ \env -> do
        sync diam env db
        snapshot db >>= \ss -> height ss @?= 100
