{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Test.ChainDB.Sync ( tests ) where

import Chainweb.ChainDB.Sync
import Chainweb.ChainId (testChainId)
import Chainweb.Test.Utils (withServer, withDB)
import Refined (refineTH)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Single-Chain Sync"
    [ testCase "Two length-1 chains" noopSync
    ]

diam :: Diameter
diam = Diameter $$(refineTH 6)

-- | Syncing a length-1 chain to another length-1 chain should have no effect.
--
noopSync :: Assertion
noopSync = withDB cid $ \_ db -> withServer [(cid, db)] $ \env -> sync diam env db
  where
    cid = testChainId 0
