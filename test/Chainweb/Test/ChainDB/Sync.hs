{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Test.ChainDB.Sync ( tests ) where

import Chainweb.ChainDB.Sync
import Chainweb.Test.Utils (withServer, withDB, chainId)
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
noopSync :: Assertion
noopSync = withDB $ \_ db -> withServer [(chainId, db)] $ \env -> sync diam env db
