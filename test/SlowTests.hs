{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Slow Tests
--

module SlowTests ( main ) where

import Data.CAS.RocksDB

import System.LogLevel
import Test.Tasty

-- internal modules

import Chainweb.Graph
import Chainweb.Test.Utils
import Chainweb.Version
import qualified Chainweb.Test.Mempool.RestAPI
import qualified Chainweb.Test.MultiNode
import qualified Chainweb.Test.Pact.RemotePactTest
import qualified Chainweb.Test.RestAPI
import qualified Chainweb.Test.SPV

import qualified Network.X509.SelfSigned.Test

main :: IO ()
main = withTempRocksDb "slow-tests" (defaultMain . suite)

suite :: RocksDb -> TestTree
suite rdb = testGroup "ChainwebSlowTests"
    [ Chainweb.Test.MultiNode.test Warn (TimedConsensus petersonChainGraph) 10 150
    , testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    , Chainweb.Test.RestAPI.tests rdb
    , Chainweb.Test.SPV.tests rdb
    , Chainweb.Test.Mempool.RestAPI.tests
    , _schTest $ Chainweb.Test.Pact.RemotePactTest.tests rdb
    ]
