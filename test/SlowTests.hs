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

import System.LogLevel
import Test.Tasty

import Data.CAS.RocksDB

-- internal modules

import Chainweb.Graph
import qualified Chainweb.Test.CutDB
import qualified Chainweb.Test.MultiNode
import Chainweb.Version

import qualified Network.X509.SelfSigned.Test

main :: IO ()
main = withTempRocksDb "chainweb-tests" (defaultMain . suite)

loglevel :: LogLevel
loglevel = Warn

suite :: RocksDb -> TestTree
suite rdb = testGroup "ChainwebSlowTests"
    [ Chainweb.Test.MultiNode.test loglevel (TimedConsensus petersonChainGraph twentyChainGraph) 10 120
    , testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    ]
