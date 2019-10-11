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

module Tools.Tests.SlowTests ( main ) where

import System.LogLevel
import Test.Tasty

-- internal modules

import Chainweb.Graph
import qualified Tools.Tests.MultiNode
import Chainweb.Version

import qualified Tools.Tests.Network.X509.SelfSigned.Test

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "ChainwebSlowTests"
    [ Tools.Tests.MultiNode.test Warn (TimedConsensus petersonChainGraph) 10 150
    , testGroup "Network.X05.SelfSigned.Test"
        [ Tools.Tests.Network.X509.SelfSigned.Test.tests
        ]
    ]
