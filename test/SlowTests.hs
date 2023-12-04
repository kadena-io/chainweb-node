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

-- internal modules

import Chainweb.Graph
import qualified Chainweb.Test.MultiNode
import Chainweb.Test.TestVersions

import qualified Network.X509.SelfSigned.Test

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Warn

suite :: TestTree
suite = testGroup "ChainwebSlowTests"
    [ Chainweb.Test.MultiNode.test loglevel (timedConsensusVersion petersonChainGraph twentyChainGraph) 10 30
    , Chainweb.Test.MultiNode.replayTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6
    , Chainweb.Test.MultiNode.compactAndResumeTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6
    , testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    ]
