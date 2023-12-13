{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

import Chainweb.Graph
import Chainweb.Storage.Table.RocksDB (RocksDb, withTempRocksDb)
import Chainweb.Test.MultiNode qualified
import Chainweb.Test.TestVersions
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Network.X509.SelfSigned.Test qualified
import System.LogLevel
import Chainweb.Test.Pact.PactSingleChainTest qualified
import Test.Tasty

main :: IO ()
main = do
  withTempRocksDb "chainweb-slow-tests" $ \rdb ->
    runResourceT $ do
      liftIO $ defaultMain (suite rdb)

loglevel :: LogLevel
loglevel = Warn

suite :: RocksDb -> TestTree
suite rdb = testGroup "ChainwebSlowTests"
    [ Chainweb.Test.MultiNode.test loglevel (timedConsensusVersion petersonChainGraph twentyChainGraph) 10 30
    , Chainweb.Test.MultiNode.replayTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6
    , Chainweb.Test.MultiNode.compactAndResumeTest loglevel (fastForkingCpmTestVersion pairChainGraph) 6
    , testGroup "Network.X05.SelfSigned.Test"
        [ Network.X509.SelfSigned.Test.tests
        ]
    , Chainweb.Test.Pact.PactSingleChainTest.compactionTransactionIndex rdb
    ]
