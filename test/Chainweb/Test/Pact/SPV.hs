module Chainweb.Test.Pact.SPV ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Pact.Types
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Version

import Data.CAS.RocksDB
import Data.LogMessage

tests :: RocksDb -> TestTree
tests rdb = testGroup "pactServiceSpv"
    [ testCase "inmemorySpvTransaction" (pactSpvTransaction rdb version)
    ]

version :: ChainwebVersion
version = TimedCPM petersonChainGraph

noop :: LogFunction
noop _ _ = pure ()

mempoolAccess :: ChainId -> MemPoolAccess
mempoolAccess = undefined

pactSpvTransaction :: RocksDb -> ChainwebVersion -> IO ()
pactSpvTransaction rdb v = do
    pact <- testWebPactExecutionService v mempoolAccess
    withTestCutDb rdb v 0 pact noop $ \_cutDb -> do
      undefined
