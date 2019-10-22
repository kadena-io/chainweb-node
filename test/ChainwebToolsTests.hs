-- Chainweb Tools Test Suite
--

module Main (main) where


import Test.Tasty

-- chainweb imports
import Chainweb.Test.Utils
import Chainweb.Test.Tools.TXGen

import Data.CAS.RocksDB

main :: IO ()
main =
    withTempRocksDb "chainweb-tools-tests" $ \rdb ->
    defaultMain
        $ adjustOption adj
        $ testGroup "Chainweb Tools Tests" . schedule Sequential
        $ [toolsTestSuite rdb]
  where
    adj NoTimeout = Timeout (1000000 * 60 * 10) "10m"
    adj x = x

toolsTestSuite :: RocksDb -> ScheduledTest
toolsTestSuite rdb =
  ScheduledTest "Chainweb Tools Tests"
  (Chainweb.Test.Tools.TXGen.tests rdb)
