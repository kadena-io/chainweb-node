-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb


module Chainweb.Test.Pact

import Test.Tasty

tests :: TestTree
tests = testGroup "Chainweb Pact tests"
    [ simpleExecTests
    ]

simpleExecTests :: TestTree
simpleExecTests = testGroup "Simple Pact execution tests"

  pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
  let cmdConfig = toCommandConfig pactCfg
  theStore <- initPactCheckpointStore :: IO MapPurePactCheckpointStore
  let env = CheckpointEnv {_cpeCheckpointStore = theStore, _cpeCommandConfig = cmdConfig }
  theState <- DB.initService cmdConfig P.neverLog
  _ <- runRWST execSimple env theState
  return ()

execSimple :: PactT
execSimple = do
