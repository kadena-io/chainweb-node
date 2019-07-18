module Chainweb.Test.Pact.PactReplay (tests) where

import System.Directory
import System.IO.Extra

import Test.Tasty

-- chainweb imports

import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Test.Utils


tests :: ScheduledTest
tests = ScheduledTest label $
    withRocksTemporaryFile $ \_sqlfile ->
    withResource newPayloadDb killPdb $ \_iopdb ->
    withRocksResource $ \_rocksIO ->
      testGroup label []
  where
    label = "Pact replay test"
    killPdb _ = return ()


-- THIS SHOULD BE MOVED TO UTILS
withRocksTemporaryFile :: (IO FilePath -> TestTree) -> TestTree
withRocksTemporaryFile = withResource (fst <$> newTempFile) removeFile
