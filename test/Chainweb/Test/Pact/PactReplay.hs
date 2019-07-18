{-# language TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay (tests) where

import System.Directory
import System.IO.Extra

import Test.Tasty

-- chainweb imports

import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Utils
import Chainweb.Version


tests :: ScheduledTest
tests = ScheduledTest label $
    withRocksTemporaryFile $ \_sqlfile ->
    withResource newPayloadDb killPdb $ \iopdb ->
    withRocksResource $ \rocksIO ->
    withPopulatedDBs iopdb (createTestBlockHeaderDb rocksIO) $ \_iodbs ->
      testGroup label []
  where
    label = "Chainweb.Test.Pact.PactReplay"
    killPdb _ = return ()
    v :: ChainwebVersion
    v = undefined
    cid :: ChainId
    cid = undefined
    createTestBlockHeaderDb iordb = do
      rdb <- iordb
      testBlockHeaderDb rdb (genesisBlockHeader v cid)

-- THESE SHOULD BE MOVED TO UTILS
withRocksTemporaryFile :: (IO FilePath -> TestTree) -> TestTree
withRocksTemporaryFile = withResource (fst <$> newTempFile) removeFile

withPopulatedDBs :: PayloadCas cas => IO (PayloadDb cas) -> IO BlockHeaderDb -> (IO (PayloadDb cas, BlockHeaderDb) -> TestTree) -> TestTree
withPopulatedDBs iopdb iobhdb = withResource ((,) <$> (populatepayloaddb <$> iopdb) <*> (populatebhdb <$> iobhdb)) (const $ return ())
  where
    populatebhdb = undefined
    populatepayloaddb = undefined
