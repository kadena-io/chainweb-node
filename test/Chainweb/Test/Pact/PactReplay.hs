{-# language TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay (tests) where

import Data.Foldable

import System.Directory
import System.IO.Extra

import Test.Tasty

-- chainweb imports

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Version


tests :: ScheduledTest
tests =
    ScheduledTest label $
    withRocksTemporaryFile $ \_sqlfile ->
    withResource newPayloadDb killPdb $ \iopdb ->
    withRocksResource $ \rocksIO ->
    withPopulatedDBs iopdb (createTestBlockHeaderDb rocksIO) $ \_iodbs ->
      testGroup label []
  where
    label = "Chainweb.Test.Pact.PactReplay"
    killPdb _ = return ()
    v :: ChainwebVersion
    v = Testnet01
    cid :: ChainId
    cid = unsafeChainId 0
    createTestBlockHeaderDb iordb = do
      rdb <- iordb
      testBlockHeaderDb rdb (genesisBlockHeader v cid)

-- THESE SHOULD BE MOVED TO UTILS
withRocksTemporaryFile :: (IO FilePath -> TestTree) -> TestTree
withRocksTemporaryFile = withResource (fst <$> newTempFile) removeFile

withPopulatedDBs :: PayloadCas cas => IO (PayloadDb cas) -> IO BlockHeaderDb -> (IO (PayloadDb cas, BlockHeaderDb) -> TestTree) -> TestTree
withPopulatedDBs iopdb iobhdb = withResource ((,) <$> (populatepayloaddb <$> iopdb >> iopdb) <*> (populatebhdb <$> iobhdb >> iobhdb)) (const $ return ())
  where
    populatebhdb :: BlockHeaderDb -> IO ()
    populatebhdb bhdb = do
        traverse_ (insert bhdb) ms
        traverse_ (insert bhdb) $ drop 1 $ fork1blockheaders b1
        traverse_ (insert bhdb) $ drop 1 $ fork2blockheaders b2
      where (ms,b1,b2) = mainlineblockheaders undefined
    populatepayloaddb = undefined mainlinepayloads fork1payloads fork2payloads

mainlinepayloads :: [PayloadWithOutputs]
mainlinepayloads = []

mainlineblockheaders :: BlockHeader -> ([BlockHeader], BlockHeader, BlockHeader)
mainlineblockheaders genesis = (bs, bs !! 1, bs !! 2)
  where
    bs = testBlockHeaders genesis

-- forked off blockheight 1
fork1payloads :: [PayloadWithOutputs]
fork1payloads = []

-- forked off blockheight 1
fork1blockheaders :: BlockHeader -> [BlockHeader]
fork1blockheaders = testBlockHeaders

-- forked off blockheight 2
fork2payloads :: [PayloadWithOutputs]
fork2payloads = []

-- forked off blockheight 2
fork2blockheaders :: BlockHeader -> [BlockHeader]
fork2blockheaders = testBlockHeaders

_unitTest :: TestTree
_unitTest = undefined
