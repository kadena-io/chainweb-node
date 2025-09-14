{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.BlockHeaderDB.PruneForks
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.BlockHeaderDB.PruneForks
( tests
) where

import Control.Monad
import Control.Monad.Catch

-- import Data.CAS
-- import Data.CAS.RocksDB
import qualified Data.Text as T

import Numeric.Natural

import System.LogLevel
import System.Random

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.BlockHeaderDB.PruneForks
import Chainweb.Logger
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Storage.Table.RocksDB
import Chainweb.CutDB
import Control.Monad.Trans.Resource
import Chainweb.Test.CutDB (withTestCutDb)
import Chainweb.PayloadProvider (ConfiguredPayloadProvider(DisabledPayloadProvider))
import Control.Monad.IO.Class
import Chainweb.Test.Pact.Utils
import Control.Lens
import Chainweb.Storage.Table
import Chainweb.Cut (unsafeMkCut)
import Chainweb.Cut.CutHashes
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Chainweb.Core.Brief

-- -------------------------------------------------------------------------- --
-- Utils

-- | Log level for the tests in this module. Set to 'Debug' for debugging any
-- issues with the test.
--
testLogLevel :: LogLevel
testLogLevel = Warn

-- logg :: Show a => (String -> t) -> a -> T.Text -> t
-- logg s l msg
--     | l >= testLogLevel = s $ "[" <> sshow l <> "] " <> T.unpack msg
--     | otherwise = return ()

withDbs
    :: HasVersion
    => RocksDb
    -> ResourceT IO CutDb
withDbs rdb = do
    -- create unique namespace for each test so that they so that test can
    -- run in parallel.
    -- x <- randomIO :: IO Int
    -- rdb <- rio >>= testRocksDb (sshow x)
    testLogger <- liftIO getTestLogger
    (_, cutDb) <- withTestCutDb rdb id 0 (onAllChains DisabledPayloadProvider) testLogger
    return cutDb
  where

createForks
    :: HasVersion
    => BlockHeaderDb
    -> BlockHeader
    -> IO ([BlockHeader], [BlockHeader])
createForks bdb h = (,)
    <$> insert bdb h (Nonce 1) 10
    <*> insert bdb h (Nonce 2) 5

insert
    :: HasVersion
    => BlockHeaderDb
    -> BlockHeader
    -> Nonce
    -> Natural
    -> IO [BlockHeader]
insert bdb h n l = do
    hdrs <- insertN_ n l h bdb
    return hdrs

cid :: ChainId
cid = unsafeChainId 0

delHdr :: BlockHeaderDb -> BlockHeader -> IO ()
delHdr cdb k = do
    tableDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader k)
    tableDelete (_chainDbRankTable cdb) (view blockHash k)

-- -------------------------------------------------------------------------- --
-- Test cases

tests :: RocksDb -> TestTree
tests rdb =
    testGroup "Chainweb.BlockHeaderDb.PruneForks"
        [ testCaseSteps "simple 1"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 1 5
        , testCaseSteps "simple 2"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 2 5
        , testCaseSteps "simple 3"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 4 5
        , testCaseSteps "simple 4"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 5 0
        , testCaseSteps "skipping: max bound 1"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 9 0
        , testCaseSteps "skipping: depth == max block height"
            $ withVersion (barebonesTestVersion singletonChainGraph)
            $ singleForkTest rdb 10 0
        , testCaseSteps "fail on missing header 5" $ failTest rdb 5
        , testCaseSteps "fail on missing header 6" $ failTest rdb 6
            -- failTest <= 4: succeeds because of second branch
            -- failTest 7: empty upper bound warning
            -- failTest 8: succeeds because deleted block at height 8 is above upper pruning bound

        -- , pruneWithChecksTests rdb
        -- , failPruningChecksTests rdb
        , testCaseSteps "full gc" $ testFullGc rdb
        ]

-- pruneWithChecksTests :: IO RocksDb -> TestTree
-- pruneWithChecksTests rio = testGroup "prune with checks" $ go <$>
--     [ [CheckPayloads]
--     , [CheckPayloadsExist]
--     , [CheckIntrinsic]
--     , [CheckInductive]
--     , [CheckFull]
--     , [minBound .. maxBound]
--     ]
--   where
--     go checks = testCaseSteps (sshow checks) $ testPruneWithChecks rio checks

-- failPruningChecksTests :: IO RocksDb -> TestTree
-- failPruningChecksTests rio = testGroup "fail pruning checks"
--     [ testCaseSteps "CheckIntrinsic" $ failIntrinsicCheck rio [CheckIntrinsic] 7
--     , testCaseSteps "CheckInductive" $ failIntrinsicCheck rio [CheckInductive] 7
--     , testCaseSteps "CheckFull" $ failIntrinsicCheck rio [CheckFull] 7
--     ]

singleForkTest
    :: HasVersion
    => RocksDb
    -> Natural
    -> Int
    -> (String -> IO ())
    -> IO ()
singleForkTest rdb d expect step = runResourceT $ do
    cdb <- withDbs rdb
    liftIO $ do
        let db = cdb ^?! cutDbBlockHeaderDb cid
        let h = genesisBlockHeader cid
        (f0, f1) <- createForks db h
        let f0Cut = cutToCutHashes Nothing $ unsafeMkCut (HM.singleton cid (last f0))
        addCutHashes cdb f0Cut
        atomically $ do
            curCut <- _cutStm cdb
            guard (cutToCutHashes Nothing curCut == f0Cut)
        initialCut <- _cut cdb
        let wbhdb = view cutDbWebBlockHeaderDb cdb
        n <- pruneForks logg initialCut wbhdb Prune d
        assertHeaders db f0
        when (expect > 0) $ assertPrunedHeaders db f1
        assertEqual "" expect n
  where
    logg = genericLogger testLogLevel (step . T.unpack)

assertHeaders :: HasVersion => BlockHeaderDb -> [BlockHeader] -> IO ()
assertHeaders db f =
    unlessM (fmap and $ mapM (tableMember db) $ view blockHash <$> f) $
        assertFailure "missing block header that should not have been pruned"

assertPrunedHeaders :: HasVersion => BlockHeaderDb -> [BlockHeader] -> IO ()
assertPrunedHeaders db f =
    forM_ f $ \bh -> do
        whenM (tableMember db (casKey bh)) $
            assertFailure $ "failed to prune some block header: " <> T.unpack (brief bh)

-- -------------------------------------------------------------------------- --
-- Header Pruning Tests

failTest :: RocksDb -> Natural -> (String -> IO ()) -> IO ()
failTest rio n step = withVersion (barebonesTestVersion singletonChainGraph) $ runResourceT $ do
    cdb <- withDbs rio
    liftIO $ do
        let db = cdb ^?! cutDbBlockHeaderDb cid
        let h = genesisBlockHeader cid
        (f0, _) <- createForks db h
        delHdr db $ f0 !! (int n)
        initialCut <- _cut cdb
        let wbhdb = view cutDbWebBlockHeaderDb cdb
        try (pruneForks logg initialCut wbhdb Prune 2) >>= \case
            Left (InternalInvariantViolation{}) -> return ()
            Right x -> assertFailure
                $ "missing expected InternalInvariantViolation"
                <> ". Instead pruning succeeded and deleted "
                <> sshow x <> " headers"
        return ()
  where
    logg = genericLogger testLogLevel (step . T.unpack)

-- -------------------------------------------------------------------------- --
-- GC Tests

testFullGc :: RocksDb -> (String -> IO ()) -> IO ()
testFullGc rdb step = withVersion (barebonesTestVersion singletonChainGraph) $ runResourceT $ do
    cdb <- withDbs rdb
    let db = cdb ^?! cutDbBlockHeaderDb cid
    let h = genesisBlockHeader cid
    liftIO $ do
        (f0, f1) <- createForks db h
        -- fullGc logger rdb (barebonesTestVersion singletonChainGraph)
        assertHeaders db f0
        assertPrunedHeaders db f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)

testPruneWithChecks :: RocksDb -> (String -> IO ()) -> IO ()
testPruneWithChecks rdb step = withVersion (barebonesTestVersion singletonChainGraph) $ runResourceT $ do
    cdb <- withDbs rdb
    let db = cdb ^?! cutDbBlockHeaderDb cid
    let h = genesisBlockHeader cid
    liftIO $ do
        (f0, f1) <- createForks db h
        -- pruneAllChains logger rdb (barebonesTestVersion singletonChainGraph)
        assertHeaders db f0
        assertPrunedHeaders db f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)
