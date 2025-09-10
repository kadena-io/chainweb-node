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
        [ testCaseSteps "simple 1" (test0 rdb)
        , testCaseSteps "simple 2" (test1 rdb)
        , testCaseSteps "simple 3" (test2 rdb)
        , testCaseSteps "simple 5" (test3 rdb)
        , testCaseSteps "Skippping: max bound 1" $ test4 rdb
        , testCaseSteps "Skippping: depth 10" $ test5 rdb
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
    -> (String -> IO ())
    -> Natural
    -> Int
    -> String
    -> IO ()
singleForkTest rdb step d expect msg = runResourceT $ do
    cdb <- withDbs rdb
    liftIO $ do
        let db = cdb ^?! cutDbBlockHeaderDb cid
        let h = toyGenesis cid
        (f0, f1) <- createForks db h
        let f0Cut = cutToCutHashes Nothing $ unsafeMkCut (HM.singleton cid (last f0))
        addCutHashes cdb f0Cut
        atomically $ do
            curCut <- _cutStm cdb
            guard (cutToCutHashes Nothing curCut == f0Cut)
        n <- pruneForks logg cdb Prune d
        assertHeaders db f0
        when (expect > 0) $ assertPrunedHeaders db f1
        assertEqual msg expect n
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

test0 :: RocksDb -> (String -> IO ()) -> IO ()
test0 rdb step = withVersion toyVersion $ singleForkTest rdb step 1 5 "5 block headers pruned"

test1 :: RocksDb -> (String -> IO ()) -> IO ()
test1 rdb step = withVersion toyVersion $ singleForkTest rdb step 2 5 "5 block headers pruned"

test2 :: RocksDb -> (String -> IO ()) -> IO ()
test2 rdb step = withVersion toyVersion $ singleForkTest rdb step 4 5 "5 block headers pruned"

test3 :: RocksDb -> (String -> IO ()) -> IO ()
test3 rdb step = withVersion toyVersion $ singleForkTest rdb step 5 0 "0 block headers pruned"

test4 :: RocksDb -> (String -> IO ()) -> IO ()
test4 rdb step = withVersion toyVersion $ singleForkTest rdb step 9 0 "Skipping: max bound 1"

test5 :: RocksDb -> (String -> IO ()) -> IO ()
test5 rdb step = withVersion toyVersion $ singleForkTest rdb step 10 0
    "Skipping: depth == max block height"

failTest :: RocksDb -> Natural -> (String -> IO ()) -> IO ()
failTest rio n step = withVersion toyVersion $ runResourceT $ do
    cdb <- withDbs rio
    liftIO $ do
        let db = cdb ^?! cutDbBlockHeaderDb cid
        let h = toyGenesis cid
        (f0, _) <- createForks db h
        delHdr db $ f0 !! (int n)
        try (pruneForks logg cdb Prune 2) >>= \case
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
testFullGc rdb step = withVersion toyVersion $ runResourceT $ do
    cdb <- withDbs rdb
    let db = cdb ^?! cutDbBlockHeaderDb cid
    let h = toyGenesis cid
    liftIO $ do
        (f0, f1) <- createForks db h
        -- fullGc logger rdb toyVersion
        assertHeaders db f0
        assertPrunedHeaders db f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)

testPruneWithChecks :: RocksDb -> (String -> IO ()) -> IO ()
testPruneWithChecks rdb step = withVersion toyVersion $ runResourceT $ do
    cdb <- withDbs rdb
    let db = cdb ^?! cutDbBlockHeaderDb cid
    let h = toyGenesis cid
    liftIO $ do
        (f0, f1) <- createForks db h
        -- pruneAllChains logger rdb toyVersion
        assertHeaders db f0
        assertPrunedHeaders db f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)
