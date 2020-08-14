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

import Data.CAS
import Data.CAS.RocksDB
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
import Chainweb.Chainweb.PruneChainDatabase
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Utils
import Chainweb.Version

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
    :: IO RocksDb
    -> (RocksDb -> BlockHeaderDb -> PayloadDb RocksDbCas -> BlockHeader -> IO ())
    -> IO ()
withDbs rio inner = do
    -- create unique namespace for each test so that they so that test can
    -- run in parallel.
    x <- randomIO :: IO Int
    rdb <- rio >>= testRocksDb (sshow x)

    let pdb = newPayloadDb rdb
    initializePayloadDb toyVersion pdb
    bracket
        (initBlockHeaderDb (Configuration h rdb))
        closeBlockHeaderDb
        (\bdb -> inner rdb bdb pdb h)
  where
    h = toyGenesis cid

createForks
    :: BlockHeaderDb
    -> PayloadDb RocksDbCas
    -> BlockHeader
    -> IO ([BlockHeader], [BlockHeader])
createForks bdb pdb h = (,)
    <$> insertWithPayloads bdb pdb h (Nonce 1) 10
    <*> insertWithPayloads bdb pdb h (Nonce 2) 5

insertWithPayloads
    :: BlockHeaderDb
    -> PayloadDb RocksDbCas
    -> BlockHeader
    -> Nonce
    -> Natural
    -> IO [BlockHeader]
insertWithPayloads bdb pdb h n l = do
    hdrs <- insertN_ n l h bdb
    forM_ hdrs $ casInsert pdb . testBlockPayload_
    return hdrs

cid :: ChainId
cid = unsafeChainId 0

delHdr :: BlockHeaderDb -> BlockHeader -> IO ()
delHdr cdb k = do
    casDelete (_chainDbCas cdb) (casKey $ RankedBlockHeader k)
    tableDelete (_chainDbRankTable cdb) (_blockHash k)

-- -------------------------------------------------------------------------- --
-- Test cases

tests :: TestTree
tests = withRocksResource $ \rio ->
    testGroup "Chainweb.BlockHeaderDb.PruneForks"
        [ testCaseSteps "simple 1" (test0 rio)
        , testCaseSteps "simple 2" (test1 rio)
        , testCaseSteps "simple 3" (test2 rio)
        , testCaseSteps "simple 5" (test3 rio)
        , testCaseSteps "Skippping: max bound 1" $ test4 rio
        , testCaseSteps "Skippping: depth 10" $ test5 rio
        , testCaseSteps "fail on missing header 5" $ failTest rio 5
        , testCaseSteps "fail on missing header 6" $ failTest rio 6
            -- failTest <= 4: succeeds because of second branch
            -- failTest 7: empty upper bound warning
            -- failTest 8: succeeds because deleted block at height 8 is above upper pruning bound

        , pruneWithChecksTests rio
        , failPruningChecksTests rio
        , testCaseSteps "full gc" $ testFullGc rio
        ]

pruneWithChecksTests :: IO RocksDb -> TestTree
pruneWithChecksTests rio = testGroup "prune with checks" $ go <$>
    [ [CheckPayloads]
    , [CheckPayloadsExist]
    , [CheckIntrinsic]
    , [CheckInductive]
    , [CheckFull]
    , [minBound .. maxBound]
    ]
  where
    go checks = testCaseSteps (sshow checks) $ testPruneWithChecks rio checks

failPruningChecksTests :: IO RocksDb -> TestTree
failPruningChecksTests rio = testGroup "fail pruning checks"
    [ testCaseSteps "CheckPayloadExists" $ failPayloadCheck rio [CheckPayloadsExist] 7
    , testCaseSteps "CheckPayload" $ failPayloadCheck rio [CheckPayloads] 7

    -- deleted transactions from payload
    -- CheckPayloadsExist succeeds for this scenario
    , testCaseSteps "CheckPayload2" $ failPayloadCheck2 rio [CheckPayloads] 7

    , testCaseSteps "CheckIntrinsic" $ failIntrinsicCheck rio [CheckIntrinsic] 7
    , testCaseSteps "CheckInductive" $ failIntrinsicCheck rio [CheckInductive] 7
    , testCaseSteps "CheckFull" $ failIntrinsicCheck rio [CheckFull] 7
    ]

singleForkTest
    :: IO RocksDb
    -> (String -> IO ())
    -> Natural
    -> Int
    -> String
    -> IO ()
singleForkTest rio step d expect msg =
    withDbs rio $ \_rdb db pdb h -> do
        (f0, f1) <- createForks db pdb h
        n <- pruneForks logg db d $ \_ x ->
            logg Info (sshow $ _blockHeight x)
        assertHeaders db f0
        when (expect > 0) $ assertPrunedHeaders db f1
        assertEqual msg expect n
  where
    logg = logFunctionText $ genericLogger testLogLevel (step . T.unpack)

assertHeaders :: BlockHeaderDb -> [BlockHeader] -> IO ()
assertHeaders db f =
    unlessM (fmap and $ mapM (casMember db) $ _blockHash <$> f) $
        assertFailure "missing block header that should not have been pruned"

assertPrunedHeaders :: BlockHeaderDb -> [BlockHeader] -> IO ()
assertPrunedHeaders db f =
    whenM (fmap or $ mapM (casMember db) $ _blockHash <$> f) $
        assertFailure "failed to prune some block header"

assertPayloads :: PayloadDb RocksDbCas -> [BlockHeader] -> IO ()
assertPayloads db f =
    unlessM (fmap and $ mapM (casMember db) $ _blockPayloadHash <$> f) $
        assertFailure "missing block payload that should not have been garbage collected"

-- | This can fail due to the probabilistic nature of the GC algorithms
--
assertPrunedPayloads :: PayloadDb RocksDbCas -> [BlockHeader] -> IO ()
assertPrunedPayloads db f = do
    results <- mapM (casMember db) $ _blockPayloadHash <$> f
    let remained = length (filter id results)
    when (remained > 1) $
        assertFailure $ "failed to garage collect some block payloads"
            <> ". " <> sshow remained <> " remaining"
            <> ". Since can happen due to the probabilistic natures of the garabage collection algorithm"
            <> ". But it should very rare. Try to rerun the test."

-- -------------------------------------------------------------------------- --
-- Header Pruning Tests

test0 :: IO RocksDb -> (String -> IO ()) -> IO ()
test0 rio step = singleForkTest rio step 1 5 "5 block headers pruned"

test1 :: IO RocksDb -> (String -> IO ()) -> IO ()
test1 rio step = singleForkTest rio step 2 5 "5 block headers pruned"

test2 :: IO RocksDb -> (String -> IO ()) -> IO ()
test2 rio step = singleForkTest rio step 4 5 "5 block headers pruned"

test3 :: IO RocksDb -> (String -> IO ()) -> IO ()
test3 rio step = singleForkTest rio step 5 0 "0 block headers pruned"

test4 :: IO RocksDb -> (String -> IO ()) -> IO ()
test4 rio step = singleForkTest rio step 9 0 "Skipping: max bound 1"

test5 :: IO RocksDb -> (String -> IO ()) -> IO ()
test5 rio step = singleForkTest rio step 10 0
    "Skipping: depth == max block height"

failTest :: IO RocksDb -> Natural -> (String -> IO ()) -> IO ()
failTest rio n step = withDbs rio $ \_rdb db pdb h -> do
    (f0, _) <- createForks db pdb h
    delHdr db $ f0 !! (int n)
    try (prune db 2) >>= \case
        Left (InternalInvariantViolation{}) -> return ()
        Right x -> assertFailure
            $ "missing expected InternalInvariantViolation"
            <> ". Instead pruning succeeded and deleted "
            <> sshow x <> " headers"
    return ()
  where
    prune db d = pruneForks logg db d $ \_ h ->
        logg Info (sshow $ _blockHeight h)

    logg = logFunctionText $ genericLogger testLogLevel (step . T.unpack)

-- -------------------------------------------------------------------------- --
-- GC Tests

testFullGc :: IO RocksDb -> (String -> IO ()) -> IO ()
testFullGc rio step = withDbs rio $ \rdb db pdb h -> do
    (f0, f1) <- createForks db pdb h
    fullGc logger rdb toyVersion
    assertHeaders db f0
    assertPrunedHeaders db f1
    assertPayloads pdb f0
    assertPrunedPayloads pdb f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)

testPruneWithChecks :: IO RocksDb -> [PruningChecks] -> (String -> IO ()) -> IO ()
testPruneWithChecks rio checks step = withDbs rio $ \rdb db pdb h -> do
    (f0, f1) <- createForks db pdb h
    pruneAllChains logger rdb toyVersion checks
    assertHeaders db f0
    assertPrunedHeaders db f1
  where
    logger = genericLogger testLogLevel (step . T.unpack)

-- | Remove BlockPayload from the Payload.
--
failIntrinsicCheck :: IO RocksDb -> [PruningChecks] -> Natural -> (String -> IO ()) -> IO ()
failIntrinsicCheck rio checks n step = withDbs rio $ \rdb bdb pdb h -> do
    (f0, _) <- createForks bdb pdb h
    let b = f0 !! int n
    delHdr bdb b
    unsafeInsertBlockHeaderDb bdb $ b { _blockChainwebVersion = Development }
    try (pruneAllChains logger rdb toyVersion checks) >>= \case
        Left e
            | CheckFull `elem` checks
                && VersionMismatch `elem` _validationFailureFailures e
                && IncorrectHash `elem` _validationFailureFailures e
                && AdjacentChainMismatch `elem` _validationFailureFailures e -> return ()
            | CheckInductive `elem` checks
                && VersionMismatch `elem` _validationFailureFailures e -> return ()
            | CheckIntrinsic `elem` checks
                && IncorrectHash `elem` _validationFailureFailures e
                && AdjacentChainMismatch `elem` _validationFailureFailures e -> return ()
            | otherwise ->
                assertFailure $ "test failed with unexpected validation failure: " <> sshow e
        Right x -> assertFailure
            $ "missing expected ValidationFailure"
            <> ". Instead pruning succeeded and deleted "
            <> sshow x <> " headers"
    return ()
  where
    logger = genericLogger testLogLevel (step . T.unpack)

-- | Remove BlockPayload from the Payload.
--
-- CheckPayloadsExist and CheckPayload fail for this scenario
--
failPayloadCheck :: IO RocksDb -> [PruningChecks] -> Natural -> (String -> IO ()) -> IO ()
failPayloadCheck rio checks n step = withDbs rio $ \rdb bdb pdb h -> do
    (f0, _) <- createForks bdb pdb h
    let db = _transactionDbBlockPayloads $ _transactionDb pdb
    casDelete db (_blockPayloadHash $ f0 !! int n)
    try (pruneAllChains logger rdb toyVersion checks) >>= \case
        Left (MissingPayloadException{}) -> return ()
        Left e -> assertFailure
            $ "Expected MissingPayloadException but got: "
            <> sshow e
        Right x -> assertFailure
            $ "missing expected MissingPayloadException"
            <> ". Instead pruning succeeded and deleted "
            <> sshow x <> " headers"
    return ()
  where
    logger = genericLogger testLogLevel (step . T.unpack)

-- | Remove the Transactions from the Payload.
--
-- CheckPayloadsExist succeeds for this scenario. CheckPayload fails.
--
failPayloadCheck2 :: IO RocksDb -> [PruningChecks] -> Natural -> (String -> IO ()) -> IO ()
failPayloadCheck2 rio checks n step = withDbs rio $ \rdb bdb pdb h -> do
    (f0, _) <- createForks bdb pdb h
    let b = f0 !! int n
    payload <- casLookupM pdb (_blockPayloadHash b)
    casDelete (_transactionDbBlockTransactions $ _transactionDb pdb)
        $ _payloadWithOutputsTransactionsHash payload
    try (pruneAllChains logger rdb toyVersion checks) >>= \case
        Left (MissingPayloadException{}) -> return ()
        Left e -> assertFailure
            $ "Expected MissingPayloadException but got: "
            <> sshow e
        Right x -> assertFailure
            $ "missing expected MissingPayloadException"
            <> ". Instead pruning succeeded and deleted "
            <> sshow x <> " headers"
    return ()
  where
    logger = genericLogger testLogLevel (step . T.unpack)

