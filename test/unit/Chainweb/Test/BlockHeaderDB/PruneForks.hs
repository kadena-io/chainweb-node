{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

-- import Data.CAS
-- import Data.CAS.RocksDB
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Tree (Tree)
import Data.Tree qualified as Tree

import Numeric.Natural

import System.LogLevel
import System.Random

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash
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
import Chainweb.Test.CutDB (withTestCutDb)
import Chainweb.PayloadProvider (ConfiguredPayloadProvider(DisabledPayloadProvider))
import Chainweb.Test.Pact.Utils
import Chainweb.Storage.Table
import Chainweb.Cut (unsafeMkCut, genesisCut)
import Chainweb.Cut.CutHashes
import Chainweb.Core.Brief
import Chainweb.Test.TestVersions
import Chainweb.Graph
import Chainweb.WebBlockHeaderDB
import Control.Monad.State.Strict
import Chainweb.Parent
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Chainweb.Cut.Create
import Hedgehog
import Hedgehog.Gen (shuffle)
import qualified Data.HashSet as HS
import System.Random.Shuffle (shuffleM)
import Streaming.Prelude qualified as S
import Control.Monad.Except
import Streaming qualified as S
import PropertyMatchers qualified as P
import Chainweb.ChainValue
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Ranked
import Chainweb.Version.Utils (avgBlockHeightAtCutHeight, chainIdsAt)

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

selectHighCuts :: Casify RocksDbTable CutHashes -> Int -> Maybe (CasKeyType CutHashes) -> IO ([CutHashes], CasKeyType CutHashes)
selectHighCuts cutTable candidateCount maybeMaxCutHeight = do
    (highestCuts, nextCutHeight) <- withTableIterator cutTable $ \iter -> do
        case maybeMaxCutHeight of
            Nothing ->
                iterLast iter
            Just ch -> do
                iterSeek iter ch
                iterPrev iter

        cuts <- fmap catMaybes $ replicateM candidateCount $ do
            iterValue iter >>= \case
                Just r -> do
                    iterPrev iter
                    return (Just r)
                Nothing -> return Nothing
        nextCutHeight <- iterKey iter >>= \case
            Nothing -> iterNext iter >> fromJuste <$> iterKey iter
            Just ch -> return ch
        return (cuts, nextCutHeight)
    (,nextCutHeight) <$> shuffleM highestCuts

findM :: Monad m => [t] -> (t -> m (Maybe a)) -> m (Maybe a)
findM (x:xs) f = do
    y <- f x
    case y of
        Nothing -> findM xs f
        Just u -> return $ Just u
findM [] _ = return Nothing

selectNewBlockParents
    :: HasVersion
    => Casify RocksDbTable CutHashes
    -> WebBlockHeaderDb
    -> Maybe (CasKeyType CutHashes)
    -> Int
    -> IO CutExtension
selectNewBlockParents cutTable wbhdb maybeMaxCutHeight candCutCount = do
    (candCutHashesList, newMaxCutHeight) <- selectHighCuts cutTable candCutCount maybeMaxCutHeight
    shuffledChains <- shuffleM $ HS.toList $ chainIdsAt (round $ avgBlockHeightAtCutHeight $ maybe maxBound (view _1) maybeMaxCutHeight)
    r <- findM candCutHashesList $ \candCutHashes -> do
        candCutBlocks <- liftIO $
            iforM (candCutHashes ^. cutHashes) (lookupRankedWebBlockHeaderDb wbhdb)
        let candCut = unsafeMkCut candCutBlocks
        let exts = mapMaybe (getCutExtension candCut) shuffledChains
        case exts of
            ext:_ -> return $ Just ext
            [] -> return Nothing
    case r of
        Just r' -> return r'
        Nothing -> selectNewBlockParents cutTable wbhdb (Just newMaxCutHeight) candCutCount

progressArbitraryFork
    :: (HasVersion, MonadIO m, MonadState Nonce m)
    => Casify RocksDbTable CutHashes
    -> WebBlockHeaderDb
    -> Int
    -> m ()
progressArbitraryFork cutTable wbhdb candCutCount = do
    cutExt <- liftIO $ selectNewBlockParents cutTable wbhdb Nothing candCutCount
    let parent = _cutExtensionParent cutExt
    let bhdb = wbhdb ^?! webBlockHeaderDb . ix (parent ^. chainId)
    adjsForParent <- liftIO $ iforMOf (itraversed . _Parent)
        (_getBlockHashRecord $ _cutExtensionAdjacentHashes cutExt)
        (\c blk -> lookupRankedWebBlockHeaderDb wbhdb c
            (Ranked (view (_Parent . blockHeight) parent) blk))
    nextNonce <- get
    let newBlock = testBlockHeader adjsForParent nextNonce parent
    liftIO $ unsafeInsertBlockHeaderDb bhdb newBlock
    newCut <- liftIO $ tryMonotonicCutExtension (_cutExtensionCut cutExt) newBlock
    liftIO $ casInsert cutTable (cutToCutHashes Nothing $ fromJuste newCut)
    put (succ nextNonce)
    return ()

pruneTestRandom :: (HasVersion, _) => _
pruneTestRandom baseRdb f = runResourceT $ do
    rdb <- withTestRocksDb "" baseRdb
    liftIO $ do
        let t = cutHashesTable rdb
        wbhdb <- initWebBlockHeaderDb rdb
        let lookupChainBlk (ChainValue c bhsh) = do
                db <- getWebBlockHeaderDb wbhdb c
                TreeDB.lookup db bhsh
        let validateAllBlocks = do
                void $ webEntries wbhdb Nothing Nothing $ \blks -> do
                    blks
                        & S.hoist liftIO
                        & S.mapM_ (\blk ->
                            validateAllParentsExist lookupChainBlk blk
                                >>= \case
                                Right r -> P.succeed r
                                Left e -> P.fail "successful validation" (e, "failed on block: " <> sshow blk)
                            )
        casInsert t (cutToCutHashes Nothing genesisCut)
        lgr <- getTestLogger
        logFunctionText lgr Info "inserting blocks..."
        (_, finalNonce) <- flip runStateT (Nonce 0) $ f t wbhdb
        logFunctionText lgr Info $ "final nonce " <> sshow finalNonce
        logFunctionText lgr Info "validating initial blocks..."
        validateAllBlocks
        highestCut <- readHighestCutHeaders (logFunctionText lgr) wbhdb t
        numPruned <- pruneForks lgr (unsafeMkCut highestCut) wbhdb Prune 0
        logFunctionText lgr Info "validating pruned blocks..."
        validateAllBlocks
        return numPruned

pruneTestWithOnePivot :: _
pruneTestWithOnePivot rdb = withVersion (barebonesTestVersion pairChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 4000 $ do
            progressArbitraryFork t wbhdb 2
        replicateM_ 10 $ do
            progressArbitraryFork t wbhdb 1
    return ()

pruneTestWithLotsOfPivots :: _
pruneTestWithLotsOfPivots rdb = withVersion (barebonesTestVersion pairChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 4000 $ do
            progressArbitraryFork t wbhdb 16
    return ()

pruneTestWithManyForks :: _
pruneTestWithManyForks rdb = withVersion (barebonesTestVersion pairChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 4000 $ do
            progressArbitraryFork t wbhdb 16
    return ()

pruneTestWithManyChains :: _
pruneTestWithManyChains rdb = withVersion (barebonesTestVersion petersenChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 1000 $ do
            progressArbitraryFork t wbhdb 14
    return ()

pruneTestWithManyManyChains :: _
pruneTestWithManyManyChains rdb = withVersion (barebonesTestVersion d4k4ChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 200 $ do
            progressArbitraryFork t wbhdb 1
        replicateM_ 400 $ do
            progressArbitraryFork t wbhdb 140
        replicateM_ 400 $ do
            progressArbitraryFork t wbhdb 1
    return ()

pruneTestSmall :: _
pruneTestSmall rdb = withVersion (barebonesTestVersion petersenChainGraph) $ do
    _ <- pruneTestRandom rdb $ \t wbhdb -> do
        replicateM_ 20 $ do
            progressArbitraryFork t wbhdb 1
    return ()

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

        , testCase "small" $ pruneTestSmall rdb

        , testCase "one pivot" $ pruneTestWithOnePivot rdb

        , testCase "lots of pivots" $ pruneTestWithLotsOfPivots rdb

        , testCase "many forks" $ pruneTestWithManyForks rdb

        , testCase "many chains" $ pruneTestWithManyChains rdb
        , testCase "many many chains" $ pruneTestWithManyManyChains rdb

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
