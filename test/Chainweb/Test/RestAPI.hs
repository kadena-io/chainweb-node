{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module: Chainweb.Test.RestAPI
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.RestAPI
( tests
) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B8
import Data.Either
import Data.Foldable
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX

import Network.HTTP.Types.Status

import Servant.Client

import qualified Streaming.Prelude as SP

import Test.Tasty
import Test.Tasty.HUnit

import Text.Read (readEither)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MempoolBackend, MockTx)
import Chainweb.RestAPI
import Chainweb.Test.RestAPI.Client_
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.CAS.RocksDB

import Servant.Client_

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb queries

-- TODO remove these?

hashes :: MonadIO m => BlockHeaderDb -> m [DbKey BlockHeaderDb]
hashes db = liftIO $ SP.toList_ & keys db Nothing Nothing Nothing Nothing

headers :: MonadIO m => BlockHeaderDb -> m [DbEntry BlockHeaderDb]
headers db = liftIO $ SP.toList_ & entries db Nothing Nothing Nothing Nothing

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Utils

genesisBh :: MonadIO m => BlockHeaderDb -> m BlockHeader
genesisBh db = head <$> headers db

missingKey :: MonadIO m => BlockHeaderDb -> m (DbKey BlockHeaderDb)
missingKey db = key
    . head
    . testBlockHeadersWithNonce (Nonce 34523)
    . ParentHeader
    <$> genesisBh db

-- -------------------------------------------------------------------------- --
-- Response Predicates

isErrorCode :: Int -> Either ClientError a -> Bool
isErrorCode code (Left (FailureResponse _ Response { responseStatusCode = status}))
    | statusCode status == code = True
isErrorCode _ _ = False

-- -------------------------------------------------------------------------- --
-- Tests

tests :: RocksDb -> TestTree
tests rdb = testGroup "REST API tests"
    [ testGroup "Http" (tests_ rdb False)
    , testGroup "Https" (tests_ rdb True)
    ]

tests_ :: RocksDb -> Bool -> [TestTree]
tests_ rdb tls =
    [ simpleSessionTests rdb tls version
    , pagingTests rdb tls version
    ]
  where
    version = Test singletonChainGraph

-- -------------------------------------------------------------------------- --
-- Test all endpoints on each chain

-- | The type of 'TestClientEnv' that is used everywhere in this file
--
type TestClientEnv_ = TestClientEnv MockTx RocksDbCas

noMempool :: [(ChainId, MempoolBackend MockTx)]
noMempool = []

simpleSessionTests :: RocksDb -> Bool -> ChainwebVersion -> TestTree
simpleSessionTests rdb tls version =
    withBlockHeaderDbsResource rdb version $ \dbs ->
        withBlockHeaderDbsServer tls version dbs (return noMempool)
        $ \env -> testGroup "client session tests"
            $ httpHeaderTests env (head $ toList $ chainIds version)
            : (simpleClientSession env <$> toList (chainIds version))

httpHeaderTests :: IO TestClientEnv_ -> ChainId -> TestTree
httpHeaderTests envIO cid =
    testGroup ("http header tests for chain " <> sshow cid)
        [ go "headerClient" $ \v h -> headerClient' v cid (key h)
        , go "headersClient" $ \v _ -> headersClient' v cid Nothing Nothing Nothing Nothing
        , go "hashesClient" $ \v _ -> hashesClient' v cid Nothing Nothing Nothing Nothing
        , go "branchHashesClient" $ \v _ -> branchHashesClient' v cid Nothing Nothing Nothing
            Nothing (BranchBounds mempty mempty)
        , go "branchHeadersClient" $ \v _ -> branchHeadersClient' v cid Nothing Nothing Nothing
            Nothing (BranchBounds mempty mempty)
        ]
      where
        go name run = testCase name $ do
            BlockHeaderDbsTestClientEnv env _ version <- liftIO envIO
            res <- flip runClientM_ env $ modifyResponse checkHeader $
                run version (genesisBlockHeader version cid)
            assertBool ("test failed: " <> sshow res) (isRight res)
            return ()

        checkHeader res = do
            cur <- realToFrac <$> getPOSIXTime :: IO Double
            case Prelude.lookup "X-Server-Timestamp" (toList $ responseHeaders res) of
                Nothing -> assertFailure "X-Server-Timestamp header is missing from response"
                Just t -> case readEither (B8.unpack t) of
                    Left e -> assertFailure $ "failed to read value of X-Server-Timestamp header: " <> sshow e
                    Right x -> do
                        let d = cur - x
                        assertBool
                            ("test failed because X-Server-Time is of by " <> sshow d <> " seconds")
                            (d <= 2)
                        return res

simpleClientSession :: IO TestClientEnv_ -> ChainId -> TestTree
simpleClientSession envIO cid =
    testCaseSteps ("simple session for chain " <> sshow cid) $ \step -> do
        BlockHeaderDbsTestClientEnv env dbs version <- envIO
        res <- runClientM (session version dbs step) env
        assertBool ("test failed: " <> sshow res) (isRight res)
  where

    session version dbs step = do

        let gbh0 = genesisBlockHeader version cid

        db <- case Prelude.lookup cid dbs of
            Just x -> return x
            Nothing ->  error "Chainweb.Test.RestAPI.simpleClientSession: missing block header db in test"

        void $ liftIO $ step "headerClient: get genesis block header"
        gen0 <- headerClient version cid (key gbh0)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen0)

        void $ liftIO $ step "headerClient: get genesis block header pretty"
        gen01 <- headerClientJsonPretty version cid (key gbh0)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen01)

        void $ liftIO $ step "headerClient: get genesis block header binary"
        gen02 <- headerClientJsonBinary version cid (key gbh0)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen02)

        void $ liftIO $ step "headersClient: get genesis block header"
        bhs1 <- headersClient version cid Nothing Nothing Nothing Nothing
        gen1 <- case _pageItems bhs1 of
            [] -> liftIO $ assertFailure "headersClient did return empty result"
            (h:_) -> return h
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen1)

        void $ liftIO $ step "put 3 new blocks"
        let newHeaders = take 3 $ testBlockHeaders (ParentHeader gbh0)
        liftIO $ traverse_ (unsafeInsertBlockHeaderDb db) newHeaders

        void $ liftIO $ step "headersClient: get all 4 block headers"
        bhs2 <- headersClient version cid Nothing Nothing Nothing Nothing
        assertExpectation "headersClient returned wrong number of entries"
            (Expected 4)
            (Actual $ _pageLimit bhs2)

        void $ liftIO $ step "hashesClient: get all 4 block hashes"
        hs2 <- hashesClient version cid Nothing Nothing Nothing Nothing
        assertExpectation "hashesClient returned wrong number of entries"
            (Expected $ _pageLimit bhs2)
            (Actual $ _pageLimit hs2)
        assertExpectation "hashesClient returned wrong hashes"
            (Expected $ key <$> _pageItems bhs2)
            (Actual $ _pageItems hs2)

        forM_ newHeaders $ \h -> do
            void $ liftIO $ step $ "headerClient: " <> T.unpack (encodeToText (_blockHash h))
            r <- headerClient version cid (key h)
            assertExpectation "header client returned wrong entry"
                (Expected h)
                (Actual r)

        -- branchHeaders

        void $ liftIO $ step "branchHeadersClient: get no block headers"
        bhs3 <- branchHeadersClient version cid Nothing Nothing Nothing Nothing
            (BranchBounds mempty mempty)
        assertExpectation "branchHeadersClient returned wrong number of entries"
            (Expected 0)
            (Actual $ _pageLimit bhs3)

        forM_ ([2..] `zip` newHeaders) $ \(i, h) -> do
            void $ liftIO $ step $ "branchHeadersClient: get " <> sshow i <> " block headers with upper bound"
            bhs4 <- branchHeadersClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds mempty (HS.singleton (UpperBound $ key h)))
            assertExpectation "branchHeadersClient returned wrong number of entries"
                (Expected i)
                (Actual $ _pageLimit bhs4)

            void $ liftIO $ step "branchHeadersClient: get no block headers with lower and upper bound"
            bhs5 <- branchHeadersClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h)) (HS.singleton (UpperBound $ key h)))
            assertExpectation "branchHeadersClient returned wrong number of entries"
                (Expected 0)
                (Actual $ _pageLimit bhs5)

        forM_ (newHeaders `zip` drop 1 newHeaders) $ \(h0, h1) -> do
            void $ liftIO $ step "branchHeadersClient: get one block headers with lower and upper bound"
            bhs5 <- branchHeadersClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h0)) (HS.singleton (UpperBound $ key h1)))
            assertExpectation "branchHeadersClient returned wrong number of entries"
                (Expected 1)
                (Actual $ _pageLimit bhs5)

        forM_ (newHeaders `zip` drop 2 newHeaders) $ \(h0, h1) -> do
            void $ liftIO $ step "branchHeadersClient: get two block headers with lower and upper bound"
            bhs5 <- branchHeadersClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h0)) (HS.singleton (UpperBound $ key h1)))
            assertExpectation "branchHeadersClient returned wrong number of entries"
                (Expected 2)
                (Actual $ _pageLimit bhs5)

        -- branchHeaders

        void $ liftIO $ step "branchHashesClient: get no block headers"
        hs3 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
            (BranchBounds mempty mempty)
        assertExpectation "branchHashesClient returned wrong number of entries"
            (Expected 0)
            (Actual $ _pageLimit hs3)

        forM_ ([2..] `zip` newHeaders) $ \(i, h) -> do
            void $ liftIO $ step $ "branchHashesClient: get " <> sshow i <> " block headers with upper bound"
            hs4 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds mempty (HS.singleton (UpperBound $ key h)))
            assertExpectation "branchHashesClient returned wrong number of entries"
                (Expected i)
                (Actual $ _pageLimit hs4)

            void $ liftIO $ step "branchHashesClient: get no block headers with lower and upper bound"
            hs5 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h)) (HS.singleton (UpperBound $ key h)))
            assertExpectation "branchHashesClient returned wrong number of entries"
                (Expected 0)
                (Actual $ _pageLimit hs5)

        forM_ (newHeaders `zip` drop 1 newHeaders) $ \(h0, h1) -> do
            void $ liftIO $ step "branchHashesClient: get one block headers with lower and upper bound"
            hs5 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h0)) (HS.singleton (UpperBound $ key h1)))
            assertExpectation "branchHashesClient returned wrong number of entries"
                (Expected 1)
                (Actual $ _pageLimit hs5)

        forM_ (newHeaders `zip` drop 2 newHeaders) $ \(h0, h1) -> do
            void $ liftIO $ step "branchHashesClient: get two block headers with lower and upper bound"
            hs5 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key h0)) (HS.singleton (UpperBound $ key h1)))
            assertExpectation "branchHashesClient returned wrong number of entries"
                (Expected 2)
                (Actual $ _pageLimit hs5)

        -- branch hashes with fork

        void $ liftIO $ step "headerPutClient: put 3 new blocks on a new fork"
        let newHeaders2 = take 3 $ testBlockHeadersWithNonce (Nonce 17) (ParentHeader gbh0)
        liftIO $ traverse_ (unsafeInsertBlockHeaderDb db) newHeaders2

        let lower = last newHeaders
        forM_ ([1..] `zip` newHeaders2) $ \(i, h) -> do
            void $ liftIO $ step "branchHashesClient: get one block headers with lower and upper bound"
            hs5 <- branchHashesClient version cid Nothing Nothing Nothing Nothing
                (BranchBounds (HS.singleton (LowerBound $ key lower)) (HS.singleton (UpperBound $ key h)))
            assertExpectation "branchHashesClient returned wrong number of entries"
                (Expected i)
                (Actual $ _pageLimit hs5)

-- -------------------------------------------------------------------------- --
-- Paging Tests

pagingTests :: RocksDb -> Bool -> ChainwebVersion -> TestTree
pagingTests rdb tls version =
    withBlockHeaderDbsServer tls version
            (starBlockHeaderDbs 6 $ testBlockHeaderDbs rdb version)
            (return noMempool)
    $ \env -> testGroup "paging tests"
        [ testPageLimitHeadersClient version env
        , testPageLimitHashesClient version env
        ]

pagingTest
    :: Eq a
    => Show a
    => String
        -- ^ Test name
    -> (BlockHeaderDb -> IO [a])
        -- ^ Get test items from database
    -> (a -> DbKey BlockHeaderDb)
        -- ^ Compute paging key from item
    -> Bool
        -- ^ whether the result represents an finite (True) or infinite (False)
        -- set
    -> (ChainId -> Maybe Limit -> Maybe (NextItem (DbKey BlockHeaderDb)) -> ClientM (Page (NextItem (DbKey BlockHeaderDb)) a))
        -- ^ Request with paging parameters
    -> IO TestClientEnv_
        -- ^ Test environment
    -> TestTree
pagingTest name getDbItems getKey fin request envIO = testGroup name
    [ testCaseSteps "test limit parameter" $ \step -> do
        BlockHeaderDbsTestClientEnv env [(cid, db)] _ <- envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env $ forM_ [0 .. (l+2)] $ \i ->
            session step ents cid (Just i) Nothing
        assertBool ("test of limit failed: " <> sshow res) (isRight res)

    -- TODO Did a limit value of 0 mean something else previously?
    -- The two last tests that are failing now are failing when
    -- hitting `Limit 0`.

    , testCaseSteps "test next parameter" $ \step -> do
        BlockHeaderDbsTestClientEnv env [(cid, db)] _ <- envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env $ forM_ [0 .. (l-1)] $ \i -> do
            let es = drop i ents
            session step es cid Nothing (Just . Inclusive . getKey . head $ es)
        assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCaseSteps "test limit and next paramter" $ \step -> do
        BlockHeaderDbsTestClientEnv env [(cid, db)] _ <- envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env
            $ forM_ [0 .. (l-1)] $ \i -> forM_ [0 .. (l+2-i)] $ \j -> do
                let es = drop (int i) ents
                session step es cid (Just j) (Just . Inclusive . getKey . head $ es)
        assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCase "non existing next parameter" $ do
        BlockHeaderDbsTestClientEnv env [(cid, db)] _ <- envIO
        missing <- missingKey db
        res <- flip runClientM env $ request cid Nothing (Just $ Exclusive missing)
        assertBool ("test failed with unexpected result: " <> sshow res) (isErrorCode 404 res)
    ]
  where
    session step ents cid n next = do
        void $ liftIO $ step $ "limit " <> sshow n <> ", next " <> sshow next
        r <- request cid n next
        assertExpectation "result has wrong page 'limit' value"
            (Expected . maybe id min n . int $ length ents)
            (Actual $ _pageLimit r)
        assertExpectation "result contains wrong page 'items'"
            (Expected . maybe id (take . int) n $ ents)
            (Actual $ _pageItems r)
        assertExpectation "result contains wrong page 'next' value"
            (Expected $ expectedNext ents n)
            (Actual $ _pageNext r)

    expectedNext = if fin then expectedNextFin else expectedNextInf

    -- Finite case
    expectedNextFin _ Nothing = Nothing
    expectedNextFin ents (Just n) = Inclusive . getKey <$> listToMaybe (drop (int n) ents)

    -- Infinite case
    expectedNextInf ents Nothing = Exclusive . getKey <$> (Just $ last ents)
    expectedNextInf ents (Just n)
        | n >= len ents = Exclusive . getKey <$> (Just $ last ents)
        | otherwise = Inclusive . getKey <$> listToMaybe (drop (int n) ents)

testPageLimitHeadersClient :: ChainwebVersion -> IO TestClientEnv_ -> TestTree
testPageLimitHeadersClient version = pagingTest "headersClient" headers key False request
  where
    request cid l n = headersClient version cid l n Nothing Nothing

testPageLimitHashesClient :: ChainwebVersion -> IO TestClientEnv_ -> TestTree
testPageLimitHashesClient version = pagingTest "hashesClient" hashes id False request
  where
    request cid l n = hashesClient version cid l n Nothing Nothing
