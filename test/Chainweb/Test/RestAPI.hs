{-# LANGUAGE DataKinds #-}
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

import Data.Either
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T

import Network.HTTP.Types.Status

import Servant.Client

import qualified Streaming.Prelude as SP

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MempoolBackend, MockTx)
import Chainweb.RestAPI
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.CAS.RocksDB

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
missingKey db = key . head . testBlockHeadersWithNonce (Nonce 34523) <$> genesisBh db

-- -------------------------------------------------------------------------- --
-- Response Predicates

isErrorCode :: Int -> Either ServantError a -> Bool
isErrorCode code (Left (FailureResponse Response { responseStatusCode = status}))
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
    , putTests rdb tls version
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
    withBlockHeaderDbsServer tls version (testBlockHeaderDbs rdb version) (return noMempool)
    $ \env -> testGroup "client session tests"
        $ simpleClientSession env <$> toList (chainIds version)

simpleClientSession :: IO TestClientEnv_ -> ChainId -> TestTree
simpleClientSession envIO cid =
    testCaseSteps ("simple session for chain " <> sshow cid) $ \step -> do
        BlockHeaderDbsTestClientEnv env _ version <- envIO
        res <- runClientM (session version step) env
        assertBool ("test failed: " <> sshow res) (isRight res)
  where

    session version step = do

        let gbh0 = genesisBlockHeader version cid

        void $ liftIO $ step "headerClient: get genesis block header"
        gen0 <- headerClient version cid (key gbh0)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen0)

        void $ liftIO $ step "headersClient: get genesis block header"
        bhs1 <- headersClient version cid Nothing Nothing Nothing Nothing
        gen1 <- case _pageItems bhs1 of
            [] -> liftIO $ assertFailure "headersClient did return empty result"
            (h:_) -> return h
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen1)

        void $ liftIO $ step "headerPutClient: put 3 new blocks"
        let newHeaders = take 3 $ testBlockHeaders gbh0
        forM_ newHeaders $ \h -> do
            void $ liftIO $ step $ "headerPutClient: " <> T.unpack (encodeToText (_blockHash h))
            void $ headerPutClient version cid h

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

-- -------------------------------------------------------------------------- --
-- Test Client Session

simpleTest
    :: Show a
    => String
        -- ^ Test description
    -> (Either ServantError a -> Bool)
        -- ^ Success predicate
    -> (BlockHeader -> ClientM a)
        -- ^ Test HTTP client session
    -> IO TestClientEnv_
        -- ^ Test environment
    -> TestTree
simpleTest msg p session envIO = testCase msg $ do
    BlockHeaderDbsTestClientEnv env [(_, db)] _ <- envIO
    gbh <- head <$> headers db
    res <- runClientM (session gbh) env
    assertBool ("test failed with unexpected result: " <> sshow res) (p res)

-- -------------------------------------------------------------------------- --
-- Put Tests

putNewBlockHeader :: IO TestClientEnv_ -> TestTree
putNewBlockHeader = simpleTest "put new block header" isRight $ \h0 ->
    headerPutClient (_chainwebVersion h0) (_chainId h0)
        . head
        $ testBlockHeadersWithNonce (Nonce 1) h0

putExisting :: IO TestClientEnv_ -> TestTree
putExisting = simpleTest "put existing block header" isRight $ \h0 ->
    headerPutClient (_chainwebVersion h0) (_chainId h0) h0

putOnWrongChain :: IO TestClientEnv_ -> TestTree
putOnWrongChain = simpleTest "put on wrong chain fails" (isErrorCode 400)
    $ \h0 -> do
        cid <- mkChainId v (1 :: Int)
        headerPutClient (_chainwebVersion h0) (_chainId h0)
            . head
            . testBlockHeadersWithNonce (Nonce 2)
            $ genesisBlockHeader v cid
  where
    v = Test petersonChainGraph

putMissingParent :: IO TestClientEnv_ -> TestTree
putMissingParent = simpleTest "put missing parent" (isErrorCode 400) $ \h0 ->
    headerPutClient (_chainwebVersion h0) (_chainId h0)
        . (!! 2)
        $ testBlockHeadersWithNonce (Nonce 3) h0

put5NewBlockHeaders :: IO TestClientEnv_ -> TestTree
put5NewBlockHeaders = simpleTest "put 5 new block header" isRight $ \h0 ->
    mapM_ (headerPutClient (_chainwebVersion h0) (_chainId h0))
        . take 5
        $ testBlockHeadersWithNonce (Nonce 4) h0

putTests :: RocksDb -> Bool -> ChainwebVersion -> TestTree
putTests rdb tls version =
    withBlockHeaderDbsServer tls version (testBlockHeaderDbs rdb version) (return noMempool)
        $ \env -> testGroup "put tests"
            [ putNewBlockHeader env
            , putExisting env
            , putOnWrongChain env
            , putMissingParent env
            , put5NewBlockHeaders env
            ]

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

