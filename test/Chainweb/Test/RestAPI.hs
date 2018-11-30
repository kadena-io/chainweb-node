{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Reflection (give)
import qualified Data.Text as T

import Network.HTTP.Types.Status

import Numeric.Natural

import Servant.Client

import qualified Streaming.Prelude as SP

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainDB
import Chainweb.ChainDB.Queries
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.RestAPI
import Chainweb.RestAPI.Utils
import Chainweb.Test.Utils
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- ChainDb queries from Chainweb.ChainDB.Queries

hashes :: MonadIO m => ChainDb -> m [Key 'Checked]
hashes db = SP.toList_ $ chainDbHashes db Nothing Nothing Nothing

headers :: MonadIO m => ChainDb -> m [Entry 'Checked]
headers db = SP.toList_ $ chainDbHeaders db Nothing Nothing Nothing

dbBranches :: MonadIO m => ChainDb -> m [Key 'Checked]
dbBranches db = do
    sn <- liftIO $ snapshot db
    SP.toList_ $ chainDbBranches sn Nothing Nothing

-- -------------------------------------------------------------------------- --
-- ChainDB Utils

genesisBh :: MonadIO m => ChainDb -> m BlockHeader
genesisBh db = dbEntry . head <$> headers db

missingKey :: MonadIO m => ChainDb -> m (Key 'Unchecked)
missingKey db = uncheckedKey
    . key
    . entry
    . head
    . testBlockHeadersWithNonce (Nonce 34523)
    <$> genesisBh db

-- -------------------------------------------------------------------------- --
-- Response Predicates

isErrorCode :: Int -> Either ServantError a -> Bool
isErrorCode code (Left (FailureResponse Response { responseStatusCode = status}))
    | statusCode status == code = True
isErrorCode _ _ = False

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "REST API tests"
    [ simpleSessionTests
    , putTests
    , pagingTests
    ]

-- -------------------------------------------------------------------------- --
-- Test all endpoints on each chain

simpleSessionTests :: TestTree
simpleSessionTests = testGroup "" [] -- TODO restore
-- simpleSessionTests = withChainDbsServer petersonGenesisChainDbs
--     $ \env -> testGroup "client session tests"
--         $ simpleClientSession env <$> toList (give peterson chainIds)

simpleClientSession :: IO TestClientEnv -> ChainId -> TestTree
simpleClientSession envIO cid =
    testCaseSteps ("simple session for chain " <> sshow cid) $ \step -> do
        -- TODO RESTORE
        -- ChainDbsTestClientEnv env _ <- envIO
        -- res <- runClientM (session step) env
        -- assertBool ("test failed: " <> sshow res) (isRight res)
        return ()
  where
    gbh0 = genesisBlockHeader Test peterson cid

    session step = do
        void $ liftIO $ step "headerClient: get genesis block header"
        gen0 <- headerClient Test cid (key $ entry gbh0)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual $ dbEntry gen0)

        void $ liftIO $ step "headersClient: get genesis block header"
        bhs1 <- headersClient Test cid Nothing Nothing Nothing Nothing Nothing
        gen1 <- case _pageItems bhs1 of
            [] -> liftIO $ assertFailure "headersClient did return empty result"
            (h:_) -> return (dbEntry h)
        assertExpectation "header client returned wrong entry"
            (Expected gbh0)
            (Actual gen1)

        void $ liftIO $ step "branchesClient: get gensis block header"
        brs <- branchesClient Test cid Nothing Nothing Nothing Nothing
        assertExpectation "branchesClient returned wrong number of entries"
            (Expected 1)
            (Actual $ _pageLimit brs)
        assertExpectation "branchesClient returned wrong entry"
            (Expected $ _blockHash gbh0)
            (Actual . dbKey . head . _pageItems $ brs)

        void $ liftIO $ step "headerPutClient: put 3 new blocks"
        let newHeaders = take 3 $ testBlockHeaders gbh0
        forM_ newHeaders $ \h -> do
            void $ liftIO $ step $ "headerPutClient: " <> T.unpack (encodeToText (_blockHash h))
            void $ headerPutClient Test cid (entry h)

        void $ liftIO $ step "headersClient: get all 4 block headers"
        bhs2 <- headersClient Test cid Nothing Nothing Nothing Nothing Nothing
        assertExpectation "headersClient returned wrong number of entries"
            (Expected 4)
            (Actual $ _pageLimit bhs2)

        void $ liftIO $ step "hashesClient: get all 4 block hashes"
        hs2 <- hashesClient Test cid Nothing Nothing Nothing Nothing Nothing
        assertExpectation "hashesClient returned wrong number of entries"
            (Expected $ _pageLimit bhs2)
            (Actual $ _pageLimit hs2)
        assertExpectation "hashesClient returned wrong hashes"
            (Expected $ key <$> _pageItems bhs2)
            (Actual $ _pageItems hs2)

        void $ liftIO $ step "branchesClient: get latest branch"
        brs2 <- branchesClient Test cid Nothing Nothing Nothing Nothing
        assertExpectation "branchesClient returned wrong number of entries"
            (Expected 1)
            (Actual $ _pageLimit brs2)
        assertExpectation "branchesClient returned wrong entry"
            (Expected . _blockHash $ last newHeaders)
            (Actual . dbKey . head $ _pageItems brs2)

        forM_ newHeaders $ \h -> do
            void $ liftIO $ step $ "headerClient: " <> T.unpack (encodeToText (_blockHash h))
            r <- headerClient Test cid (key $ entry h)
            assertExpectation "header client returned wrong entry"
                (Expected h)
                (Actual $ dbEntry r)

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
    -> IO TestClientEnv
        -- ^ Test environment
    -> TestTree
simpleTest msg p session envIO = testCase msg $ do
    pure ()
    -- TODO restore
    -- ChainDbsTestClientEnv env [(_, db)] <- envIO
    -- gbh <- dbEntry . head <$> headers db
    -- res <- runClientM (session gbh) env
    -- assertBool ("test failed with unexpected result: " <> sshow res) (p res)

-- -------------------------------------------------------------------------- --
-- Put Tests

putNewBlockHeader :: IO TestClientEnv -> TestTree
putNewBlockHeader = simpleTest "put new block header" isRight $ \h0 ->
    headerPutClient Test (_chainId h0)
        . entry
        . head
        $ testBlockHeadersWithNonce (Nonce 1) h0

putExisting :: IO TestClientEnv -> TestTree
putExisting = simpleTest "put existing block header" isRight $ \h0 ->
    headerPutClient Test (_chainId h0) (entry h0)

putOnWrongChain :: IO TestClientEnv -> TestTree
putOnWrongChain = simpleTest "put on wrong chain fails" (isErrorCode 400)
    $ \h0 -> headerPutClient Test (_chainId h0)
        . entry
        . head
        . testBlockHeadersWithNonce (Nonce 2)
        $ genesisBlockHeader Test peterson (testChainId 1)

putMissingParent :: IO TestClientEnv -> TestTree
putMissingParent = simpleTest "put missing parent" (isErrorCode 400) $ \h0 ->
    headerPutClient Test (_chainId h0)
        . entry
        . (!! 2)
        $ testBlockHeadersWithNonce (Nonce 3) h0

put5NewBlockHeaders :: IO TestClientEnv -> TestTree
put5NewBlockHeaders = simpleTest "put 5 new block header" isRight $ \h0 ->
    mapM_ (headerPutClient Test (_chainId h0))
        . fmap entry
        . take 5
        $ testBlockHeadersWithNonce (Nonce 4) h0

putTests :: TestTree
putTests = undefined -- TODO restore
-- putTests = withChainDbsServer singletonGenesisChainDbs
--     $ \env -> testGroup "put tests"
--         [ putNewBlockHeader env
--         , putExisting env
--         , putOnWrongChain env
--         , putMissingParent env
--         , put5NewBlockHeaders env
--         ]

-- -------------------------------------------------------------------------- --
-- Paging Tests

pagingTests :: TestTree
pagingTests = undefined -- TODO restore
-- pagingTests = withChainDbsServer (starChainDbs 6 singletonGenesisChainDbs)
--     $ \env -> testGroup "paging tests"
--         [ testPageLimitHeadersClient env
--         , testPageLimitHashesClient env
--         , testPageLimitBranchesClient env
--         ]

pagingTest
    :: Eq a
    => Show a
    => String
        -- ^ Test name
    -> (ChainDb -> IO [a])
        -- ^ Get test items from database
    -> (a -> Key 'Unchecked)
        -- ^ Compute paging key from item
    -> (ChainId -> Maybe Natural -> Maybe (Key 'Unchecked) -> ClientM (Page (Key 'Unchecked) a))
        -- ^ Request with paging parameters
    -> IO TestClientEnv
        -- ^ Test environment
    -> TestTree
pagingTest name getDbItems getKey request envIO = testGroup name
    [ testCaseSteps "test limit parameter" $ \step -> do
        pure () -- TODO restore
        -- ChainDbsTestClientEnv env [(cid, db)] <- envIO
        -- entries <- getDbItems db
        -- let l = len entries
        -- res <- flip runClientM env $ forM_ [0 .. (l+2)] $ \i ->
        --     session step entries cid (Just i) Nothing
        -- assertBool ("test of limit failed: " <> sshow res) (isRight res)

    , testCaseSteps "test next parameter" $ \step -> do
        pure () -- TODO restore
        -- ChainDbsTestClientEnv env [(cid, db)] <- envIO
        -- entries <- getDbItems db
        -- let l = len entries
        -- res <- flip runClientM env $ forM_ [0 .. (l-1)] $ \i -> do
        --     let es = drop i entries
        --     session step es cid Nothing (Just . getKey . head $ es)
        -- assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCaseSteps "test limit and next paramter" $ \step -> do
        pure () -- TODO restore
        -- ChainDbsTestClientEnv env [(cid, db)] <- envIO
        -- entries <- getDbItems db
        -- let l = len entries
        -- res <- flip runClientM env
        --     $ forM_ [0 .. (l-1)] $ \i -> forM_ [0 .. (l+2-i)] $ \j -> do
        --         let es = drop (int i) entries
        --         session step es cid (Just j) (Just . getKey . head $ es)
        -- assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCase "non existing next parameter" $ do
        pure () -- restore
        -- ChainDbsTestClientEnv env [(cid, db)] <- envIO
        -- missing <- missingKey db
        -- res <- flip runClientM env $ request cid Nothing (Just missing)
        -- assertBool ("test failed with unexpected result: " <> sshow res) (isErrorCode 404 res)
    ]
  where
    session step entries cid n next = do
        void $ liftIO $ step $ "limit " <> sshow n <> ", next " <> sshow next
        r <- request cid n next
        assertExpectation "result has wrong page 'limit' value"
            (Expected . maybe id min n . int $ length entries)
            (Actual $ _pageLimit r)
        assertExpectation "result contains wrong page 'items'"
            (Expected . maybe id (take . int) n $ entries)
            (Actual $ _pageItems r)
        assertExpectation "result contains wrong page 'next' value"
            (Expected . fmap getKey . listToMaybe . maybe (const []) (drop . int) n $ entries)
            (Actual $ _pageNext r)

testPageLimitHeadersClient :: IO TestClientEnv -> TestTree
testPageLimitHeadersClient = pagingTest "headersClient" entries key request
  where
    entries = fmap (fmap uncheckedEntry) . headers
    request cid l n = headersClient Test cid l n Nothing Nothing Nothing

testPageLimitHashesClient :: IO TestClientEnv -> TestTree
testPageLimitHashesClient = pagingTest "hashesClient" entries id request
  where
    entries db = fmap uncheckedKey <$> hashes db
    request cid l n = hashesClient Test cid l n Nothing Nothing Nothing

testPageLimitBranchesClient :: IO TestClientEnv -> TestTree
testPageLimitBranchesClient = pagingTest "branchesClient" entries id request
  where
    entries db = fmap uncheckedKey <$> dbBranches db
    request cid l n = branchesClient Test cid l n Nothing Nothing
