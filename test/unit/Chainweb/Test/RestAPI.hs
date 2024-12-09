{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# options_ghc -fno-warn-unused-local-binds -fno-warn-unused-imports #-}

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

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Bifunctor (first)
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

import Chainweb.Block
import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal (unsafeInsertBlockHeaderDb)
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MempoolBackend, MockTx)
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.RestAPI
import Chainweb.Test.RestAPI.Client_
import Chainweb.Test.RestAPI.Utils (isFailureResponse, clientErrorStatusCode)
import Chainweb.Test.Utils
import Chainweb.Test.Utils.BlockHeader
import Chainweb.Test.TestVersions (barebonesTestVersion)
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Chainweb.Storage.Table.RocksDB

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
    [ simpleSessionTests rdb tls
    , pagingTests rdb tls
    ]

version :: ChainwebVersion
version = barebonesTestVersion singletonChainGraph

-- -------------------------------------------------------------------------- --
-- Test all endpoints on each chain

-- | The type of 'TestClientEnv' that is used everywhere in this file
--
type TestClientEnv_ = TestClientEnv MockTx RocksDbTable

mkEnv :: RocksDb -> Bool -> [(ChainId, BlockHeaderDb)] -> ResourceT IO TestClientEnv_
mkEnv rdb tls dbs = do
    let pdb = newPayloadDb rdb
    liftIO $ initializePayloadDb version pdb
    clientEnvWithChainwebTestServer ValidateSpec tls version emptyChainwebServerDbs
        { _chainwebServerBlockHeaderDbs = dbs
        , _chainwebServerPayloadDbs = [ (cid, pdb) | (cid, _) <- dbs ]
        }

simpleSessionTests :: RocksDb -> Bool -> TestTree
simpleSessionTests rdb tls =
    withResource' (testBlockHeaderDbs rdb version) $ \dbsIO ->
        withResourceT (mkEnv rdb tls =<< liftIO dbsIO)
        $ \env -> testGroup "client session tests"
            $ httpHeaderTests env (head $ toList $ chainIds version)
            : (simpleClientSession env <$> toList (chainIds version))

httpHeaderTests :: IO TestClientEnv_ -> ChainId -> TestTree
httpHeaderTests envIO cid =
    testGroup ("http header tests for chain " <> sshow cid)
        [ testCase "headerClient" $ go $ \v h -> headerClient' v cid (key h)
        , testCase "headersClient" $ go $ \v _ -> headersClient' v cid Nothing Nothing Nothing Nothing
        , testCase "blocksClient" $ go $ \v _ -> blocksClient' v cid Nothing Nothing Nothing Nothing
        , testCase "hashesClient" $ go $ \v _ -> hashesClient' v cid Nothing Nothing Nothing Nothing
        , testCase "branchHashesClient" $ go $ \v _ -> branchHashesClient' v cid Nothing Nothing Nothing
            Nothing (BranchBounds mempty mempty)
        , testCase "branchHeadersClient" $ go $ \v _ -> branchHeadersClient' v cid Nothing Nothing Nothing
            Nothing (BranchBounds mempty mempty)
        , testCase "branchBlocksClient" $ go $ \v _ -> branchBlocksClient' v cid Nothing Nothing Nothing
            Nothing (BranchBounds mempty mempty)
        ]
      where
        go run = do
            env <- _envClientEnv <$> envIO
            res <- flip runClientM_ env $ modifyResponse checkHeader $
                run version (genesisBlockHeader version cid)
            assertBool ("test failed: " <> sshow res) (isRight res)

        checkHeader res = do
            cur <- realToFrac <$> getPOSIXTime :: IO Double
            case Prelude.lookup "X-Server-Timestamp" (toList $ responseHeaders res) of
                Nothing -> assertFailure "X-Server-Timestamp header is missing from response"
                Just t -> case readEither (B8.unpack t) of
                    Left e -> assertFailure $ "failed to read value of X-Server-Timestamp header: " <> sshow e
                    Right x -> do
                        let d = cur - x
                        assertBool
                            ("test failed because X-Server-Time is off by " <> sshow d <> " seconds")
                            (d <= 2)
                        return res

simpleClientSession :: IO TestClientEnv_ -> ChainId -> TestTree
simpleClientSession envIO cid =
    testCaseSteps ("simple session for chain " <> sshow cid) $ \step -> do
        env <- _envClientEnv <$> envIO
        bhdbs <- _envBlockHeaderDbs <$> envIO
        pdbs <- _envPayloadDbs <$> envIO
        res <- runClientM (session bhdbs pdbs step) env
        assertBool ("test failed: " <> sshow res) (isRight res)
  where

    session :: [(ChainId, BlockHeaderDb)] -> [(ChainId, PayloadDb RocksDbTable)] -> (String -> IO a) -> ClientM ()
    session bhdbs pdbs step = do

        let gbh0 = genesisBlockHeader version cid

        bhdb <- case Prelude.lookup cid bhdbs of
            Just x -> return x
            Nothing -> error "Chainweb.Test.RestAPI.simpleClientSession: missing block header db in test"

        pdb <- case Prelude.lookup cid pdbs of
            Just x -> return x
            Nothing -> error "Chainweb.Test.RestAPI.simpleClientSession: missing payload db in test"

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

        void $ liftIO $ step "blocksClient: get genesis block"
        block1 <- blocksClient version cid Nothing Nothing Nothing Nothing
        gen1Block <- case _pageItems block1 of
            [] -> liftIO $ assertFailure "blocksClient did return empty result"
            (h:_) -> return h
        assertExpectation "block client returned wrong entry"
            (Expected (Block gbh0 (version ^?! versionGenesis . genesisBlockPayload . atChain cid)))
            (Actual gen1Block)

        void $ liftIO $ step "put 3 new blocks"
        let newHeaders = take 3 $ testBlockHeaders (ParentHeader gbh0)
        liftIO $ traverse_ (unsafeInsertBlockHeaderDb bhdb) newHeaders
        liftIO $ traverse_ (\x -> addNewPayload pdb (view blockHeight x) (testBlockPayload_ x)) newHeaders

        void $ liftIO $ step "headersClient: get all 4 block headers"
        bhs2 <- headersClient version cid Nothing Nothing Nothing Nothing
        assertExpectation "headersClient returned wrong number of entries"
            (Expected 4)
            (Actual $ _pageLimit bhs2)

        void $ liftIO $ step "blocksClient: get all 4 blocks"
        blocks2 <- blocksClient version cid Nothing Nothing Nothing Nothing
        assertExpectation "blocksClient returned wrong number of entries"
            (Expected 4)
            (Actual $ _pageLimit blocks2)

        void $ liftIO $ step "hashesClient: get all 4 block hashes"
        hs2 <- hashesClient version cid Nothing Nothing Nothing Nothing
        assertExpectation "hashesClient returned wrong number of entries"
            (Expected $ _pageLimit bhs2)
            (Actual $ _pageLimit hs2)
        assertExpectation "hashesClient returned wrong hashes"
            (Expected $ key <$> _pageItems bhs2)
            (Actual $ _pageItems hs2)

        forM_ newHeaders $ \h -> do
            void $ liftIO $ step $ "headerClient: " <> T.unpack (encodeToText (view blockHash h))
            r <- headerClient version cid (key h)
            assertExpectation "header client returned wrong entry"
                (Expected h)
                (Actual r)

        -- branchHeaders

        do
          void $ liftIO $ step "branchHeadersClient: BranchBounds limits exceeded"
          clientEnv <- liftIO $ _envClientEnv <$> envIO
          let query bounds = liftIO
                $ flip runClientM clientEnv
                $ branchHeadersClient
                    version cid Nothing Nothing Nothing Nothing bounds
          let limit = 32
          let blockHeaders = testBlockHeaders (ParentHeader gbh0)
          let maxBlockHeaders = take limit blockHeaders
          let excessBlockHeaders = take (limit + 1) blockHeaders

          let mkLower :: [BlockHeader] -> HS.HashSet (LowerBound BlockHash)
              mkLower hs = HS.fromList $ map (LowerBound . key) hs
          let mkUpper :: [BlockHeader] -> HS.HashSet (UpperBound BlockHash)
              mkUpper hs = HS.fromList $ map (UpperBound . key) hs

          let emptyLower = mkLower []
          let badLower = mkLower excessBlockHeaders
          let goodLower = mkLower maxBlockHeaders

          let emptyUpper = mkUpper []
          let badUpper = mkUpper excessBlockHeaders
          let goodUpper = mkUpper maxBlockHeaders

          let badRespCheck :: Int -> ClientError -> Bool
              badRespCheck s e = isFailureResponse e && clientErrorStatusCode e == Just s

          badLowerResponse <- query (BranchBounds badLower emptyUpper)
          assertExpectation "branchHeadersClient returned a 400 error code on excess lower"
            (Expected (Left True))
            (Actual (first (badRespCheck 400) badLowerResponse))

          badUpperResponse <- query (BranchBounds emptyLower badUpper)
          assertExpectation "branchHeadersClient returned a 400 error code on excess upper"
            (Expected (Left True))
            (Actual (first (badRespCheck 400) badUpperResponse))

          -- This will still fail because a bunch of these keys won't be found,
          -- but it won't fail the bounds check, which happens first
          doesntFailBoundsCheck <- query (BranchBounds goodLower goodUpper)
          assertExpectation "branchHeadersClient returned a 404; bounds were within the limits, still fails key exists check"
            (Expected (Left True))
            (Actual (first (badRespCheck 404) doesntFailBoundsCheck))

          doesntFailAtAll <- query (BranchBounds emptyLower emptyUpper)
          assertExpectation "branchHeadersClient returned a 200; bounds were within the limits, and no keys to check at all"
            (Expected (Right ()))
            (Actual (() <$ doesntFailAtAll))

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
        liftIO $ traverse_ (unsafeInsertBlockHeaderDb bhdb) newHeaders2
        liftIO $ traverse_ (\x -> addNewPayload pdb (view blockHeight x) (testBlockPayload_ x)) newHeaders2

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

pagingTests :: RocksDb -> Bool -> TestTree
pagingTests rdb tls =
    withResourceT
        (mkEnv rdb tls =<<
            liftIO (starBlockHeaderDbs 6 =<< testBlockHeaderDbs rdb version))
    $ \env -> testGroup "paging tests"
        [ testPageLimitHeadersClient env
        , testPageLimitHashesClient env
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
        env <- _envClientEnv <$> envIO
        [(cid, db)] <- _envBlockHeaderDbs <$> envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env $ forM_ [0 .. (l+2)] $ \i ->
            session step ents cid (Just i) Nothing
        assertBool ("test of limit failed: " <> sshow res) (isRight res)

    -- TODO Did a limit value of 0 mean something else previously?
    -- The two last tests that are failing now are failing when
    -- hitting `Limit 0`.

    , testCaseSteps "test next parameter" $ \step -> do
        env <- _envClientEnv <$> envIO
        [(cid, db)] <- _envBlockHeaderDbs <$> envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env $ forM_ [0 .. (l-1)] $ \i -> do
            let es = drop i ents
            session step es cid Nothing (Just . Inclusive . getKey . head $ es)
        assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCaseSteps "test limit and next parameter" $ \step -> do
        env <- _envClientEnv <$> envIO
        [(cid, db)] <- _envBlockHeaderDbs <$> envIO
        ents <- getDbItems db
        let l = len ents
        res <- flip runClientM env
            $ forM_ [0 .. (l-1)] $ \i -> forM_ [0 .. (l+2-i)] $ \j -> do
                let es = drop (int i) ents
                session step es cid (Just j) (Just . Inclusive . getKey . head $ es)
        assertBool ("test limit and next failed: " <> sshow res) (isRight res)

    , testCase "non existing next parameter" $ do
        env <- _envClientEnv <$> envIO
        [(cid, db)] <- _envBlockHeaderDbs <$> envIO
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

testPageLimitHeadersClient :: IO TestClientEnv_ -> TestTree
testPageLimitHeadersClient = pagingTest "headersClient" headers key False request
  where
    request cid l n = headersClient version cid l n Nothing Nothing

testPageLimitHashesClient :: IO TestClientEnv_ -> TestTree
testPageLimitHashesClient = pagingTest "hashesClient" hashes id False request
  where
    request cid l n = hashesClient version cid l n Nothing Nothing
