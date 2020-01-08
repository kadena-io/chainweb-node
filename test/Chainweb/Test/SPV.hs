{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.SPV
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.SPV
( tests
, spvTransactionRoundtripTest
, spvTransactionOutputRoundtripTest
, apiTests
) where

import Control.Lens ((^?!), view)
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.CAS
import Data.Foldable
import Data.Functor.Of
import qualified Data.List as L
import Data.Reflection hiding (int)
import qualified Data.Vector.Unboxed as V

import Numeric.Natural

import Servant.Client

import Statistics.Regression

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MockTx)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI.Client
import Chainweb.SPV.VerifyProof
import Chainweb.Test.CutDB
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- Test Tree

-- FIXME: These tests is randomized, and should either be rewritten using
-- quickCheck instead of HUnit or should be derandomized.
--
tests :: RocksDb -> TestTree
tests rdb = testGroup "SPV tests"
    [ testCaseStepsN "SPV transaction proof" 10 (spvTransactionRoundtripTest rdb version)
    , testCaseStepsN "SPV transaction output proof" 10 (spvTransactionOutputRoundtripTest rdb version)
    , apiTests rdb True version
    , testCaseSteps "SPV transaction proof test" (spvTest rdb version)
    ]
  where
    version = Test petersonChainGraph

-- -------------------------------------------------------------------------- --
-- Utils

type Step = String -> IO ()

testCaseStepsN :: String -> Natural -> (Step -> Assertion) -> TestTree
testCaseStepsN name n test = testGroup name $ flip map [1..n] $ \i ->
    testCaseSteps ("Run test number " <> sshow i) test

-- Find a reachable target chain
--
targetChain :: Cut -> BlockHeader -> IO ChainId
targetChain c srcBlock = do
    cids <- generate (shuffle $ toList $ chainIds c)
    go cids
  where
    graph = _chainGraph c

    go [] = error
        $ "SPV proof test failed to find a reachable target chain. This is a bug in the test code"
        <> ". source block: " <> sshow srcBlock
        <> ". current cut: " <> sshow c
    go (h:t) = if isReachable h then return h else go t

    chainHeight trgChain = _blockHeight (c ^?! ixg trgChain)

    isReachable trgChain
        = _blockHeight srcBlock <= chainHeight trgChain - distance trgChain

    distance x = len $ shortestPath (_chainId srcBlock) x graph

-- -------------------------------------------------------------------------- --
-- Comprehensive SPV test for transaction proofs

-- | Creates a test chainweb instance and exhaustively creates SPV transaction
-- proofs for each transaction on each chain.
--
-- Also checks that the of the created proofs meets the expecatations.
--
spvTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTest rdb v step = do
    withTestCutDbWithoutPact rdb v 100 (\_ _ -> return ()) $ \cutDb -> do
        curCut <- _cutMap <$> _cut cutDb

        -- for each blockheader h in cut
        samples <- S.each (toList curCut)
            -- for each ancestor ah of h
            & flip S.for (\h -> ancestors (cutDb ^?! cutDbBlockHeaderDb h) (_blockHash h))
            -- for each transaction in ah
            & flip S.for (\h -> getPayloads cutDb h)
            -- for each target chain c
            & flip S.for (\(a,b,c,d) -> S.each $ (a,b,c,d,) <$> toList (chainIds cutDb))
            -- Create and verify transaction proof
            & S.mapM (go cutDb)
            -- Ingore all cases where a proof couldn't be created
            & S.concat
            & S.toList_

        -- Confirm size of proofs
        let (coef, r) = regress samples
        step $ show r
        step $ show coef
        assertBool "proof size is not constant in the block height" (r > 0.8)

        let (coef', r') = regressWithHeightDiff samples
        step $ show r'
        step $ show coef'
        assertBool "proof size is not constant in the block height" (abs (coef' V.! 1) < 1)
  where

    -- Stream of all transactions in a block. Each returned item is
    -- - block header
    -- - block size (tx count)
    -- - tx index
    -- - tx
    --
    getPayloads
        :: PayloadCas cas
        => CutDb cas
        -> BlockHeader
        -> S.Stream (Of (BlockHeader, Int, Int, Transaction)) IO ()
    getPayloads cutDb h = do
        pay <- liftIO $ casLookupM (view cutDbPayloadCas cutDb) (_blockPayloadHash h)
        let n = length $ _payloadWithOutputsTransactions pay
        S.each (zip [0..] $ fmap fst $ toList $ _payloadWithOutputsTransactions pay)
            & S.map (\(b,c) -> (h,n,b,c))

    -- Given
    -- - block header,
    -- - tx count of block,
    -- - tx index,
    -- - tx, and
    -- - target chain,
    --
    -- creates and verifies SPV proofs and returns list of
    -- - tx proof length
    -- - block size (tx count)
    -- - block height difference
    -- - distance between source chain and target chain
    -- - size of tx
    --
    go
        :: PayloadCas cas
        => CutDb cas
        -> (BlockHeader, Int, Int, Transaction, ChainId)
        -> IO (Maybe [Double])
    go cutDb (h, n, txIx, tx, trgChain) = do

        let inner = do
                -- create inclusion proof for transaction
                proof <- createTransactionProof cutDb trgChain
                    (_chainId h) -- source chain
                    (_blockHeight h) -- source block height
                    txIx -- transaction index
                subj <- verifyTransactionProof cutDb proof
                assertEqual "transaction proof subject matches transaction" tx subj

                -- return (proof size, block size, height, distance, tx size)
                return
                    [ int $ BL.length $ encode proof
                    , int n
                    , int $ _blockHeight h
                    , int $ distance cutDb h trgChain
                    , int $ B.length (_transactionBytes tx)
                    ]

        isReachable <- reachable cutDb h trgChain
        try inner >>= \case
            Right x -> do
                assertBool "SPV proof creation succeeded although target chain is not reachable" isReachable
                return (Just x)
            Left SpvExceptionTargetNotReachable{} -> do
                assertBool "SPV proof creation failed although target chain is reachable" (not isReachable)
                return Nothing
            Left e -> throwM e

    -- Distance between source chain an target chain
    --
    distance cutDb h trgChain = length
        $ shortestPath (_chainId h) trgChain
        $ _chainGraph cutDb

    -- Check whether target chain is reachable from the source block
    --
    reachable :: CutDb as -> BlockHeader -> ChainId -> IO Bool
    reachable cutDb h trgChain = do
        m <- maxRank $ cutDb ^?! cutDbBlockHeaderDb trgChain
        return $ (int m - int (_blockHeight h)) >= (distance cutDb h trgChain)

    -- regression model with @createTransactionProof@. Proof size doesn't depend on target height.
    --
    regress r
        | [proofSize, blockSize, _heightDiff, chainDist, txSize] <- V.fromList <$> L.transpose r
            = olsRegress [V.map (logBase 2) blockSize, chainDist, txSize] proofSize
        | otherwise = error "Chainweb.Test.SPV.spvTest.regress: fail to match regressor list. This is a bug in the test code."

    -- regression model for @createTransactionProof'@. Proof size depends on target height.
    -- When used with @createTransactionProof@ the coefficient for the target height must be small.
    --
    regressWithHeightDiff r
        | [proofSize, blockSize, heightDiff, chainDist, txSize] <- V.fromList <$> L.transpose r
            = olsRegress [V.map (logBase 2) blockSize, heightDiff, chainDist, txSize] proofSize
        | otherwise = error "Chainweb.Test.SPV.spvTest.regress: fail to match regressor list. This is a bug in the test code."


-- -------------------------------------------------------------------------- --
-- SPV Tests

spvTransactionRoundtripTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTransactionRoundtripTest rdb v step = do
    step "setup cut db"
    withTestCutDbWithoutPact rdb v 100 (\_ _ -> return ()) $ \cutDb -> do
        step "pick random transaction"
        (h, txIx, tx, _) <- randomTransaction cutDb

        step "pick a reachable target chain"
        curCut <- _cut cutDb
        trgChain <- targetChain curCut h

        step "create inclusion proof for transaction"
        proof <- createTransactionProof
            cutDb
                -- CutDb
            trgChain
                -- target chain
            (_chainId h)
                -- source chain
            (_blockHeight h)
                -- source block height
            txIx
                -- transaction index

        step "json encoding roundtrip of proof"
        assertEqual "decode proof equals original proof"
            (Right proof)
            (eitherDecode (encode proof))

        step "verify proof"
        subj <- verifyTransactionProof cutDb proof

        step "confirm that proof subject matches transaction"
        assertEqual "proof subject matches transaction" tx subj

spvTransactionOutputRoundtripTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTransactionOutputRoundtripTest rdb v step = do
    step "setup cut db"
    withTestCutDbWithoutPact rdb v 100 (\_ _ -> return ()) $ \cutDb -> do

        step "pick random transaction output"
        (h, outIx, _, out) <- randomTransaction cutDb

        step "pick a reachable target chain"
        curCut <- _cut cutDb
        trgChain <- targetChain curCut h

        step "create inclusion proof for transaction output"
        proof <- createTransactionOutputProof
            cutDb
                -- CutDb
            trgChain
                -- target chain
            (_chainId h)
                -- source chain
            (_blockHeight h)
                -- source block height
            outIx
                -- transaction index

        step "json encoding roundtrip of proof"
        assertEqual "decode proof equals original proof"
            (Right proof)
            (eitherDecode (encode proof))

        step "verify proof"
        subj <- verifyTransactionOutputProof cutDb proof

        step "confirm that proof subject matches transaction output"
        assertEqual "proof subject matches transaction output" out subj

-- -------------------------------------------------------------------------- --
-- REST API

type TestClientEnv_ = TestClientEnv MockTx RocksDbCas

apiTests :: RocksDb -> Bool -> ChainwebVersion -> TestTree
apiTests rdb tls v = withTestPayloadResource rdb v 100 (\_ _ -> return ()) $ \dbsIO ->
    testGroup "SPV API tests"
        [ withPayloadServer tls v (fst <$> dbsIO) (payloadDbs . snd <$> dbsIO) $ \env ->
            testCaseStepsN "spv api tests (without tls)" 10 (txApiTests env)
        , withPayloadServer tls v (fst <$> dbsIO) (payloadDbs . snd <$> dbsIO) $ \env ->
            testCaseStepsN "spv api tests (with tls)" 10 (txApiTests env)
        ]
  where
    cids = toList $ chainIds v
    payloadDbs db = (, db) <$> cids

txApiTests :: IO TestClientEnv_ -> Step -> IO ()
txApiTests envIO step = do
    PayloadTestClientEnv env cutDb payloadDbs v <- envIO
    give (snd . head $ payloadDbs) $ do

        step "pick random transaction"
        (h, txIx, tx, out) <- randomTransaction cutDb
        step $ "picked random transaction, height: " <> sshow (_blockHeight h) <> ", ix: " <> sshow txIx

        curCut <- _cut cutDb
        trgChain <- targetChain curCut h
        step $ "picked a reachable target chain, chain id: " <> sshow trgChain

        -- Transaction Proof:

        step "request transaction proof"
        txProof <- flip runClientM env $
            spvGetTransactionProofClient v trgChain (_chainId h) (_blockHeight h) (int txIx)

        case txProof of

            Left err -> do
                assertFailure $ "request for transaction proof failed: " <> sshow err

            Right proof -> do
                step "verify transaction proof"
                subj <- verifyTransactionProof cutDb proof

                step "confirm that transaction proof subject matches transaction"
                assertEqual "proof subject matches transaction" tx subj

        -- Transaction Output Proof:

        step "request transaction output proof"
        outProof <- flip runClientM env $
            spvGetTransactionOutputProofClient v trgChain (_chainId h) (_blockHeight h) (int txIx)

        case outProof of

            Left err ->
                assertFailure $ "request for transaction output proof failed: " <> sshow err

            Right proof -> do
                step "verify transaction output proof"
                subj <- verifyTransactionOutputProof cutDb proof

                step "confirm that transaction output proof subject matches transaction output"
                assertEqual "proof subject matches transaction output" out subj

