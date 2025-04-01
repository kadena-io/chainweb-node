{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.SPV
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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

import Control.Lens (view, (^?!))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Crypto.Hash.Algorithms

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Functor.Of
import qualified Data.List as L
import Data.LogMessage
import Data.MerkleLog
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Numeric.Natural

import Servant.Client

import Statistics.Regression

import qualified Streaming.Prelude as S

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Cut hiding (join)
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Mempool.Mempool (MockTx)
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.OutputProof
import Chainweb.SPV.PayloadProof
import Chainweb.SPV.RestAPI.Client
import Chainweb.SPV.VerifyProof
import Chainweb.Test.CutDB hiding (tests)
import Chainweb.Test.Orphans.Internal
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions(barebonesTestVersion)
import Chainweb.TreeDB
import Chainweb.Utils hiding ((==>))
import Chainweb.Version

import Chainweb.Storage.Table.RocksDB

-- -------------------------------------------------------------------------- --
-- Test Tree

-- FIXME: These tests is randomized, and should either be rewritten using
-- quickCheck instead of HUnit or should be derandomized.
--
tests :: RocksDb -> TestTree
tests rdb = testGroup  "SPV tests"
    [ testCaseStepsN "SPV transaction proof" 10 (spvTransactionRoundtripTest rdb version)
    , testCaseStepsN "SPV transaction output proof" 10 (spvTransactionOutputRoundtripTest rdb version)
    , apiTests rdb version
    , testCaseSteps "SPV transaction proof test" (spvTest rdb version)
    , properties
    ]
  where
    version = barebonesTestVersion petersenChainGraph


-- -------------------------------------------------------------------------- --
-- Utils

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

    chainHeight trgChain = view blockHeight (c ^?! ixg trgChain)

    isReachable trgChain
        = view blockHeight srcBlock <= chainHeight trgChain - distance trgChain

    distance x = len $ shortestPath (_chainId srcBlock) x graph

-- -------------------------------------------------------------------------- --
-- QuickCheck PayloadOutput tests

properties :: TestTree
properties = testGroup "merkle proof properties"
    [ testGroup "ChainwebMerklehashAlgorithm"
        [ testProperty "prop_merkleProof_run" $ prop_merkleProof_run @ChainwebMerkleHashAlgorithm
        , testProperty "prop_outputProof_run" $ prop_outputProof_run @ChainwebMerkleHashAlgorithm
        , testProperty "prop_outputProof_run2" $ prop_outputProof_run2 @ChainwebMerkleHashAlgorithm
        , testProperty "prop_outputProof_subject" $ prop_outputProof_subject @ChainwebMerkleHashAlgorithm
        , testProperty "prop_outputProof_valid" $ prop_outputProof_valid
        ]
    , testGroup "ChainwebMerklehashAlgorithm"
        [ testProperty "prop_merkleProof_run" $ prop_merkleProof_run @Keccak_256
        , testProperty "prop_outputProof_run" $ prop_outputProof_run @Keccak_256
        , testProperty "prop_outputProof_run2" $ prop_outputProof_run2 @Keccak_256
        , testProperty "prop_outputProof_subject" $ prop_outputProof_subject @Keccak_256
        ]
    ]

prop_merkleProof_run :: MerkleHashAlgorithm a => MerkleProof a -> Bool
prop_merkleProof_run p = case runMerkleProof p of !_ -> True

prop_outputProof_run
    :: MerkleHashAlgorithm a
    => PayloadProof a
    -> Bool
prop_outputProof_run p =
    case runMerkleProof (_payloadProofBlob p) of !_ -> True

prop_outputProof_run2
    :: forall a
    . MerkleHashAlgorithm a
    => PayloadProof a
    -> Property
prop_outputProof_run2 p = case runOutputProof p of
  Left e -> counterexample ("failed to validate proof: " <> show e) False
  Right (!_, !_) -> property True

prop_outputProof_subject
    :: forall a
    . MerkleHashAlgorithm a
    => Property
prop_outputProof_subject = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx ->
            case runOutputProof $ mkTestOutputProof @a p (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (!_, !subject) ->
                subject === snd (_payloadWithOutputsTransactions p V.! idx)
      where
        s = V.length (_payloadWithOutputsTransactions p)

-- | This test compares the root from running the proof with the hash of the
-- input payload. Because 'mkTestPayloadOutputProof' accepts only values of type
-- @PayloadWithOutputs_ ChainwebMerkleHashAlgorithm@ as input, this test only
-- works with proofs that use 'ChainwebMerkleHashAlgorithm'.
--
prop_outputProof_valid :: Property
prop_outputProof_valid = forAll arbitraryPayloadWithStructuredOutputs go
  where
    go (ks, p) = s > 0 ==>
        forAll (choose (0, s-1)) $ \idx ->
            case runOutputProof $ mkTestOutputProof p (ks V.! idx) of
            Left e -> counterexample ("failed to validate proof: " <> show e) False
            Right (!rootHash, !subject) ->
                subject === snd (_payloadWithOutputsTransactions p V.! idx)
                .&.
                rootHash === _payloadWithOutputsPayloadHash p
      where
        s = V.length (_payloadWithOutputsTransactions p)

-- -------------------------------------------------------------------------- --
-- Comprehensive SPV test for transaction proofs

-- | Creates a test chainweb instance and exhaustively creates SPV transaction
-- output proofs for each transaction on each chain.
--
-- Also checks that the size of the created proofs meets the expectations.
--
spvTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTest rdb v step = do
    withTestCutDbWithoutPact rdb v id 100 logg $ \_ cutDb -> do
        curCut <- _cutMap <$> _cut cutDb

        -- for each blockheader h in cut
        samples <- S.each (toList curCut)
            -- for each ancestor ah of h
            & flip S.for (\h -> ancestors (cutDb ^?! cutDbBlockHeaderDb h) (view blockHash h))
            -- for each transaction in ah
            & flip S.for (getPayloads cutDb)
            -- for each target chain c
            & flip S.for (\(a,b,c,d) -> S.each $ (a,b,c,d,) <$> toList (chainIds cutDb))
            -- Create and verify transaction output proof
            & S.mapM (go cutDb)
            -- Ingore all cases where a proof couldn't be created
            & S.concat
            & S.toList_

        -- Confirm size of proofs
        let (coef, r2) = regress samples
        step $ show r2
        step $ show coef
        assertBool
            ("proof size is not constant in the block height: r-value " <> sshow r2 <> " is < 0.8")
            (r2 > 0.8)
  where
    logg :: LogFunction
    -- logg _ msg = step $ T.unpack $ logText msg
    logg _ _ = return ()

    -- Stream of all transactions in a block. Each returned item is
    -- - block header
    -- - block size (tx count)
    -- - tx index
    -- - tx
    --
    getPayloads
        :: CanReadablePayloadCas tbl
        => CutDb tbl
        -> BlockHeader
        -> S.Stream (Of (BlockHeader, Int, Int, TransactionOutput)) IO ()
    getPayloads cutDb h = do
        Just pay <- liftIO $ lookupPayloadWithHeight (view cutDbPayloadDb cutDb) (Just $ view blockHeight h) (view blockPayloadHash h)
        let n = length $ _payloadWithOutputsTransactions pay
        S.each (zip [0..] $ fmap snd $ toList $ _payloadWithOutputsTransactions pay)
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
        :: CanReadablePayloadCas tbl
        => CutDb tbl
        -> (BlockHeader, Int, Int, TransactionOutput, ChainId)
        -> IO (Maybe [Double])
    go cutDb (h, n, txIx, txOut, trgChain) = do

        let inner = do
                -- create inclusion proof for transaction
                proof <- createTransactionOutputProof cutDb trgChain
                    (_chainId h) -- source chain
                    (view blockHeight h) -- source block height
                    txIx -- transaction index
                subj <- verifyTransactionOutputProof cutDb proof
                assertEqual "transaction output proof subject matches transaction" txOut subj

                -- return (proof size, block size, height, distance, tx size)
                return
                    [ int $ BL.length $ encode proof
                    , int n
                    , int $ view blockHeight h
                    , int $ distance cutDb h trgChain
                    , int $ B.length (_transactionOutputBytes txOut)
                    ]

        isReachable <- reachable cutDb h trgChain
        try inner >>= \case
            Right x -> do
                let msg = "SPV proof creation succeeded although target chain is not reachable ("
                        <> "source height: " <> sshow (view blockHeight h)
                        <> ", distance: " <> sshow (distance cutDb h trgChain)
                        <> ")"
                assertBool msg isReachable
                return (Just x)
            Left SpvExceptionTargetNotReachable{} -> do
                let msg = "SPV proof creation failed although target chain is reachable ("
                        <> "source height: " <> sshow (view blockHeight h)
                        <> ", distance: " <> sshow (distance cutDb h trgChain)
                        <> ")"
                assertBool msg (not isReachable)
                return Nothing
            Left e -> throwM e

    -- Distance between source chain an target chain
    --
    distance cutDb h trgChain = length
        $ shortestPath (_chainId h) trgChain
        $ chainGraphAt cutDb (view blockHeight h)

    -- Check whether target chain is reachable from the source block
    --
    reachable :: CutDb as -> BlockHeader -> ChainId -> IO Bool
    reachable cutDb h trgChain = do
        m <- maxRank $ cutDb ^?! cutDbBlockHeaderDb trgChain
        return $ (int m - int (view blockHeight h)) >= distance cutDb h trgChain

    -- regression model with @createTransactionOutputProof@. Proof size doesn't
    -- depend on target height.
    --
    -- (This proof size is logarithmic in the block size and linear in the
    -- distance and transation size)
    --
    regress r
        | [proofSize, blockSize, _heightDiff, chainDist, txSize] <- VU.fromList <$> L.transpose r
            = olsRegress [VU.map (logBase 2) blockSize, chainDist, txSize] proofSize
        | otherwise = error "Chainweb.Test.SPV.spvTest.regress: fail to match regressor list. This is a bug in the test code."

    -- regression model for @createTransactionOutputProof'@. Proof size depends
    -- on target height.
    --
    -- regressWithHeightDiff r
    --     | [proofSize, blockSize, heightDiff, chainDist, txSize] <- V.fromList <$> L.transpose r
    --         = olsRegress [V.map (logBase 2) blockSize, heightDiff, chainDist, txSize] proofSize
    --     | otherwise = error "Chainweb.Test.SPV.spvTest.regress: fail to match regressor list. This is a bug in the test code."


-- -------------------------------------------------------------------------- --
-- SPV Tests

spvTransactionRoundtripTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTransactionRoundtripTest rdb v step = do
    step "setup cut db"
    withTestCutDbWithoutPact rdb v id 100 (\_ _ -> return ()) $ \_ cutDb -> do
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
            (view blockHeight h)
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
    withTestCutDbWithoutPact rdb v id 100 (\_ _ -> return ()) $ \_ cutDb -> do

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
            (view blockHeight h)
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

type TestClientEnv_ tbl = TestClientEnv MockTx tbl

apiTests :: RocksDb -> ChainwebVersion -> TestTree
apiTests rdb v = withResourceT (withTestPayloadResource rdb v 100 (\_ _ -> pure ())) $ \dbIO ->
    testGroup "SPV API tests"
        -- TODO: there is no openapi spec for this SPV API.
        [ withResourceT (join $ withPayloadServer DoNotValidateSpec False v <$> liftIO dbIO <*> (liftIO $ payloadDbs . view cutDbPayloadDb <$> dbIO)) $ \env ->
            testCaseStepsN "spv api tests (without tls)" 10 (txApiTests env)
        , withResourceT (join $ withPayloadServer DoNotValidateSpec True v <$> liftIO dbIO <*> (liftIO $ payloadDbs . view cutDbPayloadDb <$> dbIO)) $ \env ->
            testCaseStepsN "spv api tests (with tls)" 10 (txApiTests env)
        ]
  where
    cids = toList $ chainIds v

    payloadDbs :: CanReadablePayloadCas tbl' => PayloadDb tbl' -> [(ChainId, PayloadDb tbl')]
    payloadDbs db = (, db) <$> cids

txApiTests :: CanReadablePayloadCas tbl => IO (TestClientEnv_ tbl) -> Step -> IO ()
txApiTests envIO step = do
    PayloadTestClientEnv env cutDb _payloadDbs v <- envIO
    step "pick random transaction"
    (h, txIx, tx, out) <- randomTransaction cutDb

    step $ "picked random transaction, height: " <> sshow (view blockHeight h) <> ", ix: " <> sshow txIx

    curCut <- _cut cutDb
    trgChain <- targetChain curCut h
    step $ "picked a reachable target chain, chain id: " <> sshow trgChain

    -- Transaction Proof:

    step "request transaction proof"
    txProof <- flip runClientM env $
        spvGetTransactionProofClient v trgChain (_chainId h) (view blockHeight h) (int txIx)

    case txProof of

        Left err ->
            assertFailure $ "request for transaction proof failed: " <> sshow err

        Right proof -> do
            step "verify transaction proof"
            subj <- verifyTransactionProof cutDb proof

            step "confirm that transaction proof subject matches transaction"
            assertEqual "proof subject matches transaction" tx subj

    -- Transaction Output Proof:

    step "request transaction output proof"
    outProof <- flip runClientM env $
        spvGetTransactionOutputProofClient v trgChain (_chainId h) (view blockHeight h) (int txIx)

    case outProof of

        Left err ->
            assertFailure $ "request for transaction output proof failed: " <> sshow err

        Right proof -> do
            step "verify transaction output proof"
            subj <- verifyTransactionOutputProof cutDb proof

            step "confirm that transaction output proof subject matches transaction output"
            assertEqual "proof subject matches transaction output" out subj
