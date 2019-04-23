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

import Control.Lens ((^?!))

import Data.Aeson
import Data.Foldable
import Data.Reflection hiding (int)

import Numeric.Natural

import Servant.Client

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
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI.Client
import Chainweb.SPV.VerifyProof
import Chainweb.Test.CutDB
import Chainweb.Test.Utils
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
-- SPV Tests

spvTransactionRoundtripTest :: RocksDb -> ChainwebVersion -> Step -> IO ()
spvTransactionRoundtripTest rdb v step = do
    step "setup cut db"
    withTestCutDb rdb v 100 (\_ _ -> return ()) $ \cutDb -> do
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
    withTestCutDb rdb v 100 (\_ _ -> return ()) $ \cutDb -> do

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

