{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module: Chainweb.Test.CutDB.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV Support roundtrip tests
--
module Chainweb.Test.Pact.SPV
( tests
) where

import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Control.Exception (SomeException, finally)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (catch)

import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Function
import Data.Functor (void)
import Data.IORef
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)

import NeatInterpolation (text)

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Parse
import Pact.Types.ChainMeta as Pact
import Pact.Types.Term

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.Types
import Chainweb.SPV.CreateProof
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb

import Data.CAS.RocksDB
import Data.LogMessage


tests :: TestTree
tests = testGroup "Chainweb-Pact SPV integration"
    [ testCase "standard SPV verification round trip" standard
    , testCase "double spends fail" doublespend
    , testCase "wrong chain execution fails" wrongchain
    , testCase "invalid proof formats fail" badproof
    ]

v :: ChainwebVersion
v = TimedCPM petersonChainGraph

logg :: LogMessage a => LogLevel -> a -> IO ()
logg l
  | l <= Warn = T.putStrLn . logText
  | otherwise = const $ return ()

diam :: Num a => a
diam = int . diameter . _chainGraph $ v

gorder :: Num a => a
gorder = int . order . _chainGraph $ v

height :: Chainweb.ChainId -> Cut -> BlockHeight
height cid c = _blockHeight $ c ^?! ixg cid

handle :: SomeException -> IO Bool
handle _ = return False

-- expected failures take this form
expectedFailure :: IO Bool -> String -> Assertion
expectedFailure test msg = do
    b <- catch test handle
    assertBool ("Unexpected success: " <> msg <> " should fail") (not b)

expectedSuccess :: IO Bool -> String -> Assertion
expectedSuccess test msg = do
    b <- catch test handle
    assertBool ("Unexpected failure: " <> msg <> " should succeed") b

-- -------------------------------------------------------------------------- --
-- tests

standard :: Assertion
standard = expectedSuccess (roundtrip 0 1 txGenerator1 txGenerator2) "round trip"

doublespend :: Assertion
doublespend = expectedFailure (roundtrip 0 1 txGenerator1 txGenerator3) "double spend"

wrongchain :: Assertion
wrongchain = expectedFailure (roundtrip 0 1 txGenerator1 txGenerator4) "wrong chain execution"

badproof :: Assertion
badproof = expectedFailure (roundtrip 0 1 txGenerator1 txGenerator5) "wrong proof format"

withAll :: ChainwebVersion -> ([SQLiteEnv] -> IO c) -> IO c
withAll vv f = foldl' (\soFar _ -> with soFar) f (chainIds vv) []
  where
    with :: ([SQLiteEnv] -> IO c) -> [SQLiteEnv] -> IO c
    with g envs =  withTempSQLiteConnection fastNoJournalPragmas $ \s -> g (s : envs)

roundtrip
    :: Int
      -- ^ source chain id
    -> Int
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> IO Bool
roundtrip _sid _tid burn create =
    withAll v $ \sqlenv0s ->
      withAll v $ \sqlenv1s ->
        withAll v $ \sqlenv2s -> do

          -- Pact service that is used to initialize the cut data base
          pact0 <- testWebPactExecutionService v Nothing (return mempty) sqlenv0s
          withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
              withTestCutDb rdb v 20 pact0 logg $ \cutDb -> do
                  cdb <- newMVar cutDb

                  sid <- mkChainId v _sid
                  tid <- mkChainId v _tid

                  -- pact service, that is used to extend the cut data base
                  pact1 <- testWebPactExecutionService v (Just cdb) (burn tid) sqlenv1s
                  syncPact cutDb pact1

                  c0 <- _cut cutDb

                  -- get tx output from `(coin.delete-coin ...)` call.
                  -- Note: we must mine at least (diam + 1) * graph order many blocks
                  -- to ensure we synchronize the cutdb across all chains
                  c1 <- fmap fromJuste $ extendAwait cutDb pact1 ((diam + 1) * gorder) $
                      ((<) `on` height sid) c0

                  -- A proof can only be constructed if the block hash of the source
                  -- block is included in the block hash of the target. Extending the
                  -- cut db with `distance(source, target) * order(graph) + 2 *
                  -- diameter(graph) * order(graph)` should be sufficient to guarantee
                  -- that a proof exists (modulo off-by-one errors)

                  -- So, if the distance is 2, you would mine 10 (order of peterson
                  -- graph) * 2 new blocks. Since extending the cut db picks chains
                  -- randomly you mine another 2 * diameter(graph) * 10 = 40 blocks to
                  -- make up for uneven height distribution.

                  -- So in total you would add 60 blocks which would guarantee that
                  -- all chains advanced by at least 2 blocks. This is probably an
                  -- over-approximation, I guess the formula could be made a little
                  -- more tight, but the that’s the overall idea. The idea behind the
                  -- `2 * diameter(graph) * order(graph)` corrective is that, the
                  -- block heights between any two chains can be at most
                  -- `diameter(graph)` apart.

                  c2 <- fmap fromJuste $ extendAwait cutDb pact1 60 $ \c ->
                      height tid c > diam + height tid c0

                  -- execute '(coin.create-coin ...)' using the  correct chain id and block height
                  txGen2 <- create cdb sid tid (height sid c1)
                  pact2 <- testWebPactExecutionService v (Just cdb) txGen2 sqlenv2s
                  syncPact cutDb pact2

                  -- consume the stream and mine second batch of transactions
                  void $ extendAwait cutDb pact2 (diam * gorder)
                      $ ((<) `on` height tid) c2

                  return True

-- -------------------------------------------------------------------------- --
-- transaction generators

type TransactionGenerator
    = Chainweb.ChainId -> BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction)

type BurnGenerator
    = Chainweb.ChainId -> TransactionGenerator

type CreatesGenerator
    = MVar (CutDb RocksDbCas) -> Chainweb.ChainId -> Chainweb.ChainId -> BlockHeight -> IO TransactionGenerator

-- | Generate burn/create Pact Service commands on arbitrarily many chains
--
txGenerator1 :: BurnGenerator
txGenerator1 tid _cid _bhe _bha _ = do
    ks <- testKeyPairs

    let pcid = Pact.ChainId $ chainIdToText _cid
        g = ParsedInteger 100
        gr = ParsedDecimal 0.0001

    mkPactTestTransactions "sender00" pcid ks "1" g gr txs
  where
    txs = fromList [ PactTransaction tx1Code tx1Data ]

    tx1Code =
      [text|
        (coin.delete-coin 'sender00
          (read-msg 'target-chain-id)
          'sender01
          (read-keyset 'sender01-keyset)
          1.0)
        |]

    tx1Data =
      -- sender01 keyset guard
      let ks = KeySet
            [ "6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7" ]
            (Name "keys-all" def)

      in Just $ object
         [ "sender01-keyset" .= ks
         , "target-chain-id" .= chainIdToText tid ]

-- | Generate the 'create-coin' command in response to the previous 'delete-coin' call.
-- Note that we maintain an atomic update to make sure that if a given chain id
-- has already called the 'create-coin' half of the transaction, it will not do so again.
--
txGenerator2 :: CreatesGenerator
txGenerator2 cdbv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref tid' _bhe _bha _
        | tid /= tid' = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid sid bhe 0

                let pcid = Pact.ChainId (chainIdToText tid)
                    g = ParsedInteger 100
                    gr = ParsedDecimal 0.0001

                ks <- testKeyPairs

                mkPactTestTransactions "sender00" pcid ks "1" g gr (txs q)
                    `finally` writeIORef ref True

    txs q = fromList
      [ PactTransaction tx1Code (tx1Data q)
      ]

    tx1Code =
      [text|
        (coin.create-coin (read-msg 'proof))
        |]

    tx1Data q = Just $ object [ "proof" .= q ]

-- | Double spend transaction which calls the coin-create
-- function twice
txGenerator3 :: CreatesGenerator
txGenerator3 cdbv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref tid' _bhe _bha _
        | tid /= tid' = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid sid bhe 0

                let pcid = Pact.ChainId (chainIdToText tid)
                    g = ParsedInteger 100
                    gr = ParsedDecimal 0.0001

                ks <- testKeyPairs
                mkPactTestTransactions "sender00" pcid ks "1" g gr (txs q)
                    `finally` writeIORef ref True

    txs q = fromList
      [ PactTransaction tx1Code (tx1Data q)
      ]

    tx1Code =
      [text|
        (coin.create-coin (read-msg 'proof))
        (coin.create-coin (read-msg 'proof))
        |]

    tx1Data q = Just $ object [ "proof" .= q ]

-- | Execute on the create-coin command on the wrong target chain
txGenerator4 :: CreatesGenerator
txGenerator4 cdbv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref tid' _bhe _bha _
        | tid /= tid' = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid sid bhe 0

                let pcid = Pact.ChainId (chainIdToText sid)
                    g = ParsedInteger 100
                    gr = ParsedDecimal 0.0001

                ks <- testKeyPairs
                mkPactTestTransactions "sender00" pcid ks "1" g gr (txs q)
                    `finally` writeIORef ref True

    txs q = fromList
      [ PactTransaction tx1Code (tx1Data q)
      ]

    tx1Code =
      [text|
        (coin.create-coin (read-msg 'proof))
        |]

    tx1Data q = Just $ object [ "proof" .= q ]

-- | Execute create-coin command with invalid proof
txGenerator5 :: CreatesGenerator
txGenerator5 _cdbv _ tid _ = do
    ref <- newIORef False
    return $ go ref
  where
    go ref tid' _bhe _bha _
        | tid /= tid' = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do

                let pcid = Pact.ChainId (chainIdToText tid)
                    g = ParsedInteger 100
                    gr = ParsedDecimal 0.0001

                ks <- testKeyPairs
                mkPactTestTransactions "sender00" pcid ks "1" g gr txs
                    `finally` writeIORef ref True

    txs = fromList
      [ PactTransaction tx1Code Nothing
      ]

    tx1Code =
      [text|
        (coin.create-coin { "invalid" : "proof" })
        |]
