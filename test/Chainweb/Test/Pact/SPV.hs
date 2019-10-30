{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
( -- * test suite
  tests
  -- * repl tests
, standard
, wrongChain
, wrongChainProof
, invalidProof
) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, finally, throwIO)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (catch)

import Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Function
import Data.Functor (void)
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (pack)
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)
import qualified Data.Vector as Vector

import NeatInterpolation (text)

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.ChainId as Pact
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (toPactId)
import Pact.Types.SPV
import Pact.Types.Term


-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.SPV.CreateProof
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb

import Data.CAS.RocksDB
import Data.LogMessage


-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.SPV"
    [ testCase "standard SPV verification round trip" standard
    , testCase "wrong chain execution fails" wrongChain
    , testCase "invalid proof formats fail" invalidProof
    , testCase "wrong target chain in proofs fail" wrongChainProof
    ]

v :: ChainwebVersion
v = FastTimedCPM triangleChainGraph

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

handle :: SomeException -> IO (Bool, String)
handle e = return (False, show e)

-- debugging
_handle' :: SomeException -> IO (Bool, String)
_handle' e =
    let
      s = show e
    in logg System.LogLevel.Error (pack s) >> return (False, s)

-- | expected failures take this form.
--
expectFailure :: String -> IO (Bool, String) -> Assertion
expectFailure err test = do
    (b, s) <- catch test handle
    if err `isInfixOf` s then
      assertBool "Unexpected success" $ not b
    else throwIO $ userError s


-- | expected successes take this form
--
expectSuccess :: IO (Bool, String) -> Assertion
expectSuccess test = do
    (b, s) <- catch test handle
    assertBool ("Unexpected failure: " <> s) b

-- -------------------------------------------------------------------------- --
-- tests

standard :: Assertion
standard = expectSuccess $ roundtrip 0 1 txGenerator1 txGenerator2

wrongChain :: Assertion
wrongChain = expectFailure "enforceYield: yield provenance" $
    roundtrip 0 1 txGenerator1 txGenerator3

invalidProof :: Assertion
invalidProof = expectFailure "resumePact: no previous execution found" $
    roundtrip 0 1 txGenerator1 txGenerator4

wrongChainProof :: Assertion
wrongChainProof = expectFailure "cannot redeem continuation proof on wrong target chain" $
    roundtrip 0 1 txGenerator1 txGenerator5


withAll :: ChainwebVersion -> ([SQLiteEnv] -> IO c) -> IO c
withAll vv f = foldl' (\soFar _ -> with soFar) f (chainIds vv) []
  where
    with :: ([SQLiteEnv] -> IO c) -> [SQLiteEnv] -> IO c
    with g envs =  withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)

roundtrip
    :: Int
      -- ^ source chain id
    -> Int
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> IO (Bool, String)
roundtrip sid0 tid0 burn create =
  withAll v $ \sqlenv0s -> withAll v $ \sqlenv1s -> withAll v $ \sqlenv2s -> do
    -- Pact service that is used to initialize the cut data base
    let pactIO bhdb pdb = testWebPactExecutionService v Nothing
                              (return bhdb) (return pdb)
                              (return mempty) sqlenv0s
    withTempRocksDb "chainweb-spv-tests" $ \rdb ->
        withTestCutDb rdb v 20 pactIO logg $ \cutDb -> do
            cdb <- newMVar cutDb

            sid <- mkChainId v sid0
            tid <- mkChainId v tid0

            -- track the continuation pact id
            pidv <- newEmptyMVar @PactId

            let pdb = _webBlockPayloadStoreCas $
                      view cutDbPayloadStore cutDb

            let webStoreDb = view cutDbWebBlockHeaderStore cutDb
            let webHeaderDb = _webBlockHeaderStoreCas webStoreDb

            -- pact service, that is used to extend the cut data base
            t1 <- getCurrentTimeIntegral
            txGen1 <- burn t1 pidv sid tid

            pact1 <- testWebPactExecutionService v (Just cdb)
                         (return webHeaderDb) (return pdb)
                         (chainToMPA txGen1) sqlenv1s
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

            -- So in total you would add 60 + 2 blocks which would guarantee that
            -- all chains advanced by at least 2 blocks. This is probably an
            -- over-approximation, I guess the formula could be made a little
            -- more tight, but the that’s the overall idea. The idea behind the
            -- `2 * diameter(graph) * order(graph)` corrective is that, the
            -- block heights between any two chains can be at most
            -- `diameter(graph)` apart.

            c2 <- fmap fromJuste $ extendAwait cutDb pact1 10 $ \c ->
                height tid c > diam + height sid c1

            -- execute '(coin.create-coin ...)' using the  correct chain id and block height
            t2 <- getCurrentTimeIntegral
            txGen2 <- create t2 cdb pidv sid tid (height sid c1)

            pact2 <- testWebPactExecutionService v (Just cdb)
                         (return webHeaderDb) (return pdb)
                         (chainToMPA txGen2) sqlenv2s
            syncPact cutDb pact2

            -- consume the stream and mine second batch of transactions
            void $ fmap fromJuste $ extendAwait cutDb pact2 ((diam + 1) * gorder) $
                ((<) `on` height tid) c2

            return (True, "test succeeded")


chainToMPA :: TransactionGenerator -> Chainweb.ChainId -> MemPoolAccess
chainToMPA f cid = MemPoolAccess
    { mpaGetBlock = const $ f cid
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork  = \_ -> return ()
    }

-- -------------------------------------------------------------------------- --
-- transaction generators

type TransactionGenerator
    = Chainweb.ChainId
    -> BlockHeight
    -> BlockHash
    -> BlockHeader
    -> IO (Vector ChainwebTransaction)

type BurnGenerator
    = Time Integer -> MVar PactId -> Chainweb.ChainId -> Chainweb.ChainId -> IO TransactionGenerator

type CreatesGenerator
    = Time Integer
    -> MVar (CutDb RocksDbCas)
    -> MVar PactId
    -> Chainweb.ChainId
    -> Chainweb.ChainId
    -> BlockHeight
    -> IO TransactionGenerator

-- | Generate burn/create Pact Service commands on arbitrarily many chains
--
txGenerator1 :: BurnGenerator
txGenerator1 time pidv sid tid = do
    ref0 <- newIORef False
    ref1 <- newIORef False
    return $ go ref0 ref1
  where
    go ref0 ref1 _cid _bhe _bha _
      | sid /= _cid = return mempty
      | otherwise = readIORef ref0 >>= \case
        True -> return mempty
        False -> do
            readIORef ref1 >>= \case
              True -> return mempty
              False -> do
                ks <- testKeyPairs sender00KeyPair Nothing

                let pcid = Pact.ChainId $ chainIdToText sid

                cmd <- mkTestExecTransactions "sender00" pcid ks "1" 10 0.01 100000 (toTxCreationTime time) txs
                  `finally` writeIORef ref0 True

                let pid = toPactId $ toUntypedHash $ _cmdHash (Vector.head cmd)

                putMVar pidv pid `finally` writeIORef ref1 True
                return cmd



    txs = fromList [ PactTransaction tx1Code tx1Data ]

    tx1Code =
      [text|
        (coin.transfer-crosschain
          'sender00
          'sender01
          (read-keyset 'sender01-keyset)
          (read-msg 'target-chain-id)
          1.0)
        |]

    tx1Data =
      -- sender01 keyset guard
      let ks = mkKeySet
            ["6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"]
            "keys-all"

      in Just $ object
         [ "sender01-keyset" .= ks
         , "target-chain-id" .= chainIdToText tid
         ]

-- | Generate the 'create-coin' command in response to the previous 'delete-coin' call.
-- Note that we maintain an atomic update to make sure that if a given chain id
-- has already called the 'create-coin' half of the transaction, it will not do so again.
--
txGenerator2 :: CreatesGenerator
txGenerator2 time cdbv pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid sid bhe 0

                let pcid = Pact.ChainId (chainIdToText tid)
                    proof = Just . ContProof . B64U.encode . toStrict . Aeson.encode . toJSON $ q

                ks <- testKeyPairs sender00KeyPair Nothing
                pid <- readMVar pidv

                mkTestContTransaction "sender00" pcid ks "1" 10 0.01 1 pid False proof 100000 (toTxCreationTime time) Null
                    `finally` writeIORef ref True

-- | Execute on the create-coin command on the wrong target chain
--
txGenerator3 :: CreatesGenerator
txGenerator3 time cdbv pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid sid bhe 0

                let pcid = Pact.ChainId (chainIdToText sid)
                    proof = Just . ContProof .  B64U.encode . toStrict . Aeson.encode $ q

                ks <- testKeyPairs sender00KeyPair Nothing
                pid <- readMVar pidv

                mkTestContTransaction "sender00" pcid ks "1" 10 0.01 1 pid False proof 100000 (toTxCreationTime time) Null
                    `finally` writeIORef ref True

-- | Execute create-coin command with invalid proof
--
txGenerator4 :: CreatesGenerator
txGenerator4 time _cdbv pidv _ tid _ = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do

                let pcid = Pact.ChainId (chainIdToText tid)

                ks <- testKeyPairs sender00KeyPair Nothing
                pid <- readMVar pidv

                mkTestContTransaction "sender00" pcid ks "1" 10 0.01 1 pid False Nothing 100000 (toTxCreationTime time) Null
                    `finally` writeIORef ref True

-- | Execute on the create-coin command on the correct target chain, with a proof
-- pointing at the wrong target chain
--
txGenerator5 :: CreatesGenerator
txGenerator5 time cdbv pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                tid' <- chainIdFromText "2"
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid' sid bhe 0

                let pcid = Pact.ChainId (chainIdToText sid)
                    proof = Just . ContProof .  B64U.encode . toStrict . Aeson.encode $ q

                ks <- testKeyPairs sender00KeyPair Nothing
                pid <- readMVar pidv

                mkTestContTransaction "sender00" pcid ks "1" 10 0.01 1 pid False proof 100000 (toTxCreationTime time) Null
                    `finally` writeIORef ref True
