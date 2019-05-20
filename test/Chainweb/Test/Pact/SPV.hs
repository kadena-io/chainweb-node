{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
( tests
, spv
) where

import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Control.Exception (finally)
import Control.Lens hiding ((.=))

import Data.Aeson
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

import Pact.Types.ChainMeta as Pact

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb

import Data.CAS.RocksDB
import Data.LogMessage

tests :: TestTree
tests = testGroup "Chainweb-Pact SPV support"
    [ testCase "SPV verification round trip" spv
    ]

v :: ChainwebVersion
v = TimedCPM petersonChainGraph

spv :: IO ()
spv = do
    -- Pact service that is used to initialize the cut data base
    pact0 <- testWebPactExecutionService v Nothing (return mempty)
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pact0 logg $ \cutDb -> do
            cdb <- newMVar cutDb

            -- pact service, that is used to extend the cut data base
            pact1 <- testWebPactExecutionService v (Just cdb) txGenerator1
            syncPact cutDb pact1

            c0 <- _cut cutDb
            cid <- mkChainId v (0 :: Int)


            -- get tx output from `(coin.delete-coin ...)` call.
            -- Note: we must mine at least (diam + 1) * graph order many blocks
            -- to ensure we synchronize the cutdb across all chains
            c1 <- fmap fromJuste $ extendAwait cutDb pact1 ((diam + 1) * gorder) $
                ((<) `on` height cid) c0

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

            tid <- mkChainId cutDb (1 :: Int)
            c2 <- fmap fromJuste $ extendAwait cutDb pact1 60 $ \c ->
                height tid c > diam + height tid c0

            -- execute '(coin.create-coin ...)' using the  correct chain id and block height
            txGen2 <- txGenerator2 cdb cid (height cid c1)
            pact2 <- testWebPactExecutionService v (Just cdb) txGen2
            syncPact cutDb pact2

            -- consume the stream and mine second batch of transactions
            void $ extendAwait cutDb pact2 (diam * gorder)
                $ ((<) `on` height tid) c2

  where
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()

    diam :: Num a => a
    diam = int . diameter . _chainGraph $ v

    gorder :: Num a => a
    gorder = int . order . _chainGraph $ v

    height cid c = _blockHeight $ c ^?! ixg cid

type TransactionGenerator
    = Chainweb.ChainId -> BlockHeight -> BlockHash -> IO (Vector ChainwebTransaction)

-- | Generate burn/create Pact Service commands on arbitrarily many chains
--
txGenerator1 :: TransactionGenerator
txGenerator1 cid _bhe _bha =
    mkPactTestTransactions' "sender00" (Pact.ChainId $ chainIdToText cid) txs
  where
    txs =
      let c =
            [text|
              (coin.delete-coin 'sender00 "0" 'sender01 (read-keyset 'sender01-keyset) 1.0)
              |]
      in fromList [ PactTransaction c Nothing ]

-- | Generate the 'create-coin' command in response to the previous 'delete-coin' call.
-- Note that we maintain an atomic update to make sure that if a given chain id
-- has already called the 'create-coin' half of the transaction, it will not do so again.
--
txGenerator2
    :: PayloadCas cas
    => MVar (CutDb cas)
    -> Chainweb.ChainId
    -> BlockHeight
    -> IO TransactionGenerator
txGenerator2 cdbv cid bhe = do
    ref <- newIORef False
    cid' <- mkChainId v (0 :: Int)
    return $ go ref cid'
  where
    go ref cid' tid _bhe _bha
        | tid /= cid' = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                cdb <- readMVar cdbv
                q <- fmap toJSON
                    $ createTransactionOutputProof cdb tid cid bhe 0
                mkPactTestTransactions' "sender00" (Pact.ChainId $ chainIdToText tid) (txs q)
                    `finally` writeIORef ref True

    txs q = fromList
      [ PactTransaction tx1Code (tx1Data q)
      ]

    tx1Code =
      [text|
        (coin.create-coin (read-msg 'proof))
        |]

    tx1Data q = Just $ object [ "proof" .= q ]
