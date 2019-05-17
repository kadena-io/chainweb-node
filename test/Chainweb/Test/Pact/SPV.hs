{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Test.CutDB.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Pact.SPV
( tests
, spv
) where

import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Default (def)
import Data.Functor (void)
import Data.LogMessage
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)

import Test.Tasty
import Test.Tasty.HUnit

import NeatInterpolation (text)

import qualified Streaming.Prelude as S

import System.LogLevel

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
import Chainweb.Version

import Data.CAS.RocksDB


-- internal pact modules

import Pact.Types.Term

tests :: TestTree
tests = testGroup "Chainweb-Pact SPV support"
    [ testCase "SPV verification round trip" spv
    ]

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

            -- extract cut db so we can extract blockheight of source chain id
            c0 <- _cut cutDb
            cid <- mkChainId v (0 :: Int)
            let bh0 = _blockHeight $ c0 ^?! ixg cid

            -- get tx output from `(coin.delete-coin ...)` call
            void $! S.effects $! extendTestCutDb cutDb pact1 $ diam * gorder
            void $! awaitBlockHeight cutDb (succ bh0) cid

            -- in order to ensure that the cutdb has a chance to establic consensus
            -- we must enforce a wait time.
            c1 <- awaitCutSync' cutDb c0

            -- A proof can only be constructed if the block hash of the source block
            -- is included in the block hash of the target. Extending the cut db with
            -- `distance(source, target) * order(graph) + 2 * diameter(graph) * order(graph)`
            -- should be sufficient to guarantee that a proof exists
            -- (modulo off-by-one errors)

            -- So, if the distance is 2, you would mine 10 (order of peterson graph) * 2 new blocks.
            -- Since extending the cut db picks chains randomly you mine another
            -- 2 * diameter(graph) * 10 = 40 blocks to make up for uneven height distribution.

            -- So in total you would add 60 blocks which would guarantee that all chains
            -- advanced by at least 2 blocks. This is probably an over-approximation, I guess
            -- the formula could be made a little more tight, but the that’s the overall idea.
            -- The idea behind the `2 * diameter(graph) * order(graph)` corrective is that, the
            -- block heights between any two chains can be at most `diameter(graph)` apart.

            -- 'S.effects' forces the stream here. It -must- occur so that we evaluate the stream
            --
            void $! S.effects $! extendTestCutDb cutDb pact1 60
            syncPact cutDb pact1

            let bh1 = _blockHeight $ c1 ^?! ixg cid
            tid <- mkChainId cutDb (1 :: Int)

            -- waits must occur after each cutdb extension
            void $! awaitBlockHeight cutDb (diam + bh1) tid

            -- execute '(coin.create-coin ...)' using the  correct chain id and block height
            pact2 <- testWebPactExecutionService v (Just cdb) $! txGenerator2 cdb cid bh1
            syncPact cutDb pact2

            -- if we get this far, we have succeeded
            void $! S.effects $! extendTestCutDb cutDb pact2 $ diam * gorder


  where
    v = TimedCPM petersonChainGraph
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()

    diam :: Num a => a
    diam = int . diameter . _chainGraph $ v

    gorder :: Num a => a
    gorder = int . order . _chainGraph $ v

type TransactionGenerator
    = ChainId -> BlockHeight -> BlockHash -> IO (Vector ChainwebTransaction)

-- | Generate burn/create Pact Service commands
--
txGenerator1 :: TransactionGenerator
txGenerator1 _cid _bhe _bha =
    mkPactTestTransactions' txs
  where
    txs =
      let c =
            [text|
              (coin.delete-coin 'sender00 "0" 'sender01 (read-keyset 'sender01-keys) 1.0)
              |]

      in fromList [ PactTransaction c keys ]




-- | Generate the 'create-coin' command in response
-- to the previous 'delete-coin' call
--
txGenerator2
    :: PayloadCas cas
    => MVar (CutDb cas)
    -> ChainId
    -> BlockHeight
    -> TransactionGenerator
txGenerator2 cdbv cid bhe tid _bhe _bha =
    if tid /= unsafeChainId 0
    then mkPactTestTransactions' mempty
    else do
      cdb <- readMVar cdbv

      q <- fmap toJSON
        $ createTransactionOutputProof cdb tid cid bhe 0

      mkPactTestTransactions' (txs q)
  where
    txs q = fromList
      [ PactTransaction tx1Code (tx1Data q)
      ]

    tx1Code =
      [text|
        (coin.create-coin (read-msg 'proof))
        |]
    tx1Data q = Just $ object [ "proof" .= q ]

-- | Test admin keys (see 'Chainweb.Test.Pact.Utils')
--
keys :: Maybe Value
keys = Just $ object [ "sender01-keys" .= k ]
  where
    k = KeySet
      [ PublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" ]
      ( Name "keys-all" def )
