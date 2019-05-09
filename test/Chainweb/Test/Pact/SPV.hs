{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
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
( test
) where



import Control.Concurrent.MVar (newMVar)
import Control.Exception (throwIO)

import Data.Aeson (Value, (.=), object)
import Data.Default (def)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.LogMessage
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)


import NeatInterpolation (text)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.CutDB (CutDb)
import Chainweb.Graph
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Version

import Data.CAS.RocksDB


-- internal pact modules

import Pact.Types.Term (KeySet(..), PublicKey(..), Name(..))


test :: IO ()
test = do
    -- Pact service that is used to initialize the cut data base
    pact0 <- testWebPactExecutionService v Nothing (return mempty)
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pact0 logg $ \cutDb -> do
            cdb <- newMVar cutDb

            -- pact service, that is used to extend the cut data base
            pact1 <- testWebPactExecutionService v (Just cdb) txGenerator1
            syncPact cutDb pact1

            Just (_, _, outs1) <- S.head_ $ extendTestCutDb cutDb pact1 1
            (_, txo1) <- payloadTx outs1

            -- A proof can only be constructed if the block hash of the source block
            -- is included in the block hash of the target. Extending the cut db with
            -- `distance(source, target) * order(graph) + 2 * diameter(graph) * order(graph)`
            -- should be sufficient to guarantee that a proof exists
            -- (modulo off-by-one errors)

            -- So, if the distance is 2, you would mine 10 (order of peterson graph) * 2 new blocks.
            -- Since mining picks chains randomly you mine another 2 * diameter(graph) * 10 = 40
            -- blocks to make up for uneven height distribution.

            -- So in total you would add 60 blocks which would guarantee that all chains
            -- advanced by at least 2 blocks. This is probably an over-approximation, I guess
            -- the formula could be made a little more tight, but the that’s the overall idea.
            -- The idea behind the `2 * diameter(graph) * order(graph)` corrective is that, the
            -- block heights between any two chains can be at most `diameter(graph)` apart.

            --
            -- 'S.effects' forces the stream here. It -must- occur so that we evaluate the stream
            --
            void $! S.effects $ extendTestCutDb cutDb pact1 60
            syncPact cutDb pact1

            pact2 <- testWebPactExecutionService v (Just cdb) $ txGenerator2 cutDb
            syncPact cutDb pact2

            Just (_, _, outs2) <- S.head_ $ extendTestCutDb cutDb pact2 1
            (_, txo2) <- payloadTx outs2
            print txo2

  where
    v = TimedCPM petersonChainGraph
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()


type TransactionGenerator
    = ChainId -> BlockHeight -> BlockHash -> IO (Vector ChainwebTransaction)

-- | Generate burn/create Pact Service commands
--
txGenerator1 :: TransactionGenerator
txGenerator1 _cid _bhe _bha =
    mkPactTestTransactions' (fromList txs)
  where
    txs = [ PactTransaction tx1Code tx1Data ]

    -- Burn coin on chain '$cid' and create on chain 2, creating SPV proof
    tx1Code =
      [text|
        (coin.delete-coin 'sender00 1 'sender01 (read-keyset 'sender01-keys) 1.0)
        |]
    tx1Data = keys

txGenerator2
    :: PayloadCas cas
    => CutDb cas
    -> TransactionGenerator
txGenerator2 cdb _cid _bhe _bha = do
    txo <- createTransactionOutputProof cdb (unsafeChainId 1) (unsafeChainId 0) 0 0
    mkPactTestTransactions' $ fromList (txs txo)
  where
    txs txo =
      [ PactTransaction tx1Code (tx1Data txo)
      ]

    tx1Code =
      [text| (coin.create-coin 'proof) |]

    tx1Data txo =
      Just (object [ "proof" .= txo ])

-- | Unwrap a 'PayloadWithOutputs' and retrieve just the information
-- we need in order to execute an SPV request to the api
--
payloadTx :: PayloadWithOutputs -> IO (Transaction, TransactionOutput)
payloadTx = go . toList . _payloadWithOutputsTransactions
  where
    go [(tx,txo)] = pure (tx,txo)
    go _ = throwIO . userError $
      "Single tx yielded multiple tx outputs"

    -- standard admin keys
keys :: Maybe Value
keys = Just $ object [ "sender01-keys" .= k ]
  where
    k = KeySet
      [ PublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" ]
      ( Name "keys-all" def )
