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
( test
) where


import Control.Lens ((^.), at)
import Control.Concurrent.MVar (MVar, readMVar, newMVar)
import Control.Exception (throwIO)

import Data.Aeson (FromJSON, Value, (.=), object)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Default (def)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.LogMessage
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)

import Crypto.Hash.Algorithms

import NeatInterpolation (text)

import qualified Streaming.Prelude as S

import System.LogLevel

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Pact.SPV
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Transaction
import Chainweb.Version

import Data.CAS
import Data.CAS.RocksDB


-- internal pact modules

import Pact.Types.Command
import Pact.Types.Term
import Pact.Types.Exp

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

            -- get tx output from `(coin.delete-coin ...)` call
            Just (_, _, outs1) <- S.head_ $ extendTestCutDb cutDb pact1 1
            (_, txo1) <- payloadTx outs1

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
            void $! S.effects $ extendTestCutDb cutDb pact1 60
            syncPact cutDb pact1

            pact2 <- testWebPactExecutionService v (Just cdb) $ txGenerator2 cdb txo1
            syncPact cutDb pact2

            void $! S.head_ $ extendTestCutDb cutDb pact2 1


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
    mkPactTestTransactions' txs
  where
    txs =
      let c =
            [text|
              (coin.delete-coin 'sender00 1 'sender01 (read-keyset 'sender01-keys) 1.0)
              |]

      in fromList [ PactTransaction c keys ]


-- | Generate the 'create-coin' command in response
-- to the previous 'delete-coin' call
--
txGenerator2
    :: PayloadCas cas
    => MVar (CutDb cas)
    -> TransactionOutput
    -> TransactionGenerator
txGenerator2 cdbv p _cid _bhe _bha = do
    cdb <- readMVar cdbv
    q <- extractHash p
    r <- fmap A.toJSON
      $ createTransactionProof cdb (unsafeChainId 1) (unsafeChainId 0) 0 0
    print $ txs q r
    mkPactTestTransactions' (txs q r)
  where
    txs q r = fromList
      [ PactTransaction tx1Code (tx1Data q r)
      ]

    tx1Code =
      [text|
        (coin.create-coin
          { "create-chain-id": 1
          , "delete-tx-hash": (read-msg 'delete-hash)
          , "delete-account": "sender00"
          , "create-account": "sender01"
          , "quantity": 1.0
          , "create-account-guard": (read-keyset 'sender01-keys)
          , "delete-chain-id": 0
          , "spv-proof" : (read-msg 'proof)
          })
        |]

    tx1Data q r =
      let k = KeySet
            [ PublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" ]
            ( Name "keys-all" def )

      in Just $ object
         [ "sender01-keys" .= k
         , "delete-hash"   .= q
         , "proof"         .= r
         ]

-- | Unwrap a 'PayloadWithOutputs' and retrieve just the information
-- we need in order to execute an SPV request to the api
--
payloadTx :: PayloadWithOutputs -> IO (Transaction, TransactionOutput)
payloadTx = go . toList . _payloadWithOutputsTransactions
  where
    go [(tx,txo)] = pure (tx,txo)
    go _ = throwIO . userError $
      "Single tx yielded multiple tx outputs"

-- | Test admin keys (see 'Chainweb.Test.Pact.Utils')
--
keys :: Maybe Value
keys = Just $ object [ "sender01-keys" .= k ]
  where
    k = KeySet
      [ PublicKey "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" ]
      ( Name "keys-all" def )

-- | Given a 'TransactionOutput', we must extract the data yielded
-- by a successful result, so that we can pass this along to as the
-- 'coin.create-coin' proof
extractHash :: TransactionOutput -> IO String
extractHash (TransactionOutput t) = do
    hl <- fromBS @HashedLogTxOutput t _hlCommandResult
    cr <- toResult hl
    o <- toObject cr
    getHash o
  where

    toResult :: Value -> IO Value
    toResult v = fromValue v _csData

    toObject :: Value -> IO (Term Name)
    toObject v = fromValue v $ \o -> TObject o def

    getHash o = case o of
      (TObject (Object (ObjectMap m) _ _ _) _) ->
        case m ^. at (FieldKey "delete-tx-hash") of
          Nothing -> aesonErr "extractHash: unable to locate delete tx hash"
          Just a -> case a of
            (TLiteral (LString h) _) -> pure . unpack $ h
            _ -> aesonErr "extractHash: tx hash has wrong literal type"
      _ -> aesonErr "extractHash: wrong term type - object required"


fromValue :: FromJSON a => Value -> (a -> b) -> IO b
fromValue v f = case A.fromJSON v of
    A.Error e -> aesonErr e
    A.Success s -> pure . f $ s

fromBS :: FromJSON a => ByteString -> (a -> b) -> IO b
fromBS bs f = maybe err (pure . f)
    . A.decode
    . fromStrict
    $ bs
  where
    err = internalError "spvTests: could not decode bytes"

aesonErr :: String -> IO a
aesonErr s = internalError'
  $ "spvTests: could not decode proof object: "
  <> s
