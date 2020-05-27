{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Test.Rosetta
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Test.Rosetta
  ( tests
  ) where

import Data.Aeson
import Data.Decimal
import Data.Map (Map)
import Data.Word (Word64)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Pact.Types.Runtime (TxId(..))
import Pact.Types.Command (RequestKey(..))
import Pact.Types.Hash (Hash(..))

import Rosetta

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

--import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.RestAPI.Server
import Chainweb.Rosetta.Util

---


tests :: TestTree
tests = testGroup "Chainweb.Test.Rosetta.Server"
  [ testGroup "Unit Tests"
    [ testCase "matchNonGenesisBlockTransactionsToLogs" matchNonGenesisBlockTransactionsToLogs
    , testCase "matchNonGenesisSingleTransactionsToLogs" matchNonGenesisSingleTransactionsToLogs
    ]
  ]

matchNonGenesisBlockTransactionsToLogs :: Assertion
matchNonGenesisBlockTransactionsToLogs = do
  Right [r1, r2, r3, r4, r5] <- pure run
  assertEqualEncode "coinbase tx" r1 expected1
  assertEqualEncode "successful, non-coin 1" r2 expected2
  assertEqualEncode "successful, non-coin 2" r3 expected3
  assertEqualEncode "successful, coin" r4 expected4
  assertEqualEncode "unsuccessful" r5 expected5
  where
    run :: Either String [(T.Text, [Operation])] 
    run = nonGenesisTransactions logs getTxId toTx initial rest
    
    toTx (_, rk) ops = (rk, ops)
    getTxId (tid, _) = tid
    (logs, initial, rest) = mockTxLogs

    -- Coinbase Tx
    expected1 = ("ReqKey1", [ op CoinbaseReward 1 "miner1" 2.0 0])
    
    -- Successful, non-coin contract tx
    expected2 = ("ReqKey2", [ op FundTx 2 "sender1" 10.0 0
                            , op GasPayment 4 "miner1" 12.0 1 ])

    -- Successful, non-coin contract tx
    expected3 = ("ReqKey3", [ op FundTx 5 "sender1" 10.0 0
                            , op GasPayment 7 "miner1" 12.0 1 ])

    -- Successful, coin contract tx
    expected4 = ("ReqKey4", [ op FundTx 8 "sender1" 10.0 0
                            , op TransferOrCreateAcct 9 "sender1" 5.0 1
                            , op GasPayment 10 "miner1" 12.0 2 ])

    -- Unsuccessful tx
    expected5 = ("ReqKey5", [ op FundTx 11 "sender1" 10.0 0
                            , op GasPayment 12 "miner1" 12.0 1])


matchNonGenesisSingleTransactionsToLogs :: Assertion
matchNonGenesisSingleTransactionsToLogs = do
  [rk1, rk5, rk3, rk2, rk4, missingRk] <- pure $ map run targets

  Right rk1' <- pure rk1
  assertEqualEncode "coinbase tx" rk1' expectedRk1

  Right rk5' <- pure rk5
  assertEqualEncode "unsuccessful" rk5' expectedRk5

  Right rk3' <- pure rk3
  assertEqualEncode "successful, non-coin 2" rk3' expectedRk3

  Right rk2' <- pure rk2
  assertEqualEncode "successful, non-coin 1" rk2' expectedRk2

  Right rk4' <- pure rk4
  assertEqualEncode "successful, coin" rk4' expectedRk4

  Left missingRk' <- pure missingRk
  assertEqual "request key not present" missingRk' expectedMissing
  
  where
    run :: T.Text -> Either RosettaFailure (T.Text, [Operation])
    run t = nonGenesisTransaction logs toRk getTxId toTx initial rest (textToRk t)
    
    toTx (_, rk) ops = (rk, ops)
    toRk (_, rk) = textToRk rk
    textToRk = RequestKey . Hash . T.encodeUtf8
    getTxId (tid, _) = tid
    (logs, initial, rest) = mockTxLogs

    targets =
      [ "ReqKey1", "ReqKey5", "ReqKey3", "ReqKey2", "ReqKey4", "RandomReqKey"]

    -- Coinbase Tx
    expectedRk1 = ("ReqKey1", [ op CoinbaseReward 1 "miner1" 2.0 0])
    
    -- Successful, non-coin contract tx
    expectedRk2 = ("ReqKey2", [ op FundTx 2 "sender1" 10.0 0
                            , op GasPayment 4 "miner1" 12.0 1 ])

    -- Successful, non-coin contract tx
    expectedRk3 = ("ReqKey3", [ op FundTx 5 "sender1" 10.0 0
                            , op GasPayment 7 "miner1" 12.0 1 ])

    -- Successful, coin contract tx
    expectedRk4 = ("ReqKey4", [ op FundTx 8 "sender1" 10.0 0
                            , op TransferOrCreateAcct 9 "sender1" 5.0 1
                            , op GasPayment 10 "miner1" 12.0 2 ])

    -- Unsuccessful tx
    expectedRk5 = ("ReqKey5", [ op FundTx 11 "sender1" 10.0 0
                            , op GasPayment 12 "miner1" 12.0 1])
    expectedMissing = RosettaTxIdNotFound
    

--------------------------------------------------------------------------------
-- Utils

type MockCoinbaseTxResult = MockTxResult
type MockTxResult = (Maybe TxId, T.Text)

mockTxLogs :: (Map TxId [AccountLog], MockCoinbaseTxResult, V.Vector MockTxResult)
mockTxLogs = (logs, initial, rest)
  where
    (log1,initial) =
      let key = "miner1"
          amt = 2.0
          g = toJSON (key <> "PublicKey" :: T.Text)
          tid = TxId 1
          l = [(key, amt, g)]
          a = (Just tid, "ReqKey1")
      in ((tid,l), a)

    -- successful, non-coin contract tx
    (logs2,tx1) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 2, TxId 3, TxId 4)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey2")
      in ([fundLogs,gasLogs], a)

    (logs3,tx2) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 5, TxId 6, TxId 7)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey3")
      in ([fundLogs,gasLogs], a)

    -- successful, coin contract tx
    (logs4,tx3) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 8, TxId 9, TxId 10)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          transferLogs = (tid, [(key, 5.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey4")
      in ([fundLogs,transferLogs,gasLogs], a)

    -- unsuccessful tx
    (logs5,tx4) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, gasTid) = (TxId 11, TxId 12)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Nothing, "ReqKey5")
      in ([fundLogs,gasLogs], a)

    rest = V.fromList [tx1, tx2, tx3, tx4]
    logs = M.fromList $ [log1] <> logs2 <> logs3 <> logs4 <> logs5

op :: OperationType -> Word64 -> T.Text -> Decimal -> Word64 -> Operation
op t i key amt oid = operation Successful t (TxId i) (key, amt, gKey) oid
  where
    gKey = toJSON (key <> "PublicKey")

assertEqualEncode
    :: String
    -> (T.Text, [Operation])
    -> (T.Text, [Operation])
    -> Assertion
assertEqualEncode msg e1 e2 =
  assertEqual msg (encode e1) (encode e2)

-- TODO: test for `validateNetwork`

-- TODO: test for `kdaToRosettaAmount`
