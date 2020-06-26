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

import Control.Monad (foldM, void)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Decimal
import Data.List (foldl')
import Data.Map (Map)
import Data.Word (Word64)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Pact.Types.Runtime (TxId(..), RowKey(..))
import Pact.Types.Command (RequestKey(..))
import Pact.Types.Hash (Hash(..))

import Rosetta

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Rosetta.Internal
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.Util
import Chainweb.Version

---


tests :: TestTree
tests = testGroup "Chainweb.Test.Rosetta.Server"
  [ testGroup "Unit Tests"
    [ testCase "checkBalanceDeltas" checkBalanceDeltas
    , testCase "matchNonGenesisBlockTransactionsToLogs" matchNonGenesisBlockTransactionsToLogs
    , testCase "matchFailedCoinbaseBlockTransactionsToLogs" matchFailedCoinbaseBlockTransactionsToLogs
    , testCase "matchNonGenesisSingleTransactionsToLogs" matchNonGenesisSingleTransactionsToLogs
    , testCase "checkKDAToRosettaAmount" checkKDAToRosettaAmount
    , testCase "checkValidateNetwork" checkValidateNetwork
    , testCase "checkUniqueRosettaErrorCodes" checkUniqueRosettaErrorCodes
    ]
  ]


checkBalanceDeltas :: Assertion
checkBalanceDeltas =
  case1 >> case2 >> case3 >> case4 >> case5 >> case6
  where
    createCase
        :: T.Text
        -> Decimal
        -> BalanceDelta
        -> (AccountRow, AccountLog)
    createCase key endingBal delta =
      let g = mockGuard key
          acctRow = (key, endingBal, g)
          acctLog = AccountLog key endingBal delta g g
      in (acctRow, acctLog)

    cases
        :: Word64
        -> [(AccountRow, AccountLog)]
        -> (TxId, [(AccountRow, AccountLog)])
    cases i rows = (TxId i, rows)

    test
        :: String
        -> Map RowKey AccountRow
        -> [(TxId, [(AccountRow, AccountLog)])]
        -> Assertion
    test label prevBals cs =
      let justAcctRows (tid, rows) = (tid, map fst rows)
          justAcctLogs (tid, rows) = (tid, map snd rows)
          hist = M.fromList $ map justAcctRows cs
          actuals = getBalanceDeltas hist prevBals
          expects = M.fromList $ map justAcctLogs cs
      in do
        assertEqualMap label expects actuals

    case1 =
      let noPrevLogs = mockPrevTxs []
          unique = [ cases 0 [createCase "k1" 1.0 (bd 1.0)]
                   , cases 1 [createCase "k2" (negate 2.0) (bd $ negate 2.0)]
                   ]
      in test "unique keys, not previously seen" noPrevLogs unique

    case2 =
      let allHavePrevLogs = mockPrevTxs
            [ ("k1", 1.0)
            , ("k2", 2.0)
            , ("k3", 3.0)
            , ("k4", 4.0)]
          unique = [ cases 0 [createCase "k1" 1.0 (bd 0.0)]
                   , cases 1 [createCase "k2" 0.0 (bd $ negate 2.0)]
                   , cases 2 [createCase "k3" 3.5 (bd 0.5)]
                   , cases 3 [createCase "k4" 5.0 (bd 1.0)]
                   ]
      in test "unique keys, all seen before" allHavePrevLogs unique

    case3 =
      let onlyOneSeenPrevLogs = mockPrevTxs [("k1", 1.0)]
          unique = [ cases 0 [createCase "k1" 0.5 (bd $ negate 0.5)]
                   , cases 1 [createCase "k2" 2.0 (bd 2.0)]
                   ]
      in test "unique keys, only one seen before" onlyOneSeenPrevLogs unique

    case4 =
      let noPrevLogs = mockPrevTxs []
          repeated = [ cases 0 [createCase "k1" 1.0 (bd 1.0)]
                     , cases 1 [createCase "k1" 3.0 (bd 2.0)]
                     , cases 2 [createCase "k1" 2.5 (bd $ negate 0.5)]
                     , cases 3 [createCase "k1" 2.5 (bd 0.0)]
                     , cases 4 [createCase "k1" 6.0 (bd 3.5)]
                     ]
      in test "same key, different txs, not previously seen" noPrevLogs repeated

    case5 =
      let prevLogs = mockPrevTxs [("k1", 10.0)]
          repeated = [ cases 0 [ createCase "k1" 9.99 (bd $ negate 0.01)
                               , createCase "k1" 9.0 (bd $ negate 0.99)
                               , createCase "k1" 9.5 (bd 0.5) ]
                     , cases 1 [ createCase "k1" 1.0 (bd $ negate 8.5)
                               , createCase "k1" 5.0 (bd 4.0)
                               , createCase "k1" 4.5 (bd $ negate 0.5)
                               , createCase "k1" 4.55 (bd 0.05) ]
                     , cases 2 [ createCase "k1" 4.0 (bd $ negate 0.55) ]
                     ]
      in test "same key, different and same txs, previously seen" prevLogs repeated

    case6 =
      let prevLogs = mockPrevTxs [("miner", 10.0), ("sender1", 10.0), ("sender2", 10.0)]
          mock = [ cases 0 [createCase "miner" 12.0 (bd 2.0)]
                 , cases 1 [ createCase "sender1" 8.0 (bd $ negate 2.0)
                           , createCase "sender1" 9.0 (bd 1.0)
                           , createCase "miner" 13.0 (bd 1.0)]
                 , cases 2 [ createCase "sender1" 7.0 (bd $ negate 2.0)
                           , createCase "sender1" 5.0 (bd $ negate 2.0) -- transfer
                           , createCase "sender2" 12.0 (bd 2.0) -- transfer
                           , createCase "sender1" 6.0 (bd 1.0)
                           , createCase "miner" 14.0 (bd 1.0)]
                 , cases 3 [ createCase "sender2" 10.0 (bd $ negate 2.0)
                           , createCase "sender2" 9.5 (bd $ negate 0.5) -- transfer
                           , createCase "sender1" 6.5 (bd 0.5) -- transfer
                           , createCase "sender2" 10.5 (bd 1.0)
                           , createCase "miner" 15.0 (bd 1.0)]
                 , cases 4 [ createCase "sender1" 4.5 (bd $ negate 2.0)
                           , createCase "sender1" 5.5 (bd 1.0)
                           , createCase "miner" 16.0 (bd 1.0)]
                 ]
      in test "simulate actual block, previously seen transactions" prevLogs mock


matchNonGenesisBlockTransactionsToLogs :: Assertion
matchNonGenesisBlockTransactionsToLogs = do
  Right [r1, r2, r3, r4, r5] <- pure run
  assertEqualEncode "coinbase tx" r1 expected1
  assertEqualEncode "successful, non-coin 1" r2 expected2
  assertEqualEncode "successful, non-coin 2" r3 expected3
  assertEqualEncode "successful, coin" r4 expected4
  assertEqualEncode "unsuccessful" r5 expected5
  where
    run :: Either String [Transaction]
    run = nonGenesisTransactions logs initial rest

    (logs, initial, rest) = mockTxLogs

    -- Coinbase Tx
    expected1 = mockRosettaTx "ReqKey1" [ op CoinbaseReward 1 "miner1" 2.0 undefined 0]
    
    -- Successful, non-coin contract tx
    expected2 = mockRosettaTx "ReqKey2"
                [ op FundTx 2 "sender1" 10.0 undefined 0
                , op GasPayment 4 "miner1" 12.0 undefined 1 ]

    -- Successful, non-coin contract tx
    expected3 = mockRosettaTx "ReqKey3"
                [ op FundTx 5 "sender1" 10.0 undefined 0
                , op GasPayment 7 "miner1" 12.0 undefined 1 ]

    -- Successful, coin contract tx
    expected4 = mockRosettaTx "ReqKey4"
                [ op FundTx 8 "sender1" 10.0 undefined 0
                , op TransferOrCreateAcct 9 "sender1" 5.0 undefined 1
                , op GasPayment 10 "miner1" 12.0 undefined 2 ]

    -- Unsuccessful tx
    expected5 = mockRosettaTx "ReqKey5"
                [ op FundTx 11 "sender1" 10.0 undefined 0
                , op GasPayment 12 "miner1" 12.0 undefined 1]



matchFailedCoinbaseBlockTransactionsToLogs :: Assertion
matchFailedCoinbaseBlockTransactionsToLogs = do
  initial:rest <- pure $ mockResults txs
  Right [r0, r1] <- pure $ run initial (V.fromList rest)
  assertEqualEncode "coinbase tx" r0 expected0
  assertEqualEncode "successful, non-coin" r1 expected1
  where
    run = nonGenesisTransactions logs

    txs =
      [ PendingCoinbase "miner" 10.0 CoinbaseFailure
      , PendingMockTx "sender" "miner" TxSuccessNonCoin 10.0 10.0 ]

    prevLogs = mockPrevTxs
      [ ("miner", 10.0)
      , ("sender", 10.0)]
    logs = getBalanceDeltas (mockLogs txs) prevLogs

    -- Coinbase Tx
    expected0 = mockRosettaTx (mockRkText 0) []
    
    -- Successful, non-coin contract tx
    expected1 = mockRosettaTx (mockRkText 1)
                [ op FundTx 2 "sender" (10.0 - gasLimit) (negate gasLimit) 0
                , op GasPayment 4 "sender" (10.0 - gasCost) (negate gasCost) 1
                , op GasPayment 4 "miner" (10.0 + gasLimit) gasLimit 1 ]


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

  Right missingRk' <- pure missingRk
  assertEqual "request key not present" missingRk' expectedMissing
  
  where
    run :: T.Text -> Either String (Maybe Transaction)
    run t = nonGenesisTransaction logs initial rest (textToRk t)

    (logs, initial, rest) = mockTxLogs

    targets =
      [ "ReqKey1", "ReqKey5", "ReqKey3", "ReqKey2", "ReqKey4", "RandomReqKey"]

    -- Coinbase Tx
    expectedRk1 = Just $ mockRosettaTx "ReqKey1" [ op CoinbaseReward 1 "miner1" 2.0 undefined 0]
    
    -- Successful, non-coin contract tx
    expectedRk2 = Just $ mockRosettaTx "ReqKey2"
                  [ op FundTx 2 "sender1" 10.0 undefined 0
                  , op GasPayment 4 "miner1" 12.0 undefined 1 ]

    -- Successful, non-coin contract tx
    expectedRk3 = Just $ mockRosettaTx "ReqKey3"
                  [ op FundTx 5 "sender1" 10.0 undefined 0
                  , op GasPayment 7 "miner1" 12.0 undefined 1 ]

    -- Successful, coin contract tx
    expectedRk4 = Just $ mockRosettaTx "ReqKey4"
                  [ op FundTx 8 "sender1" 10.0 undefined 0
                  , op TransferOrCreateAcct 9 "sender1" 5.0 undefined 1
                  , op GasPayment 10 "miner1" 12.0 undefined 2 ]

    -- Unsuccessful tx
    expectedRk5 = Just $ mockRosettaTx "ReqKey5"
                  [ op FundTx 11 "sender1" 10.0 undefined 0
                  , op GasPayment 12 "miner1" 12.0 undefined 1]

    expectedMissing = Nothing --RosettaTxIdNotFound



checkKDAToRosettaAmount :: Assertion
checkKDAToRosettaAmount = do
  assertEqual "normal: 123.0"
    (rosettaAmount normalStandard) normalAtomic
  assertEqual "min precision: 0.123456789123"
    (rosettaAmount smallStandard) smallAtomic
  assertEqual "big with min precision: 123456789123.123456789123"
    (rosettaAmount bigStandard) bigAtomic
  assertEqual "smaller than min precision: 0.123456789123456789"
    (rosettaAmount reallySmallStandard) reallySmallAtomic
  assertEqual "really big with min precision: 123456789123456789.123456789123"
    (rosettaAmount reallyBigStandard) reallyBigAtomic
  where
    rosettaAmount = _amount_value . kdaToRosettaAmount

    (normalStandard, normalAtomic) =
      (123.0, "123000000000000")
    (smallStandard, smallAtomic) =
      (0.123456789123, "123456789123")
    (bigStandard, bigAtomic) =
      (123456789123.123456789123, "123456789123123456789123")
    (reallySmallStandard, reallySmallAtomic) =
      (0.123456789123456789, "123456789123") -- smaller than min precision so will drop extras
    (reallyBigStandard, reallyBigAtomic) =
      (123456789123456789.123456789123, "123456789123456789123456789123")

checkValidateNetwork :: Assertion
checkValidateNetwork = do
  assertEqual "valid network id"
    (run validNetId) (Right "0")
  assertEqual "invalid blockchain name"
    (run invalidBlockchainName) (Left RosettaInvalidBlockchainName)
  assertEqual "mismatched network name"
    (run mismatchedNetName) (Left RosettaMismatchNetworkName)
  assertEqual "chain id unspecified"
    (run chainIdUnspecified) (Left RosettaChainUnspecified)
  assertEqual "invalid chain id"
    (run invalidChainId) (Left RosettaInvalidChain)
  where
    run :: (ChainwebVersion, NetworkId) -> Either RosettaFailure T.Text
    run (v,net) = runExceptT (validateNetwork v net) >>= either Left (pure . chainIdToText)
    
    validNetId = (Development, NetworkId
      { _networkId_blockchain = "kadena"
      , _networkId_network = "development"
      , _networkId_subNetworkId = Just $ SubNetworkId "0" Nothing
      })
    invalidBlockchainName =
      (fst validNetId,
       (snd validNetId) { _networkId_blockchain = "incorrectName" }
      )
    mismatchedNetName = (Testnet04, snd validNetId)
    chainIdUnspecified =
      (fst validNetId,
       (snd validNetId) { _networkId_subNetworkId = Nothing }
      )
    invalidChainId =
      (fst validNetId,
       (snd validNetId) { _networkId_subNetworkId = Just $ SubNetworkId "1000" Nothing }
      )

checkUniqueRosettaErrorCodes :: Assertion
checkUniqueRosettaErrorCodes = case repeated of
  Left x -> assertFailure $ "Found a repeated Rosetta Code: " ++ show x
  Right _ -> pure ()
  where
    repeated = foldM g S.empty errCodes
    g acc x =
      if (S.member x acc)
      then (Left x)
      else (Right $ S.insert x acc)
    errCodes = map (_error_code . rosettaError) [minBound .. maxBound]

--------------------------------------------------------------------------------
-- Utils

data TxResultType =
  TxSuccessNonCoin | TxSuccessCoin Decimal (T.Text, Decimal) | TxFailure

data CoinbaseResultType = CoinbaseFailure | CoinbaseSuccess

data PendingMockTx =
    PendingMockTx
    { _pmt_senderAccount :: T.Text
    , _pmt_minerAccount :: T.Text
    , _pmt_txType :: TxResultType
    , _pmt_prevSenderBalance :: Decimal
    , _pmt_prevMinerBalance :: Decimal
    }
  | PendingCoinbase
    { _pc_minerAccount :: T.Text
    , _pc_prevMinerBalance :: Decimal
    , _pc_isSuccess :: CoinbaseResultType
    }

newtype MockTxResult = MockTxResult (Maybe TxId, T.Text)

instance PendingTx MockTxResult where
  getSomeTxId (MockTxResult (tid,_)) = tid
  getRequestKey (MockTxResult (_,rk)) = textToRk rk
  makeRosettaTx (MockTxResult (_,rk)) = mockRosettaTx rk

mockTxLogs :: (Map TxId [AccountLog], MockTxResult, V.Vector MockTxResult)
mockTxLogs = undefined -- (logs, initial, rest) -- TODO
  {--where
    (log1,initial) =
      let key = "miner1"
          amt = 2.0
          delta = BalanceDelta amt
          g = toJSON (key <> "PublicKey" :: T.Text)
          tid = TxId 1
          l = [ AccountLog key amt delta g ]
          a = (Just tid, "ReqKey1")
      in ((tid,l), MockTxResult a)

    -- successful, non-coin contract tx
    (logs2,tx1) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 2, TxId 3, TxId 4)
          fundLogs = (fundTid,
                      [ AccountLog key 10.0 -5.0 gKey ])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey2")
      in ([fundLogs,gasLogs], MockTxResult a)

    (logs3,tx2) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 5, TxId 6, TxId 7)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey3")
      in ([fundLogs,gasLogs], MockTxResult a)

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
      in ([fundLogs,transferLogs,gasLogs], MockTxResult a)

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
      in ([fundLogs,gasLogs], MockTxResult a)

    rest = V.fromList [tx1, tx2, tx3, tx4]
    logs = M.fromList $ [log1] <> logs2 <> logs3 <> logs4 <> logs5
--}

mockPrevTxs :: [(T.Text, Decimal)] -> Map RowKey AccountRow
mockPrevTxs txs = M.fromList $ map f txs
  where
    f (key, bal) = (RowKey key, mockAcctRow key bal)

mockResults :: [PendingMockTx] -> [MockTxResult]
mockResults txs = reverse $ snd $ foldl' f ((-1 :: Int, TxId 0), []) txs
  where
    f ((i, lastTxIdSeen), li) tx = case tx of
      PendingCoinbase _ _ isSuccess -> case isSuccess of
        CoinbaseSuccess ->
          let tid = succ lastTxIdSeen
              idx = succ i
              res = MockTxResult (Just tid, mockRkText idx)
          in ((idx, tid), res:li)
        CoinbaseFailure ->
          let idx = succ i
              res = MockTxResult (Nothing, mockRkText idx)
          in ((idx, lastTxIdSeen), res:li)
      PendingMockTx _ _ typ _ _ -> case typ of
        TxFailure ->
          let fundTid = succ lastTxIdSeen
              gasTid = succ fundTid
              idx = succ i
              res = MockTxResult (Nothing, mockRkText idx)
          in ((idx, gasTid), res:li)
        TxSuccessNonCoin ->
          let fundTid = succ lastTxIdSeen
              tid = succ fundTid
              gasTid = succ tid
              idx = succ i
              res = MockTxResult (Just tid, mockRkText idx)
          in ((idx, gasTid), res:li)
        TxSuccessCoin _ _ ->
          let fundTid = succ lastTxIdSeen
              tid = succ fundTid
              gasTid = succ tid
              idx = succ i
              res = MockTxResult (Just tid, mockRkText idx)
          in ((idx, gasTid), res:li)


mockLogs :: [PendingMockTx] -> Map TxId [AccountRow]
mockLogs txs = M.fromList $ reverse $ snd $ foldl' f (TxId 0, []) txs
  where
    f (lastTxIdSeen, li) tx = case tx of
      PendingCoinbase miner bal isSuccess -> case isSuccess of
        CoinbaseSuccess ->
          let tid = succ lastTxIdSeen
              logs = (tid, [mockAcctRow miner bal])  -- assumes no miner reward
          in (tid, logs:li)
        CoinbaseFailure -> (lastTxIdSeen, li) -- no logs added
      PendingMockTx sender miner typ senderBal minerBal -> case typ of
        TxFailure ->
          let fundTid = succ lastTxIdSeen
              gasTid = succ fundTid
              fundLogs = (fundTid, [mockAcctRow sender (senderBal - gasLimit)])
              gasLogs = (gasTid, [mockAcctRow miner (minerBal + gasLimit)])
          in (gasTid, gasLogs:fundLogs:li)
        TxSuccessNonCoin ->
          let fundTid = succ lastTxIdSeen
              gasTid = succ (succ fundTid)
              fundLogs = (fundTid, [mockAcctRow sender (senderBal - gasLimit)])
              gasLogs = (gasTid, [ mockAcctRow sender (senderBal - gasLimit + gasCost)
                                 , mockAcctRow miner (minerBal + gasCost)])
          in (gasTid, gasLogs:fundLogs:li)
        TxSuccessCoin amt (toAcct, toBal) ->
          let fundTid = succ lastTxIdSeen
              transferTid = succ fundTid
              gasTid = succ transferTid
              fundLogs = (fundTid, [mockAcctRow sender (senderBal - gasLimit)])
              transferLogs = (transferTid, [ mockAcctRow sender (senderBal - gasLimit - amt)
                                           , mockAcctRow toAcct (toBal + amt)])
              gasLogs = (gasTid, [ mockAcctRow sender (senderBal - gasLimit - amt + gasCost)
                                 , mockAcctRow miner (minerBal + gasCost)])
          in (gasTid, gasLogs:transferLogs:fundLogs:li)

gasLimit :: Decimal
gasLimit = 0.005

gasCost :: Decimal
gasCost = 0.5 * gasLimit

mockAcctRow :: T.Text -> Decimal -> AccountRow
mockAcctRow key bal =
  (key, bal, mockGuard key)


mockRkText :: Int -> T.Text
mockRkText i = "RequestKey" <> (T.pack $ show i)

mockGuard :: T.Text -> Value
mockGuard key = toJSON (key <> "PublicKey")

bd :: Decimal -> BalanceDelta
bd d = BalanceDelta d

op :: OperationType -> Word64 -> T.Text -> Decimal -> Decimal -> Word64 -> Operation
op t i key amt delta oid = operation Successful t (TxId i) acctLog oid
  where
    gKey = mockGuard key
    acctLog = AccountLog key amt (BalanceDelta delta) gKey gKey

assertEqualEncode
    :: (ToJSON a)
    => String
    -> a
    -> a
    -> Assertion
assertEqualEncode msg e1 e2 =
  assertEqual msg (encode e1) (encode e2)

assertEqualMap
    :: (Eq a, Ord a, Show a, Eq b, Show b)
    => String
    -> Map a [b]
    -> Map a [b]
    -> Assertion
assertEqualMap msg m1 m2 = do
  assertEqual (msg ++ ": maps should be the same size") (M.size m1) (M.size m2)
  void $ M.traverseWithKey f m1
  where
    f tid e1 =
      case (M.lookup tid m2) of
        Nothing -> assertFailure $ (msg ++ ": second map didn't have key txId=" ++ show tid)
        Just e2 -> assertEqualElems tid e1 e2

    assertEqualElems tid li1 li2 = do
      assertEqual (msg ++ ": elem list should be same size") (length li1) (length li2)
      mapM (assertEqualElem tid) (zip li1 li2)

    assertEqualElem tid (e1, e2) =
      assertEqual (msg ++ ": txId=" ++ show tid) e1 e2

mockRosettaTx :: T.Text -> [Operation] -> Transaction
mockRosettaTx mrk ops =
  Transaction
  { _transaction_transactionId = TransactionId mrk
  , _transaction_operations = ops
  , _transaction_metadata = Nothing
  }

textToRk :: T.Text -> RequestKey
textToRk = RequestKey . Hash . T.encodeUtf8
