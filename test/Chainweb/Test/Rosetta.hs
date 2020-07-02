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
checkBalanceDeltas = do
  case1 >> case2 >> case3 >> case4 >> case5 >> case6

  where
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
                 --------------------------------------------------------
                 , cases 1 [ createCase "sender1" 8.0 (bd $ negate 2.0)]
                 , cases 2 [ createCase "sender1" 9.0 (bd 1.0)
                           , createCase "miner" 13.0 (bd 1.0)]
                 --------------------------------------------------------
                 , cases 3 [ createCase "sender1" 7.0 (bd $ negate 2.0)]
                 , cases 4 [ createCase "sender1" 5.0 (bd $ negate 2.0) -- transfer
                           , createCase "sender2" 12.0 (bd 2.0)] -- transfer
                 , cases 5 [ createCase "sender1" 6.0 (bd 1.0)
                           , createCase "miner" 14.0 (bd 1.0)]
                 --------------------------------------------------------
                 , cases 6 [ createCase "sender2" 10.0 (bd $ negate 2.0)]
                 , cases 7 [ createCase "sender2" 9.5 (bd $ negate 0.5) -- transfer
                           , createCase "sender1" 6.5 (bd 0.5)] -- transfer
                 , cases 8 [ createCase "sender2" 10.5 (bd 1.0)
                           , createCase "miner" 15.0 (bd 1.0)]
                 --------------------------------------------------------
                 , cases 9 [ createCase "sender1" 4.5 (bd $ negate 2.0)]
                 , cases 10 [ createCase "sender1" 5.5 (bd 1.0)
                            , createCase "miner" 16.0 (bd 1.0)]
                 ]
      in test "simulate actual block, previously seen transactions" prevLogs mock


    mockPrevTxs :: [(T.Text, Decimal)] -> Map RowKey AccountRow
    mockPrevTxs txs = M.fromList $ map f txs
      where
        f (key, bal) = (RowKey key, mockAcctRow key bal)

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
          hist = M.fromList $! map justAcctRows cs
          actuals = getBalanceDeltas hist prevBals
          expects = M.fromList $! map justAcctLogs cs
      in assertEqualMap label assertEqualAcctLog expects actuals


matchNonGenesisBlockTransactionsToLogs :: Assertion
matchNonGenesisBlockTransactionsToLogs = do
  testNonGenesisBlock "Match all txs in a non-genesis block" cases
  where
    cases =
      [ MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Coinbase Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey0"
        , _matchRosettaTx_result = TxSuccess (TxId 0)
        , _matchRosettaTx_operations =
          [ mops (TxId 0) [ mop CoinbaseReward ] ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Non-Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey1"
        , _matchRosettaTx_result = TxSuccess (TxId 2)
        , _matchRosettaTx_operations =
          [ mops (TxId 1) [ mop FundTx ]
          , mops (TxId 3) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Another Non-Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey2"
        , _matchRosettaTx_result = TxSuccess (TxId 5)
        , _matchRosettaTx_operations =
          [ mops (TxId 4) [ mop FundTx ]
          , mops (TxId 6) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey3"
        , _matchRosettaTx_result = TxSuccess (TxId 8)
        , _matchRosettaTx_operations =
          [ mops (TxId 7) [ mop FundTx ]
          , mops (TxId 8) [ mop TransferOrCreateAcct
                          , mop TransferOrCreateAcct ]
          , mops (TxId 9) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Failed Tx"
        , _matchRosettaTx_requestKey = "ReqKey4"
        , _matchRosettaTx_result = TxFailure
        , _matchRosettaTx_operations =
          [ mops (TxId 10) [ mop FundTx ]
          , mops (TxId 11) [ mop GasPayment ]
          ]
        }
      ]

matchFailedCoinbaseBlockTransactionsToLogs :: Assertion
matchFailedCoinbaseBlockTransactionsToLogs = do
  testNonGenesisBlock "Match all txs in a non-genesis block when coinbase tx failed" failedCoinbaseCases
  where
    failedCoinbaseCases =
      [ MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Coinbase Tx, Failed"
        , _matchRosettaTx_requestKey = "ReqKey0"
        , _matchRosettaTx_result = TxFailure
        , _matchRosettaTx_operations = []
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Non-Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey1"
        , _matchRosettaTx_result = TxSuccess (TxId 1)
        , _matchRosettaTx_operations =
          [ mops (TxId 0) [ mop FundTx ]
          , mops (TxId 2) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      ]

matchNonGenesisSingleTransactionsToLogs :: Assertion
matchNonGenesisSingleTransactionsToLogs = do
  [rk1, rk0, rk3, rk2, rk4, missingRk] <- pure $ map run targets
  [rk0Exp, rk1Exp, rk2Exp, rk3Exp, rk4Exp] <- pure $ map createExpectedRosettaTx cases

  expectMatch rk0 rk0Exp
  expectMatch rk1 rk1Exp
  expectMatch rk2 rk2Exp
  expectMatch rk3 rk3Exp
  expectMatch rk4 rk4Exp
  expectMissing "request key should not be present" missingRk
  
  where
    run :: T.Text -> Either String (Maybe Transaction)
    run trk = getActual cases f
      where f logs initial rest = nonGenesisTransaction logs initial rest (textToRk trk)

    targets =
      [ "ReqKey1", "ReqKey0", "ReqKey3", "ReqKey2", "ReqKey4", "RandomReqKey"]
    
    expectMatch actual (msg, expect) =
      case actual of
        Left err -> assertFailure $ adjust msg err
        Right Nothing -> assertFailure $ adjust msg
          $ adjust "request key missing. Expected request key "
          $ show $ _transactionId_hash $ _transaction_transactionId expect
        Right (Just actual') -> assertEqualRosettaTx msg (actual', expect)

    expectMissing msg actual =
      case actual of
        (Right Nothing) -> pure ()
        Right (Just _) -> assertFailure $ adjust msg "request key NOT missing"
        Left err -> assertFailure $ adjust msg err

    cases =
      [ MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Coinbase Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey0"
        , _matchRosettaTx_result = TxSuccess (TxId 0)
        , _matchRosettaTx_operations =
          [ mops (TxId 0) [ mop CoinbaseReward ] ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Non-Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey1"
        , _matchRosettaTx_result = TxSuccess (TxId 2)
        , _matchRosettaTx_operations =
          [ mops (TxId 1) [ mop FundTx ]
          , mops (TxId 3) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Another Non-Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey2"
        , _matchRosettaTx_result = TxSuccess (TxId 5)
        , _matchRosettaTx_operations =
          [ mops (TxId 4) [ mop FundTx ]
          , mops (TxId 6) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Coin Tx, Successful"
        , _matchRosettaTx_requestKey = "ReqKey3"
        , _matchRosettaTx_result = TxSuccess (TxId 8)
        , _matchRosettaTx_operations =
          [ mops (TxId 7) [ mop FundTx ]
          , mops (TxId 8) [ mop TransferOrCreateAcct
                          , mop TransferOrCreateAcct ]
          , mops (TxId 9) [ mop GasPayment
                          , mop GasPayment ]
          ]
        }
      , MatchRosettaTx
        { _matchRosettaTx_caseLabel = "Failed Tx"
        , _matchRosettaTx_requestKey = "ReqKey4"
        , _matchRosettaTx_result = TxFailure
        , _matchRosettaTx_operations =
          [ mops (TxId 10) [ mop FundTx ]
          , mops (TxId 11) [ mop GasPayment ]
          ]
        }
      ]


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

newtype MockCommandResult = MockCommandResult (Maybe TxId, T.Text)

instance PendingRosettaTx MockCommandResult where
  getSomeTxId (MockCommandResult (tid,_)) = tid
  getRequestKey (MockCommandResult (_,rk)) = textToRk rk
  makeRosettaTx (MockCommandResult (_,rk)) = mockRosettaTx rk


type MatchFunction tx =
     Map TxId [AccountLog]
  -> CoinbaseTx MockCommandResult
  -> V.Vector MockCommandResult
  -> Either String tx

data TxResultType = TxSuccess TxId | TxFailure

data MatchOperation = MatchOperation
  { _matchOperation_accountLog :: AccountLog
  , _matchOperation_expectedOpType :: OperationType
  }

-- | Helper function that provides a random AccountLog.
--   This is helpful when testing tx-log matching don't
--   care about the actual contents of AccountLog.
mop :: OperationType -> MatchOperation
mop otype = MatchOperation acctLog otype
  where
    key = "someKey" -- dummy variable
    endingBal = 10.0 -- dummy variable
    delta = bd $ negate 1.0 -- dummy variable
    g = mockGuard key
    acctLog = AccountLog key endingBal delta g g

data MatchOperations = MatchOperations
  { _matchOperations_txId :: TxId
  , _matchOperations_operations :: [MatchOperation]
  }

mops :: TxId -> [MatchOperation] -> MatchOperations
mops tid ops = MatchOperations tid ops

data MatchRosettaTx = MatchRosettaTx
  { _matchRosettaTx_caseLabel :: String
  , _matchRosettaTx_requestKey :: T.Text
  , _matchRosettaTx_result :: TxResultType
  , _matchRosettaTx_operations :: [MatchOperations]
  }


createMockCmdResults :: [MatchRosettaTx] -> [MockCommandResult]
createMockCmdResults cases = map f cases
  where
    f (MatchRosettaTx _ rk (TxSuccess tid) _) = MockCommandResult (Just tid, rk)
    f (MatchRosettaTx _ rk TxFailure _) = MockCommandResult (Nothing, rk)

createLogsMap :: [MatchRosettaTx] -> Map TxId [AccountLog]
createLogsMap cases = M.fromList $! concat $! map ((map f) . _matchRosettaTx_operations) cases
  where
    f (MatchOperations tid ops) = (tid, map _matchOperation_accountLog ops)

createOperations :: [MatchOperations] -> [UnindexedOperation]
createOperations opsCases = concat $! map f opsCases
  where
    f (MatchOperations tid ops) = map (createOperation tid) ops

    createOperation tid (MatchOperation acctLog otype) =
      operation Successful otype tid acctLog

createExpectedRosettaTx :: MatchRosettaTx -> (String, Transaction)
createExpectedRosettaTx m = (msg, mockRosettaTx rk ops)
      where
        rk = _matchRosettaTx_requestKey m
        msg = _matchRosettaTx_caseLabel m
        ops = indexedOperations $! createOperations (_matchRosettaTx_operations m)


getActual :: [MatchRosettaTx] -> MatchFunction tx -> Either String tx
getActual cases f =
  case (createMockCmdResults cases) of
    coinbaseResult:restResults -> f logs coinbaseResult (V.fromList $! restResults)
    _ -> Left "Missing coinbase case"
  where
    logs = createLogsMap cases

testNonGenesisBlock :: String -> [MatchRosettaTx] -> Assertion
testNonGenesisBlock msg cases =
  case (getActual cases nonGenesisTransactions) of
    Left err -> assertFailure err
    Right actuals -> do
      assertEqual (adjust msg "list should be same length") (length actuals) (length expects)
      mapM_ f (zip actuals expects)
  where
    expects = map createExpectedRosettaTx cases
    f (actualTx, (txMsg, expectTx)) =
      assertEqualRosettaTx (adjust msg txMsg) (actualTx, expectTx)


mockAcctRow :: T.Text -> Decimal -> AccountRow
mockAcctRow key bal =
  (key, bal, mockGuard key)

mockGuard :: T.Text -> Value
mockGuard key = toJSON (key <> "PublicKey")

bd :: Decimal -> BalanceDelta
bd d = BalanceDelta d

assertEqualAcctLog
    :: String
    -> (AccountLog, AccountLog)
    -> Assertion
assertEqualAcctLog msg (log1, log2) = do
  assertEqual (adjust msg "same key") key1 key2
  assertEqual (adjust msg "same balanceTotal") bal1 bal2
  assertEqual (adjust msg "same balanceDelta") balDelta1 balDelta2
  assertEqual (adjust msg "same currGuard") currGuard1 currGuard2
  assertEqual (adjust msg "same prevGuard") prevGuard1 prevGuard2
  where
    AccountLog key1 bal1 balDelta1 currGuard1 prevGuard1 = log1
    AccountLog key2 bal2 balDelta2 currGuard2 prevGuard2 = log2

assertSameOperation
    :: String
    -> (Operation, Operation)
    -> Assertion
assertSameOperation msg (op1, op2) = do
  assertEqual (adjust msg "same operation id") oid1 oid2
  assertEqual (adjust msg' "same operation metadata") meta1 meta2
  assertEqual (adjust msg' "same related operations") rops1 rops2
  assertEqual (adjust msg' "same operation type") otype1 otype2
  assertEqual (adjust msg' "same operation status") ostatus1 ostatus2
  assertEqual (adjust msg' "same operation account") acct1 acct2
  assertEqual (adjust msg' "same operation amount") amt1 amt2
  where
    msg' = adjust msg $ "operationId=" ++ show (_operationId_index oid1)
    Operation oid1 rops1 otype1 ostatus1 acct1 amt1 meta1 = op1
    Operation oid2 rops2 otype2 ostatus2 acct2 amt2 meta2 = op2

assertEqualRosettaTx
    :: String
    -> (Transaction, Transaction)
    -> Assertion
assertEqualRosettaTx msg (tx1, tx2) = do
  assertEqual (adjust msg "same transactionId") tid1 tid2
  mapM_ (assertSameOperation (adjust msg' "same operations")) (zip ops1 ops2)
  assertEqual (adjust msg' "same metadata") meta1 meta2
  where
    msg' = adjust msg $ "transactionId=" ++ show (_transactionId_hash tid1)
    Transaction tid1 ops1 meta1 = tx1
    Transaction tid2 ops2 meta2 = tx2

assertEqualList
    :: String
    -> (String -> (a,a) -> Assertion)
    -> [a]
    -> [a]
    -> Assertion
assertEqualList msg f li1 li2 = do
  assertEqual (msg ++ ": lists should be the same size") (length li1) (length li2)
  mapM_ (f msg) (zip li1 li2)

assertEqualMap
    :: (Eq a, Ord a, Show a, Eq b, Show b)
    => String
    -> (String -> (b,b) -> Assertion)
    -> Map a [b]
    -> Map a [b]
    -> Assertion
assertEqualMap msg liF m1 m2 = do
  assertEqual (msg ++ ": maps should be the same size") (M.size m1) (M.size m2)
  void $ M.traverseWithKey f m1
  where
    f tid e1 =
      let msg' = (msg ++ ": key=" ++ show tid)
      in case (M.lookup tid m2) of
           Nothing -> assertFailure $ (msg' ++ ": second map didn't have key")
           Just e2 -> assertEqualList msg' liF e1 e2

mockRosettaTx :: T.Text -> [Operation] -> Transaction
mockRosettaTx mrk ops =
  Transaction
  { _transaction_transactionId = TransactionId mrk
  , _transaction_operations = ops
  , _transaction_metadata = Nothing
  }

textToRk :: T.Text -> RequestKey
textToRk = RequestKey . Hash . T.encodeUtf8

adjust :: String -> String -> String
adjust msg a = msg ++ ": " ++ show a
