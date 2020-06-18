<<<<<<< HEAD
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Rosetta
( tests
) where


import Control.Lens
import Control.Monad.Catch
import Control.Retry

import GHC.Natural

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.Graph
import Chainweb.Rosetta.RestAPI.Client
import Chainweb.Test.Utils
import Chainweb.Time (Time(..), Micros(..))
import Chainweb.Version

import Data.CAS.RocksDB

import Rosetta

-- -------------------------------------------------------------------------- --
-- Global Settings


debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

testRetryPolicy :: RetryPolicy
testRetryPolicy = stepped <> limitRetries 150
  where
    stepped = retryPolicy $ \rs -> case rsIterNumber rs of
      0 -> Just 20_000
      1 -> Just 50_000
      2 -> Just 100_000
      _ -> Just 250_000

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

nodes:: Natural
nodes = 1

type RosettaTest = IO (Time Micros) -> IO ChainwebNetwork -> TestTree

data RosettaTestException
    = AccountBalanceFailure String
    | BlockTransactionFailure String
    | BlockFailure String
    | ConstructionMetadataFailure String
    | ConstructionSubmitFailure String
    | MempoolTransactionFailure String
    | MempoolFailure String
    | NetworkListFailure String
    | NetworkOptionsFailure String
    | NetworkStatusFailure String
    deriving Show

instance Exception RosettaTestException


nid :: NetworkId
nid = NetworkId
    { _networkId_blockchain = undefined
    , _networkId_network = "fastTimedCPM"
    , _networkId_subNetworkId = undefined
    }

aid :: AccountId
aid = AccountId
    { _accountId_address = "sender00"
    , _accountId_subAccount = Nothing
    , _accountId_metadata = undefined
    }

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "Chainweb.Test.Rosetta" go
  where
    go = return $
      withNodes v "rosettaRemoteTests-" rdb nodes $ \nio ->
      withTime $ \tio -> tgroup nio tio

    tgroup nio tio
      = testGroup "Rosetta API tests"
      $ fmap (\test -> test tio nio) tests_

    tests_ =
      [ accountBalanceTests
      , blockTransactionTests
      , blockTests
      , constructionMetadataTests
      , constructionSubmitTests
      , mempoolTransactionTests
      , networkListTests
      , networkOptionsTests
      , networkStatusTests
      ]


accountBalanceTests :: RosettaTest
accountBalanceTests _tio _nio = testCaseSteps "Account Balance Lookup" $ \step -> do
    ccenv <- _runClientEnv <$> _nio
    return ()
  where
    req = AccountBalanceReq nid aid Nothing

blockTransactionTests :: RosettaTest
blockTransactionTests _tio _nio = undefined

blockTests :: RosettaTest
blockTests _tio _nio = undefined

constructionMetadataTests :: RosettaTest
constructionMetadataTests _tio _nio = undefined

constructionSubmitTests :: RosettaTest
constructionSubmitTests _tio _nio = undefined

mempoolTransactionTests :: RosettaTest
mempoolTransactionTests _tio _nio = undefined

networkListTests :: RosettaTest
networkListTests _tio _nio = undefined

networkOptionsTests :: RosettaTest
networkOptionsTests _tio _nio = undefined

networkStatusTests :: RosettaTest
networkStatusTests _tio _nio = undefined

-- ------------------------------------------------------------------ --
-- Rosetta api w/ retry

accountBalance
    :: ClientEnv
    -> AccountBalanceReq
    -> IO AccountBalanceResp
accountBalance cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting account balance for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaAccountBalanceApiClient v req) cenv >>= \case
      Left e -> throwM $ AccountBalanceFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      AccountBalanceFailure _ -> return True
      _ -> return False

blockTransaction
    :: ClientEnv
    -> BlockTransactionReq
    -> IO BlockTransactionResp
blockTransaction cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block transaction for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockTransactionFailure _ -> return True
      _ -> return False

block
    :: ClientEnv
    -> BlockReq
    -> IO BlockResp
block cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting block for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaBlockApiClient v req) cenv >>= \case
      Left e -> throwM $ BlockFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      BlockFailure _ -> return True
      _ -> return False

constructionMetadata
    :: ClientEnv
    -> ConstructionMetadataReq
    -> IO ConstructionMetadataResp
constructionMetadata cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction metadata for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionMetadataApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionMetadataFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionMetadataFailure _ -> return True
      _ -> return False

constructionSubmit
    :: ClientEnv
    -> ConstructionSubmitReq
    -> IO ConstructionSubmitResp
constructionSubmit cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting construction submit for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaConstructionSubmitApiClient v req) cenv >>= \case
      Left e -> throwM $ ConstructionSubmitFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      ConstructionSubmitFailure _ -> return True
      _ -> return False

mempoolTransaction
    :: ClientEnv
    -> MempoolTransactionReq
    -> IO MempoolTransactionResp
mempoolTransaction cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool transaction for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolTransactionApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolTransactionFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolTransactionFailure _ -> return True
      _ -> return False

mempool
    :: ClientEnv
    -> MempoolReq
    -> IO MempoolResp
mempool cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting mempool for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaMempoolApiClient v req) cenv >>= \case
      Left e -> throwM $ MempoolFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolFailure _ -> return True
      _ -> return False

networkList
    :: ClientEnv
    -> MetadataReq
    -> IO NetworkListResp
networkList cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network list for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkListApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkListFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkListFailure _ -> return True
      _ -> return False

networkOptions
    :: ClientEnv
    -> NetworkReq
    -> IO NetworkOptionsResp
networkOptions cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network options for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkOptionsApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkOptionsFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkOptionsFailure _ -> return True
      _ -> return False

networkStatus
    :: ClientEnv
    -> NetworkReq
    -> IO NetworkStatusResp
networkStatus cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network status for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkStatusApiClient v req) cenv >>= \case
      Left e -> throwM $ NetworkStatusFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkStatusFailure _ -> return True
      _ -> return False
||||||| merged common ancestors
=======
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

import Control.Monad (foldM)
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

import Pact.Types.Runtime (TxId(..))
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
    [ testCase "matchNonGenesisBlockTransactionsToLogs" matchNonGenesisBlockTransactionsToLogs
    , testCase "matchFailedCoinbaseBlockTransactionsToLogs" matchFailedCoinbaseBlockTransactionsToLogs
    , testCase "matchNonGenesisSingleTransactionsToLogs" matchNonGenesisSingleTransactionsToLogs
    , testCase "checkKDAToRosettaAmount" checkKDAToRosettaAmount
    , testCase "checkValidateNetwork" checkValidateNetwork
    , testCase "checkUniqueRosettaErrorCodes" checkUniqueRosettaErrorCodes
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
    run :: Either String [Transaction]
    run = nonGenesisTransactions logs initial rest

    (logs, initial, rest) = mockTxLogs

    -- Coinbase Tx
    expected1 = mockRosettaTx "ReqKey1" [ op CoinbaseReward 1 "miner1" 2.0 0]
    
    -- Successful, non-coin contract tx
    expected2 = mockRosettaTx "ReqKey2"
                [ op FundTx 2 "sender1" 10.0 0
                , op GasPayment 4 "miner1" 12.0 1 ]

    -- Successful, non-coin contract tx
    expected3 = mockRosettaTx "ReqKey3"
                [ op FundTx 5 "sender1" 10.0 0
                , op GasPayment 7 "miner1" 12.0 1 ]

    -- Successful, coin contract tx
    expected4 = mockRosettaTx "ReqKey4"
                [ op FundTx 8 "sender1" 10.0 0
                , op TransferOrCreateAcct 9 "sender1" 5.0 1
                , op GasPayment 10 "miner1" 12.0 2 ]

    -- Unsuccessful tx
    expected5 = mockRosettaTx "ReqKey5"
                [ op FundTx 11 "sender1" 10.0 0
                , op GasPayment 12 "miner1" 12.0 1]

matchFailedCoinbaseBlockTransactionsToLogs :: Assertion
matchFailedCoinbaseBlockTransactionsToLogs = do
  Right [r1, r2] <- pure run
  assertEqualEncode "coinbase tx" r1 expected1
  assertEqualEncode "successful, non-coin 1" r2 expected2
  where
    run :: Either String [Transaction]
    run = nonGenesisTransactions logs initial rest

    (logs, initial, rest) = mockTxLogsFailedCoinbaseAndTx

    -- Coinbase Tx
    expected1 = mockRosettaTx "ReqKey1" []
    
    -- Successful, non-coin contract tx
    expected2 = mockRosettaTx "ReqKey2"
                [ op FundTx 2 "sender1" 10.0 0
                , op GasPayment 4 "miner1" 12.0 1 ]


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
    expectedRk1 = Just $ mockRosettaTx "ReqKey1" [ op CoinbaseReward 1 "miner1" 2.0 0]
    
    -- Successful, non-coin contract tx
    expectedRk2 = Just $ mockRosettaTx "ReqKey2"
                  [ op FundTx 2 "sender1" 10.0 0
                  , op GasPayment 4 "miner1" 12.0 1 ]

    -- Successful, non-coin contract tx
    expectedRk3 = Just $ mockRosettaTx "ReqKey3"
                  [ op FundTx 5 "sender1" 10.0 0
                  , op GasPayment 7 "miner1" 12.0 1 ]

    -- Successful, coin contract tx
    expectedRk4 = Just $ mockRosettaTx "ReqKey4"
                  [ op FundTx 8 "sender1" 10.0 0
                  , op TransferOrCreateAcct 9 "sender1" 5.0 1
                  , op GasPayment 10 "miner1" 12.0 2 ]

    -- Unsuccessful tx
    expectedRk5 = Just $ mockRosettaTx "ReqKey5"
                  [ op FundTx 11 "sender1" 10.0 0
                  , op GasPayment 12 "miner1" 12.0 1]

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

newtype MockTxResult = MockTxResult (Maybe TxId, T.Text)

instance PendingTx MockTxResult where
  getSomeTxId (MockTxResult (tid,_)) = tid
  getRequestKey (MockTxResult (_,rk)) = textToRk rk
  makeRosettaTx (MockTxResult (_,rk)) = mockRosettaTx rk

mockTxLogs :: (Map TxId [AccountLog], MockTxResult, V.Vector MockTxResult)
mockTxLogs = (logs, initial, rest)
  where
    (log1,initial) =
      let key = "miner1"
          amt = 2.0
          g = toJSON (key <> "PublicKey" :: T.Text)
          tid = TxId 1
          l = [(key, amt, g)]
          a = (Just tid, "ReqKey1")
      in ((tid,l), MockTxResult a)

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


mockTxLogsFailedCoinbaseAndTx :: (Map TxId [AccountLog], MockTxResult, V.Vector MockTxResult)
mockTxLogsFailedCoinbaseAndTx = (logs, initial, rest)
  where
    initial = MockTxResult (Nothing, "ReqKey1")

    -- successful, non-coin contract tx
    (logs2,tx1) =
      let minerKey = "miner1"
          key = "sender1"
          gMiner = toJSON (minerKey <> "PublicKey" :: T.Text)
          gKey = toJSON (key <> "PublicKey" :: T.Text)
          (fundTid, tid, gasTid) = (TxId 1, TxId 2, TxId 3)
          fundLogs = (fundTid, [(key, 10.0, gKey)])
          gasLogs = (gasTid, [(minerKey, 12.0, gMiner)])
          a = (Just tid, "ReqKey2")
      in ([fundLogs,gasLogs], MockTxResult a)

    rest = V.fromList [tx1]
    logs = M.fromList logs2

op :: OperationType -> Word64 -> T.Text -> Decimal -> Word64 -> Operation
op t i key amt oid = operation Successful t (TxId i) (key, amt, gKey) oid
  where
    gKey = toJSON (key <> "PublicKey")

assertEqualEncode
    :: (ToJSON a)
    => String
    -> a
    -> a
    -> Assertion
assertEqualEncode msg e1 e2 =
  assertEqual msg (encode e1) (encode e2)

mockRosettaTx :: T.Text -> [Operation] -> Transaction
mockRosettaTx mrk ops =
  Transaction
  { _transaction_transactionId = TransactionId mrk
  , _transaction_operations = ops
  , _transaction_metadata = Nothing
  }

textToRk :: T.Text -> RequestKey
textToRk = RequestKey . Hash . T.encodeUtf8
>>>>>>> master
