{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Chainweb.Test.Rosetta.RestAPI
( tests
) where


import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens

import qualified Data.Aeson as A
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (union)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import Data.Foldable

import GHC.Natural

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.API
import Pact.Types.Command

-- internal chainweb modules

import Chainweb.Graph
import Chainweb.Pact.Utils (aeson)
import Chainweb.Rosetta.RestAPI
import Chainweb.Rosetta.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Test.RestAPI.Utils
import Chainweb.Test.Utils
import Chainweb.Time (Time(..), Micros(..))
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import Rosetta

import System.IO.Unsafe (unsafePerformIO)

-- -------------------------------------------------------------------------- --
-- Global Settings

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

nodes:: Natural
nodes = 1

cid :: ChainId
cid = unsafeChainId 0

cids :: [Text]
cids = chainIds v ^.. folded . to (sshow @Int . chainIdInt)

nonceRef :: IORef Natural
nonceRef = unsafePerformIO $ newIORef 0

type RosettaTest = IO (Time Micros) -> IO ClientEnv -> TestTree

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "Chainweb.Test.Rosetta.RestAPI" go
  where
    go = return $
      withNodes v "rosettaRemoteTests-" rdb nodes $ \envIo ->
      withTime $ \tio -> testGroup "Rosetta API tests" (tgroup tio envIo)

    tgroup tio envIo = fmap (\test -> test tio envIo)
      [ accountBalanceTests
      , blockTransactionTests
      , blockTests
      , constructionSubmitTests
      -- , mempoolTransactionTests
      , mempoolTests
      , networkListTests
      , networkOptionsTests
      , networkStatusTests
      ]

-- | Rosetta account balance endpoint tests
--
accountBalanceTests :: RosettaTest
accountBalanceTests tio envIo = testCaseSteps "Account Balance Lookup" $ \step -> do
    step "check initial balance"
    cenv <- envIo
    resp0 <- accountBalance cenv req
    checkBalance resp0 100000000.000

    step "send 1.0 tokens to sender00 from sender01"
    transferOneAsync_ tio cenv (void . return)

    step "check post-transfer balance"
    resp1 <- accountBalance cenv req
    checkBalance resp1 99999997.8906
  where
    req = AccountBalanceReq nid aid Nothing

    checkBalance resp bal1 = do
      let b0 = head $ _accountBalanceResp_balances resp
          b1 = kdaToRosettaAmount bal1
          curr = _amount_currency b0

      b1 @=? b0
      curr @=? kda

-- | Rosetta block transaction endpoint tests
--
blockTransactionTests :: RosettaTest
blockTransactionTests tio envIo = after AllSucceed "Account Balance" $
    testCaseSteps "Block Transaction Tests" $ \step -> do
      cenv <- envIo
      rkmv <- newEmptyMVar @RequestKeys

      step "send 1.0 from sender00 to sender01 and extract block tx request"
      prs <- transferOneAsync tio cenv (putMVar rkmv)
      req <- mkTxReq rkmv prs

      step "send in block tx request"
      resp <- blockTransaction cenv req

      (fundtx,cred,deb,redeem,reward) <-
        case _transaction_operations $ _blockTransactionResp_transaction resp of
          [a,b,c,d,e] -> return (a,b,c,d,e)
          _ -> assertFailure "every transfer should result in 5 transactions"

      step "validate initial gas buy at index 0"
      validateOp 0 "99999996890600000000" "FundTx" fundtx

      step "validate sender01 credit at index 1"
      validateOp 1 "110000003000000000000" "TransferOrCreateAcct" cred

      step "validate sender00 debit at index 2"
      validateOp 2 "99999995890600000000" "TransferOrCreateAcct" deb

      step "validate sender00 gas redemption at index 3"
      validateOp 3 "99999996835900000000" "GasPayment" redeem

      step "validate miner gas reward at index 4"
      validateOp 4 "7077669000000" "GasPayment" reward

  where
    mkTxReq rkmv prs = do
      rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
      hm <- extractMetadata rk prs
      bh <- hm ^?! ix "blockHeight" . to readAeson
      bhash <- hm ^?! ix "blockHash" . to readAeson

      let bid = BlockId bh bhash
          tid = rkToTransactionId rk

      return $ BlockTransactionReq nid bid tid

    validateOp idx amount opType o = do
      _operation_operationId o @?= OperationId idx Nothing
      _operation_type o @?= opType
      _operation_status o @?= "Successful"
      _operation_amount o @?= Just (Amount amount kda Nothing)


-- | Rosetta block endpoint tests
--
blockTests :: RosettaTest
blockTests tio envIo = after AllSucceed "Block Transaction Tests" $
    testCaseSteps "Block Tests" $ \step -> do
      cenv <- envIo
      rkmv <- newEmptyMVar @RequestKeys

      step "fetch genesis block"
      resp0 <- _block_blockId . _blockResp_block <$> block cenv (req 0)
      resp0 @=? genesisId

      step "send transaction"
      prs <- transferOneAsync tio cenv (putMVar rkmv)
      rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
      cmdMeta <- extractMetadata rk prs
      bh <- cmdMeta ^?! ix "blockHeight" . to readAeson

      step "check tx at block height matches sent tx"
      resp1 <- block cenv (req bh)

      step "validate remediations at block height 1"
      remResp <- block cenv (req 1)
      -- check remediation tx id's
      return ()
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

-- | Rosetta construction submit endpoint tests (i.e. tx submission directly to mempool)
--
constructionSubmitTests :: RosettaTest
constructionSubmitTests tio envIo = after AllSucceed "Block Tests" $
    testCaseSteps "Construction Submit Tests" $ \step -> return ()

-- -- | Rosetta mempool transaction endpoint tests
-- --
-- -- (PENDING: the only way to test this is to DOS the mempool)
-- --
-- mempoolTransactionTests :: RosettaTest
-- mempoolTransactionTests tio envIo =
--     testCaseSteps "Mempool Transaction Tests" $ \step -> do
--       cenv <- envIo
--       rkmv <- newEmptyMVar @RequestKeys

--       step "execute transfer and wait on mempool data"
--       void $! async $ transferOneAsync_ tio cenv (putMVar rkmv)

--       step "wait until mempool registers transfer"
--       rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
--       MempoolTransactionResp tx _meta <- mempoolTransactionWithFastRetry cenv (req rk)

--       step "compare requestkey against transaction id"
--       rkToTransactionId rk @=? _transaction_transactionId tx
--   where
--     req rk = MempoolTransactionReq nid (rkToTransactionId rk)

-- | Rosetta mempool endpoint tests
--
mempoolTests :: RosettaTest
mempoolTests tio envIo = after AllSucceed "Construction Submit Tests" $
    testCaseSteps "Mempool Tests" $ \step -> do
      cenv <- envIo
      rkmv <- newEmptyMVar @RequestKeys

      step "execute transfer and wait on mempool data"
      void $! async $ transferOneAsync_ tio cenv (putMVar rkmv)
      rk NEL.:| [] <- _rkRequestKeys <$> takeMVar rkmv

      let tid = rkToTransactionId rk
      let test (MempoolResp ts) = return $ elem tid ts

      step "compare requestkey against mempool responses"
      void $! repeatUntil test $ mempool cenv req
  where
    req = MempoolReq nid

-- | Rosetta network list endpoint tests
--
networkListTests :: RosettaTest
networkListTests _ envIo = testCaseSteps "Network List Tests" $ \step -> do
    cenv <- envIo

    step "send network list request"
    resp <- networkList cenv req

    for_ (_networkListResp_networkIds resp) $ \n -> do
       _networkId_blockchain n @=? "kadena"
       _networkId_network n @=? "fastTimedCPM-peterson"
       assertBool "chain id of subnetwork is valid"
         $ maybe False (\a -> elem (_subNetworkId_network a) cids)
         $ _networkId_subNetworkId n
  where
    req = MetadataReq Nothing

-- | Rosetta network options tests
--
networkOptionsTests :: RosettaTest
networkOptionsTests _ envIo = testCaseSteps "Network Options Tests" $ \step -> do
    cenv <- envIo

    step "send network options request"
    resp <- networkOptions cenv req0

    let allow = _networkOptionsResp_allow resp
        version = _networkOptionsResp_version resp

    step "check options responses against allowable data and versions"
    version  @=? rosettaVersion

    step "Check that response errors are a subset of valid errors"
    (respErrors resp `subset` rosettaFailures) @?
      "allowable errors must coincide with failure list"

    step "Check that response statuses are a subset of valid statuses"
    (_allow_operationStatuses allow `union` operationStatuses) `subset` operationStatuses @?
      "allowed operation statuses coincide"

    step "Check that response op types are a subset of op types"
    (_allow_operationTypes allow `subset` allowedOperations) @?
      "allowed operations coincide"

  where
    req0 = NetworkReq nid Nothing
    respErrors = _allow_errors . _networkOptionsResp_allow
    allowedOperations = _operationStatus_status <$> operationStatuses

-- | Rosetta network status tests
--
networkStatusTests :: RosettaTest
networkStatusTests tio envIo = testCaseSteps "Network Status Tests" $ \step -> do
    cenv <- envIo

    step "send network status request"
    resp0 <- networkStatus cenv req

    step "check status response against genesis"
    genesisId @=? _networkStatusResp_genesisBlockId resp0

    step "send in a transaction and update current block"
    transferOneAsync_ tio cenv (void . return)
    resp1 <- networkStatus cenv req

    step "check status response genesis and block height"
    genesisId @=? _networkStatusResp_genesisBlockId resp1
    (blockIdOf resp1 > blockIdOf resp0) @? "current block id heights must increment"
  where
    req = NetworkReq nid Nothing

    blockIdOf = _blockId_index . _networkStatusResp_currentBlockId

-- ------------------------------------------------------------------ --
-- Test Data

kda :: Currency
kda = Currency "KDA" 12 Nothing

snid :: SubNetworkId
snid = SubNetworkId
    { _subNetworkId_metadata = Nothing
    , _subNetworkId_network = "0"
    }

nid :: NetworkId
nid = NetworkId
    { _networkId_blockchain = "kadena"
    , _networkId_network = "fastTimedCPM-peterson"
    , _networkId_subNetworkId = Just snid
    }

aid :: AccountId
aid = AccountId
    { _accountId_address = "sender00"
    , _accountId_subAccount = Nothing
    , _accountId_metadata = Nothing
    }

genesisId :: BlockId
genesisId = BlockId 0 "d69wD5SUpshDI6rbmQGugDXTd1-riqr7gfg5ZjvUrqk"

rosettaVersion :: RosettaNodeVersion
rosettaVersion = RosettaNodeVersion
    { _version_rosettaVersion = "1.3.1"
    , _version_nodeVersion = "1.9"
    , _version_middlewareVersion = Nothing
    , _version_metadata = Just $ HM.fromList
      [ "node-api-version" A..= ("0.0" :: Text)
      , "chainweb-version" A..= ("fastTimedCPM-peterson" :: Text)
      ]
    }

rosettaFailures :: [RosettaError]
rosettaFailures = rosettaError <$> enumFrom RosettaChainUnspecified

operationStatuses :: [OperationStatus]
operationStatuses =
    [ OperationStatus "Successful" True
    , OperationStatus "Remediation" True
    , OperationStatus "CoinbaseReward" True
    , OperationStatus "FundTx" True
    , OperationStatus "GasPayment" True
    , OperationStatus "TransferOrCreateAcct" True
    ]

-- ------------------------------------------------------------------ --
-- Test Pact Cmds

-- | Build a simple transfer from sender00 to sender01
--
mkTransfer :: IO (Time Micros) -> IO SubmitBatch
mkTransfer tio = do
    t <- toTxCreationTime <$> tio
    n <- readIORef nonceRef
    c <- buildTextCmd
      $ set cbSigners
        [ mkSigner' sender00
          [ mkTransferCap "sender00" "sender01" 1.0
          , mkGasCap
          ]
        ]
      $ set cbCreationTime t
      $ set cbNetworkId (Just v)
      $ mkCmd ("nonce-transfer-" <> sshow t <> "-" <> sshow n)
      $ mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"

    modifyIORef' nonceRef (+1)
    return $ SubmitBatch (pure c)

-- | Transfer one token from sender00 to sender01, applying some callback to
-- the command batch before sending. This is used for updating 'MVar's that
-- require request keys for rosetta tx submission in the mempool endpoints.
--
transferOneAsync
    :: IO (Time Micros)
    -> ClientEnv
    -> (RequestKeys -> IO ())
    -> IO PollResponses
transferOneAsync tio cenv callback = do
    batch0 <- mkTransfer tio
    void $! callback (f batch0)
    rks <- sending cid cenv batch0
    prs <- polling cid cenv rks ExpectPactResult
    return prs
  where
    f (SubmitBatch cs) = RequestKeys (cmdToRequestKey <$> cs)

transferOneAsync_
    :: IO (Time Micros)
    -> ClientEnv
    -> (RequestKeys -> IO ())
    -> IO ()
transferOneAsync_ tio cenv callback
    = void $! transferOneAsync tio cenv callback

-- ------------------------------------------------------------------ --
-- Utils


-- | Extract poll response metadata at some request key
--
extractMetadata :: RequestKey -> PollResponses -> IO (HM.HashMap Text A.Value)
extractMetadata rk (PollResponses pr) = case HM.lookup rk pr of
    Just cr -> case _crMetaData cr of
      Just (A.Object o) -> return o
      _ -> assertFailure "impossible: empty metadata"
    _ -> assertFailure "test transfer did not succeed"

-- | Tell whether a list is a subset of
-- another list
--
subset :: (Foldable f, Eq a) => f a -> f a -> Bool
subset as bs = all (`elem` bs) as

-- | Decode a JSON value or fail as an assertion. Analogous to 'read'
--
readAeson :: A.FromJSON b => A.Value -> IO b
readAeson = aeson assertFailure return . A.fromJSON
