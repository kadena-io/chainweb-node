{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Rosetta where


import GHC.Natural

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

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

nodes:: Natural
nodes = 1

type RosettaTest = IO Time Micros -> IO ChainwebNetwork -> TestTree

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

    runClientM (rosettaAccountBalanceApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaBlockTransactionApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaBlockApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaConstructionMetadataApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaConstructionSubmitApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaMempoolTransactionApiClient v cmd) cenv >>= \case
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

    runClientM (rosettaMempoolTransctionApiClient v cmd) cenv >>= \case
      Left e -> throwM $ MempoolFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      MempoolFailure _ -> return True
      _ -> return False

networkList
    :: ClientEnv
    -> NetworkListReq
    -> IO NetworkListResp
networkList cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network list for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkListApiClient v cmd) cenv >>= \case
      Left e -> throwM $ NetworkListFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkListFailure _ -> return True
      _ -> return False

networkOptions
    :: ClientEnv
    -> NetworkOptionsReq
    -> IO NetworkOptionsResp
networkOptions cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network options for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkOptionsApiClient v cmd) cenv >>= \case
      Left e -> throwM $ NetworkOptionsFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkOptionsFailure _ -> return True
      _ -> return False

networkStatus
    :: ClientEnv
    -> NetworkStatusReq
    -> IO NetworkStatusResp
networkStatus cenv req =
    recovering testRetryPolicy [h] $ \s -> do
    debug
      $ "requesting network status for " <> (take 18 $ show req)
      <> " [" <> show (view rsIterNumberL s) <> "]"

    runClientM (rosettaNetworkStatusApiClient v cmd) cenv >>= \case
      Left e -> throwM $ NetworkStatusFailure (show e)
      Right t -> return t
  where
    h _ = Handler $ \case
      NetworkStatusFailure _ -> return True
      _ -> return False
