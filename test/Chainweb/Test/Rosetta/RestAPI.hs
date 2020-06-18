{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Rosetta.RestAPI
( tests
) where


import GHC.Natural

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.Graph
import Chainweb.Test.RestAPI.Utils
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

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Rosetta.RestAPI" go
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
      , mempoolTests
      , networkListTests
      , networkOptionsTests
      , networkStatusTests
      ]


accountBalanceTests :: RosettaTest
accountBalanceTests _tio _nio = testCaseSteps "Account Balance Lookup" $ \step -> do
    step "check initial balance"
    cenv <- _runClientEnv <$> _nio
    r <- accountBalance cenv req
    print r
  where
    req = AccountBalanceReq nid aid Nothing

blockTransactionTests :: RosettaTest
blockTransactionTests _tio _nio =
    testCaseSteps "Block Transaction Tests" $ \step -> return ()

blockTests :: RosettaTest
blockTests _tio _nio =
    testCaseSteps "Block Tests" $ \step -> return ()

constructionMetadataTests :: RosettaTest
constructionMetadataTests _tio _nio =
    testCaseSteps "Construction Metadata Tests" $ \step -> return ()

constructionSubmitTests :: RosettaTest
constructionSubmitTests _tio _nio =
    testCaseSteps "Construction Submit Tests" $ \step -> return ()

mempoolTransactionTests :: RosettaTest
mempoolTransactionTests _tio _nio =
    testCaseSteps "Mempool Transaction Tests" $ \step -> return ()

mempoolTests :: RosettaTest
mempoolTests _tio _nio =
    testCaseSteps "Mempool Tests" $ \step -> return ()

networkListTests :: RosettaTest
networkListTests _tio _nio =
    testCaseSteps "Network List Tests" $ \step -> return ()

networkOptionsTests :: RosettaTest
networkOptionsTests _tio _nio =
    testCaseSteps "Network Options Tests" $ \step -> return ()

networkStatusTests :: RosettaTest
networkStatusTests _tio _nio = testCaseSteps "Network Status Tests" $ \step ->
    return ()

-- ------------------------------------------------------------------ --
-- Test Data

type RosettaTest = IO (Time Micros) -> IO ChainwebNetwork -> TestTree

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
