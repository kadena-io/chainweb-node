{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Test.Rosetta where


import GHC.Natural

import Test.Tasty
import Test.Tasty.HUnit

import Chainweb.Graph
import Chainweb.Time (Time(..), Micros(..))
import Chainweb.Version
import Chainweb.Test.Utils

import Data.CAS.RocksDB

import Rosetta

-- -------------------------------------------------------------------------- --
-- Global Settings

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

nodes:: Natural
nodes = 1

type RosettaTest = IO (Time Micros) -> IO ChainwebNetwork -> TestTree

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
      withTime $ \tio -> tgrp nio tio

    tgrp nio tio
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
accountBalanceTests _tio _nio = testCaseSteps "Account Balance Lookup" $ \ _step -> do
    _cenv <- _runClientEnv <$> _nio
    return ()
  where
    _req = AccountBalanceReq nid aid Nothing

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
