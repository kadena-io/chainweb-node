{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Chainweb.Test.Rosetta.RestAPI
( tests
) where



import Control.Lens

import qualified Data.Aeson as A
import Data.Functor (void)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)

import GHC.Natural

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.API

-- internal chainweb modules

import Chainweb.Graph
import Chainweb.Rosetta.Utils
import Chainweb.Test.Pact.Utils
import Chainweb.Test.RestAPI.Utils
import Chainweb.Test.Utils
import Chainweb.Time (Time(..), Micros(..))
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import Rosetta


-- -------------------------------------------------------------------------- --
-- Global Settings

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

nodes:: Natural
nodes = 1

cid :: ChainId
cid = unsafeChainId 0

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Rosetta.RestAPI" go
  where
    go = return $
      withNodes v "rosettaRemoteTests-" rdb nodes $ \envIo ->
      withTime $ \tio -> testGroup "Rosetta API tests" (tgroup tio envIo)

    tgroup tio envIo = fmap (\test -> test tio envIo)
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
accountBalanceTests tio envIo = testCaseSteps "Account Balance Lookup" $ \step -> do
    step "check initial balance"
    cenv <- envIo
    resp0 <- accountBalance cenv req
    checkBalance resp0 100000000.000

    step "send 1.0 tokens to sender00 from sender01"
    batch0 <- transferOne tio
    rks <- sending cid cenv batch0
    void $ polling cid cenv rks ExpectPactResult

    step "check post-transfer balance"
    resp1 <- accountBalance cenv req
    checkBalance resp1 99999998.9453
  where
    req = AccountBalanceReq nid aid Nothing

    checkBalance resp bal1 = do
      let b0 = head $ _accountBalanceResp_balances resp
          b1 = kdaToRosettaAmount bal1
          curr = _amount_currency b0

      b1 @=? b0
      curr @=? kda

blockTransactionTests :: RosettaTest
blockTransactionTests tio envIo =
    testCaseSteps "Block Transaction Tests" $ \step -> return ()

blockTests :: RosettaTest
blockTests tio envIo = testCaseSteps "Block Tests" $ \step -> do
    step "fetch genesis block"
    cenv <- envIo
    resp <- block cenv req
    return ()
  where
    req = BlockReq nid $ PartialBlockId (Just 0) Nothing

constructionMetadataTests :: RosettaTest
constructionMetadataTests tio envIo =
    testCaseSteps "Construction Metadata Tests" $ \step -> return ()

constructionSubmitTests :: RosettaTest
constructionSubmitTests tio envIo =
    testCaseSteps "Construction Submit Tests" $ \step -> return ()

mempoolTransactionTests :: RosettaTest
mempoolTransactionTests tio envIo =
    testCaseSteps "Mempool Transaction Tests" $ \step -> return ()

mempoolTests :: RosettaTest
mempoolTests tio envIo =
    testCaseSteps "Mempool Tests" $ \step -> return ()

networkListTests :: RosettaTest
networkListTests tio envIo =
    testCaseSteps "Network List Tests" $ \step -> return ()

networkOptionsTests :: RosettaTest
networkOptionsTests tio envIo =
    testCaseSteps "Network Options Tests" $ \step -> return ()

networkStatusTests :: RosettaTest
networkStatusTests tio envIo = testCaseSteps "Network Status Tests" $ \step ->
    return ()

-- ------------------------------------------------------------------ --
-- Test Data

type RosettaTest
    = IO (Time Micros) -> IO ClientEnv -> TestTree

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
genesisId = BlockId 0 "qU76xuohSdCyFQd2QWm7TMJHyHIRCENmdax9KMnQiSQ"

-- ------------------------------------------------------------------ --
-- Test Pact Cmds

transferOne :: IO (Time Micros) -> IO SubmitBatch
transferOne tio = do
    t <- toTxCreationTime <$> tio
    c <- buildTextCmd
      $ set cbSigners
        [ mkSigner' sender00
          [ mkTransferCap "sender00" "sender01" 1.0
          , mkGasCap
          ]
        ]
      $ set cbCreationTime t
      $ set cbNetworkId (Just v)
      $ mkCmd ("nonce-transfer-" <> sshow t)
      $ mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"

    return $ SubmitBatch (pure c)
