{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Chainweb.Test.Rosetta.RestAPI
( tests
) where



import Control.Lens

import qualified Data.Aeson as A
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import Data.Foldable (traverse_)

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
cids = chainIds v ^.. folded . to (sshow @Int. chainIdInt)

nonceRef :: IORef Natural
nonceRef = unsafePerformIO $ newIORef 0


type RosettaTest
    = IO (Time Micros) -> IO ClientEnv -> TestTree

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
    void $ transferOne tio cenv

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
    testCaseSteps "Block Transaction Tests" $ \step -> do
      cenv <- envIo

      step "fetch genesis tx id"
      -- resp <- blockTransaction cenv req

      return ()
  where
    req = BlockTransactionReq nid genesisId genesisTxId

blockTests :: RosettaTest
blockTests tio envIo = testCaseSteps "Block Tests" $ \step -> do
    step "fetch genesis block"
    cenv <- envIo
    resp0 <- _block_blockId . _blockResp_block <$> block cenv (req 0)
    resp0 @=? genesisId

    step "send transaction"
    prs <- transferOne tio cenv
    cmdMeta <- extractMetadata prs
    bh <- cmdMeta ^?! ix "blockHeight" . to fromAeson

    step "check tx at block height matches sent tx"
    resp1 <- block cenv (req bh)

    step "validate remediations at block height 1"
    remResp <- block cenv (req 1)
    -- check remediation tx id's
    return ()
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

    fromAeson = aeson assertFailure return . A.fromJSON

    extractMetadata (PollResponses pr) =
      case _crMetaData . snd . head . HM.toList $ pr of
        Nothing -> assertFailure "test transfer did not succeed"
        Just (A.Object o) -> return o

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
networkListTests _ envIo = testCaseSteps "Network List Tests" $ \step -> do
    cenv <- envIo
    step "send network list request"
    resp <- networkList cenv req
    traverse_ checkChainIds (_networkListResp_networkIds resp)
  where
    req = MetadataReq Nothing

    checkChainIds n = do
      _networkId_blockchain n @=? "kadena"
      _networkId_network n @=? "fastTimedCPM-peterson"
      assertBool "chain id of subnetwork is valid" $
        case _networkId_subNetworkId n of
          Nothing -> False
          Just a -> elem (_subNetworkId_network a) cids

networkOptionsTests :: RosettaTest
networkOptionsTests _ envIo = testCaseSteps "Network Options Tests" $ \step -> do
    cenv <- envIo

    step "send network options request"
    resp <- networkOptions cenv req0

    step "check options response against node version"
    _networkOptionsResp_version resp @=? rosettaVersion

    -- enumerate over exceptions

  where
    req0 = NetworkReq nid Nothing

networkStatusTests :: RosettaTest
networkStatusTests tio envIo = testCaseSteps "Network Status Tests" $ \step -> do
    cenv <- envIo

    step "send network status request"
    transferOne_ tio cenv
    resp <- networkStatus cenv req

    step "check status response against genesis"
    genesisId @=? _networkStatusResp_genesisBlockId resp
    -- check current block (should be updated by tx)
  where
    req = NetworkReq nid Nothing

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

genesisTxId :: TransactionId
genesisTxId = TransactionId "Inlsd2hVbVVBOUtnZjM5d191c2dGRHoycV9RX09YX1lMQmNDMXZBSC1Mc0Ei"

-- write out allowables and check failures against alloweds

-- ------------------------------------------------------------------ --
-- Test Pact Cmds

-- | Transfer one token from sender00 to sender01, polling for responses
--
transferOne :: IO (Time Micros) -> ClientEnv -> IO PollResponses
transferOne tio cenv = do
    batch0 <- mkTransfer
    rks <- sending cid cenv batch0
    prs <- polling cid cenv rks ExpectPactResult
    return prs
  where
    mkTransfer = do
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

-- | Transfer one, ignoring the resulting responses
--
transferOne_ :: IO (Time Micros) -> ClientEnv -> IO ()
transferOne_ tio cenv = void $! transferOne tio cenv
