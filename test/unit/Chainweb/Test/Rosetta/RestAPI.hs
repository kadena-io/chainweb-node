{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Chainweb.Test.Rosetta.RestAPI
( tests
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Decimal
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import Data.Foldable

import GHC.Natural
import GHC.Word

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.API
import Pact.Types.Command

-- internal chainweb modules

import Chainweb.BlockHeight
import Chainweb.Chainweb.Configuration
import Chainweb.Graph
import Chainweb.Pact.Utils (aeson)
import qualified Chainweb.Pact.Transactions.OtherTransactions as Other
import qualified Chainweb.Pact.Transactions.CoinV3Transactions as CoinV3
import qualified Chainweb.Pact.Transactions.MainnetKADTransactions as MNKAD
import Chainweb.Rosetta.Utils
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.RestAPI.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time (Time(..), Micros(..), getCurrentTimeIntegral)
import Chainweb.Utils
import Chainweb.Version

import Chainweb.Storage.Table.RocksDB

import Rosetta

import System.IO.Unsafe (unsafePerformIO)
import Chainweb.Rosetta.RestAPI.Client (rosettaConstructionDeriveApiClient)


-- -------------------------------------------------------------------------- --
-- Global Settings

v :: ChainwebVersion
v = fastForkingCpmTestVersion petersonChainGraph

nodes :: Word
nodes = 1

cid :: ChainId
cid = unsafeChainId 0

cids :: [Text]
cids = chainIds v ^.. folded . to chainIdInt . to (sshow @Int)

nonceRef :: IORef Natural
nonceRef = unsafePerformIO $ newIORef 0
{-# NOINLINE nonceRef #-}

defGasLimit, defGasPrice :: Decimal
defGasLimit = realToFrac $ _cbGasLimit defaultCmd
defGasPrice = realToFrac $ _cbGasPrice defaultCmd

defFundGas :: Decimal
defFundGas = defGasLimit * defGasPrice

gasCost :: Integer -> Decimal
gasCost units = realToFrac units * defGasPrice

defMiningReward :: Decimal
defMiningReward = 2.304523

transferGasCost :: Decimal
transferGasCost = gasCost 698

type RosettaTest = IO (Time Micros) -> IO ClientEnv -> TestTree

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Rosetta.RestAPI"
    [ rosettaTests rdb
    , constructionApiTests rdb
    ]

-- -------------------------------------------------------------------------- --
-- General Rosetta Tests (construction API disabled)

rosettaTests :: RocksDb -> TestTree
rosettaTests rdb =
    withResourceT (withNodeDbDirs rdb nodes) $ \dbdirs ->
    withResourceT (withNodesAtLatestBehavior v mkConfig =<< liftIO dbdirs) $ \envIo ->
    withResource' getCurrentTimeIntegral $ \tio ->
    independentSequentialTestGroup "Rosetta Api tests" $
        tgroup tio $ _getServiceClientEnv <$> envIo
  where
    mkConfig = configRosetta .~ True

    -- Not supported:
    --
    --  * Mempool Transaction: cant test reasonably without DOS'ing the mempool
    --  * Construction Metadata: N/A
    --
    -- Note:
    --
    --   * Tests run in sequence, but still interact with each other because
    --     confirmation depths are not validated for each tx. Checking account
    --     balances between two different tests is futile.
    --

    tgroup tio envIo = fmap (\test -> test tio envIo)
      [ blockTransactionTests
      , blockCoinV2RemediationTests
      , block20ChainRemediationTests
      , blockTests "Block Test without potential remediation"
      , accountBalanceTests
      , mempoolTests
      , networkListTests
      , networkOptionsTests
      , networkStatusTests
      , blockKAccountAfterPact42
      , blockCoinV3RemediationTests
      , constructionApiDeprecationTest
      ]

-- | Rosetta account balance endpoint tests
--
accountBalanceTests :: RosettaTest
accountBalanceTests tio envIo =
    testCaseSteps "Account Balance Tests" $ \step -> do
      step "check initial balance"
      cenv <- envIo
      resp0 <- accountBalance v cenv req
      let startBal = 99999997.8604
      checkBalance resp0 startBal

      step "send 1.0 tokens to sender00 from sender01"
      void $! transferOneAsync_ cid tio cenv (void . return)

      step "check post-transfer and gas fees balance"
      resp1 <- accountBalance v cenv req
      checkBalance resp1 (startBal - transferGasCost - 1)
  where
    req = AccountBalanceReq nid (AccountId "sender00" Nothing Nothing) Nothing

    checkBalance :: HasCallStack => AccountBalanceResp -> Decimal -> IO ()
    checkBalance resp bal1 = do
      let b0 = head $ _accountBalanceResp_balances resp
          b1 = kdaToRosettaAmount bal1
          curr = _amount_currency b0

      b1 @=? b0
      curr @=? kda

-- | Test that /block endpoint does not return a
--   TxLog parse error after fork to Pact 420.
--   This assumes that this test occurs after the
--   fork blockheight.
blockKAccountAfterPact42 :: RosettaTest
blockKAccountAfterPact42 tio envIo =
  testCaseSteps "Block k Account After Pact 420 Test" $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "send transaction"
    prs <- mkOneKCoinAccountAsync cid tio cenv (putMVar rkmv)
    rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
    cmdMeta <- KM.toMap <$> extractMetadata rk prs
    bh <- cmdMeta ^?! mix "blockHeight"

    step "check that block endpoint doesn't return TxLog parse error"
    _ <- block v cenv (req bh)
    pure ()
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

-- | Rosetta block transaction endpoint tests
--
blockTransactionTests :: RosettaTest
blockTransactionTests tio envIo =
    testCaseSteps "Block Transaction Tests" $ \step -> do
      cenv <- envIo
      rkmv <- newEmptyMVar @RequestKeys

      step "send 1.0 from sender00 to sender01 and extract block tx request"
      prs <- transferOneAsync cid tio cenv (putMVar rkmv)
      req <- mkTxReq rkmv prs

      step "send in block tx request"
      resp <- blockTransaction v cenv req

      (fundtx,deb,cred,redeem,reward) <-
        case _transaction_operations $ _blockTransactionResp_transaction resp of
          [a,b,c,d,e] -> return (a,b,c,d,e)
          _ -> assertFailure "transfer should have resulted in 5 transactions"

      -- The order in which operations are returned is flaky and may break. Use
      -- the following to double check the order in case this test fails.
      --
      -- print "fundtx: ----------------------"
      -- print fundtx
      -- print "cred: ----------------------"
      -- print cred
      -- print "deb: ----------------------"
      -- print deb
      -- print "redeem: ----------------------"
      -- print redeem
      -- print "reward: ----------------------"
      -- print reward
      -- print "----------------------"


      step "validate initial gas buy at op index 0"
      validateOp 0 "FundTx" sender00ks Successful (negate defFundGas) fundtx

      step "validate sender00 debit at op index 1"
      validateOp 1 "TransferOrCreateAcct" sender00ks Successful (negate 1.0) deb

      step "validate sender01 credit at op index 2"
      validateOp 2 "TransferOrCreateAcct" sender01ks Successful 1.0 cred

      step "validate sender00 gas redemption at op index 3"
      validateOp 3 "GasPayment" sender00ks Successful (defFundGas - transferGasCost) redeem

      step "validate miner gas reward at op index 4"
      validateOp 4 "GasPayment" noMinerks Successful transferGasCost reward

  where
    mkTxReq rkmv prs = do
      rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
      meta <- KM.toMap <$> extractMetadata rk prs
      bh <- meta ^?! mix "blockHeight"
      bhash <- meta ^?! mix "blockHash"

      let bid = BlockId bh bhash
          tid = rkToTransactionId rk

      return $ BlockTransactionReq nid bid tid


-- | Rosetta block endpoint tests
--
-- TODO: investigate
-- Attempt to buy gas failed with: : Failure: Tx Failed: read: row not found: sender00
blockTests :: String -> RosettaTest
blockTests testname tio envIo = testCaseSteps testname $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "fetch genesis block"
    (BlockResp (Just bl0) _) <- block v cenv (req 0)
    _block_blockId bl0 @?= genesisId

    step "send transaction"
    prs <- transferOneAsync cid tio cenv (putMVar rkmv)
    rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
    cmdMeta <- KM.toMap <$> extractMetadata rk prs
    bh <- cmdMeta ^?! mix "blockHeight"

    step "check tx at block height matches sent tx + remediations"
    resp1 <- block v cenv (req bh)
    validateTransferResp bh resp1
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

    validateTransferResp bh resp = do
      _blockResp_otherTransactions resp @?= Nothing

      let validateBlock someBlock = do
            Just b <- pure someBlock
            _block_metadata b @?= Nothing
            _blockId_index (_block_blockId b) @?= bh
            _blockId_index (_block_parentBlockId b) @?= (bh - 1)

            case _block_transactions b of
              [x,y] -> do
                -- not a remediation block
                let ops = _transaction_operations x <> _transaction_operations y
                case ops of
                  [a,b',c,d,e,f] -> validateTxs Nothing a b' c d e f
                  _ -> assertFailure "should have 6 ops: coinbase + 5 for transfer tx"

              _ -> assertFailure "block should have at least 2 transactions: coinbase + txs"

      validateBlock $ _blockResp_block resp

    validateTxs remeds cbase fundtx deb cred gasRedeem gasReward = do
      -- The order in which operations are returned is flaky and may break. Use
      -- the following to double check the order in case this test fails.
      --
      -- step $ "fundtx: ----------------------"
      -- step $ debugShowOperation fundtx
      -- step $ "deb: ----------------------"
      -- step $ debugShowOperation deb
      -- step $ "cred: ----------------------"
      -- step $ debugShowOperation cred
      -- step $ "redeem: ----------------------"
      -- step $ debugShowOperation gasRedeem
      -- step $ "reward: ----------------------"
      -- step $ debugShowOperation gasReward
      -- step $ "----------------------"

      -- coinbase is considered a separate tx list
      validateOp 0 "CoinbaseReward" noMinerks Successful defMiningReward cbase

      -- 20 chain remediation
      case remeds of
        Just rem1 -> validateOp 0 "TransferOrCreateAcct" e7f7ks Remediation (negate 100) rem1
        Nothing -> pure ()

      -- rest txs (i.e. transfer transaction)
      validateOp 0 "FundTx" sender00ks Successful (negate defFundGas) fundtx
      validateOp 1 "TransferOrCreateAcct" sender00ks Successful (negate 1.0) deb
      validateOp 2 "TransferOrCreateAcct" sender01ks Successful 1.0 cred
      validateOp 3 "GasPayment" sender00ks Successful (defFundGas - transferGasCost) gasRedeem
      validateOp 4 "GasPayment" noMinerks Successful transferGasCost gasReward

blockCoinV2RemediationTests :: RosettaTest
blockCoinV2RemediationTests _ envIo =
  testCaseSteps "Block CoinV2 Remediation Tests" $ \step -> do
    cenv <- envIo

    step "fetch coin v2 remediation block"
    resp <- block v cenv (req bhCoinV2Rem)

    step "validate block"
    _blockResp_otherTransactions resp @?= Nothing
    Just b <- pure $ _blockResp_block resp
    _block_metadata b @?= Nothing
    _blockId_index (_block_blockId b) @?= bhCoinV2Rem
    _blockId_index (_block_parentBlockId b) @?= (bhCoinV2Rem - 1)

    case _block_transactions b of
      x:y:z:_ -> do
        step "check remediation transactions' request keys"
        [ycmd, zcmd] <- return Other.transactions
        _transaction_transactionId y @?= pactHashToTransactionId (_cmdHash ycmd)
        _transaction_transactionId z @?= pactHashToTransactionId (_cmdHash zcmd)

        step "check remediation transactions' operations"
        _transaction_operations y @?= [] -- didn't touch the coin table
        _transaction_operations z @?= [] -- didn't touch the coin table
                                         -- NOTE: no remedition withdrawl happens in this version

        step "check coinbase transaction"
        [cbase] <- pure $ _transaction_operations x
        validateOp 0 "CoinbaseReward" noMinerks Successful defMiningReward cbase

      _ -> assertFailure $ "coin v2 remediation block should have at least 3 transactions:"
           ++ " coinbase + 2 remediations"
  where
    bhCoinV2Rem = v ^?! versionForks . at CoinV2 . _Just . atChain cid . _ForkAtBlockHeight . to getBlockHeight
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

block20ChainRemediationTests :: RosettaTest
block20ChainRemediationTests _ envIo =
  testCaseSteps "Block 20 Chain Remediation Tests" $ \step -> do
    cenv <- envIo

    step "fetch  remediation block"
    resp <- block v cenv (req bhChain20Rem)

    step "validate block"
    _blockResp_otherTransactions resp @?= Nothing
    Just b <- pure $ _blockResp_block resp
    _block_metadata b @?= Nothing
    _blockId_index (_block_blockId b) @?= bhChain20Rem
    _blockId_index (_block_parentBlockId b) @?= (bhChain20Rem - 1)

    case _block_transactions b of
      x:y:_ -> do
        step "check remediation transactions' request keys"
        [ycmd] <- return MNKAD.transactions
        _transaction_transactionId y @?= pactHashToTransactionId (_cmdHash ycmd)

        step "check remediation transactions' operations"
        case _transaction_operations x <> _transaction_operations y of
          [cbase,remOp] -> do
            validateOp 0 "CoinbaseReward" noMinerks Successful defMiningReward cbase
            validateOp 0 "TransferOrCreateAcct" e7f7ks Remediation (negate 100) remOp

          _ -> assertFailure "total # of ops should be == 2: coinbase + remediation"

      _ -> assertFailure $ "20 chain remediation block should have at least 2 transactions:"
           ++ " coinbase + 1 remediations"
  where
    bhChain20Rem = 2
    nidChain3 = NetworkId
      { _networkId_blockchain = "kadena"
      , _networkId_network = "fastfork-CPM-peterson"
      , _networkId_subNetworkId = Just (SubNetworkId "3" Nothing)
      }
    req h = BlockReq nidChain3 $ PartialBlockId (Just h) Nothing

blockCoinV3RemediationTests :: RosettaTest
blockCoinV3RemediationTests _ envIo =
  testCaseSteps "Block CoinV3 Remediation Tests" $ \step -> do
    cenv <- envIo

    step "fetch coin v3 remediation block"
    resp <- block v cenv (req bhCoinV3Rem)

    step "validate block"
    _blockResp_otherTransactions resp @?= Nothing
    Just b <- pure $ _blockResp_block resp
    _block_metadata b @?= Nothing
    _blockId_index (_block_blockId b) @?= bhCoinV3Rem
    _blockId_index (_block_parentBlockId b) @?= (bhCoinV3Rem - 1)

    case _block_transactions b of
      x:y:_ -> do
        step "check remediation transactions' request keys"
        [ycmd] <- return CoinV3.transactions
        _transaction_transactionId y @?= pactHashToTransactionId (_cmdHash ycmd)

        step "check remediation transactions' operations"
        _transaction_operations y @?= [] -- didn't touch the coin table
                                         -- NOTE: no remedition withdrawl happens in this version

        step "check coinbase transaction"
        [cbase] <- pure $ _transaction_operations x
        validateOp 0 "CoinbaseReward" noMinerks Successful defMiningReward cbase

      _ -> assertFailure $ "coin v3 remediation block should have at least 3 transactions:"
           ++ " coinbase + 2 remediations"
  where
    bhCoinV3Rem = v ^?! versionForks . at Pact4Coin3 . _Just . atChain cid . _ForkAtBlockHeight . to getBlockHeight
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

-- | Rosetta mempool endpoint tests
--
mempoolTests :: RosettaTest
mempoolTests tio envIo = testCaseSteps "Mempool Tests" $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "execute transfer and wait on mempool data"
    void $! async $ transferOneAsync_ cid tio cenv (putMVar rkmv)
    rk NEL.:| [] <- _rkRequestKeys <$> takeMVar rkmv

    let tid = rkToTransactionId rk
    let test (MempoolResp ts) = return $ elem tid ts

    step "compare requestkey against mempool responses"
    void $! repeatUntil test $ mempool v cenv req
  where
    req = NetworkReq nid Nothing

-- | Rosetta network list endpoint tests
--
networkListTests :: RosettaTest
networkListTests _ envIo =
    testCaseSteps "Network List Tests" $ \step -> do
      cenv <- envIo

      step "send network list request"
      resp <- networkList v cenv req

      for_ (_networkListResp_networkIds resp) $ \n -> do
         _networkId_blockchain n @=? "kadena"
         _networkId_network n @=? "fastfork-CPM-peterson"
         assertBool "chain id of subnetwork is valid"
           $ maybe False (\a -> _subNetworkId_network a `elem` cids)
           $ _networkId_subNetworkId n
  where
    req = MetadataReq Nothing

-- | Rosetta network options tests
--
networkOptionsTests :: RosettaTest
networkOptionsTests _ envIo =
    testCaseSteps "Network Options Tests" $ \step -> do
      cenv <- envIo

      step "send network options request"
      resp <- networkOptions v cenv req0

      let allow = _networkOptionsResp_allow resp
          version = _networkOptionsResp_version resp

      step "check options responses against allowable data and versions"
      version  @=? rosettaVersion

      step "Check that response errors are a subset of valid errors"
      respErrors resp @?= rosettaFailures

      step "Check that response statuses are a subset of valid statuses"
      _allow_operationStatuses allow @?= operationStatuses

      step "Check that response op types are a subset of op types"
      _allow_operationTypes allow @?= operationTypes
  where
    req0 = NetworkReq nid Nothing
    respErrors = _allow_errors . _networkOptionsResp_allow

-- | Rosetta network status tests
--
networkStatusTests :: RosettaTest
networkStatusTests tio envIo =
    testCaseSteps "Network Status Tests" $ \step -> do
      cenv <- envIo

      step "send network status request"
      resp0 <- networkStatus v cenv req

      step "check status response against genesis"
      genesisId @=? _networkStatusResp_genesisBlockId resp0

      step "send in a transaction and update current block"
      transferOneAsync_ cid tio cenv (void . return)
      resp1 <- networkStatus v cenv req

      step "check status response genesis and block height"
      genesisId @=? _networkStatusResp_genesisBlockId resp1
      (blockIdOf resp1 > blockIdOf resp0) @? "current block id heights must increment"
  where
    req = NetworkReq nid Nothing

    blockIdOf = _blockId_index . _networkStatusResp_currentBlockId

-- | Test proper deprecation message when construction API is disabled
--
constructionApiDeprecationTest :: RosettaTest
constructionApiDeprecationTest _ envIo =
    testCaseSteps "Calling disabled construction Api results in failure" $ \step -> do
        cenv <- envIo
        step "rosetta API is enabled and ready to be used"
        assertRosettaApi cenv
        step "call construction API endpoint"
        void $ callConstrunctionApi cenv >>= \case
            Left (FailureResponse _ (Response { responseBody = x})) ->
                case A.eitherDecode @(HM.HashMap String A.Value) x of
                    Left e -> assertFailure $ "decoding of response failed: " <> e
                    Right y -> case HM.lookup "code" y of
                        Nothing -> assertFailure "decoding of response failed"
                        Just c -> assertEqual "failure code is 35" (A.Number 35) c
            Left e -> assertFailure $ "unexpected failure: " <> show e
            Right t -> assertFailure $ "unexpected success: " <> show t

-- -------------------------------------------------------------------------- --
-- Construction API Tests

-- | The implementation of the construction API is flaky and deprecated. We
-- don't provide test coverage for its functionality. We only check whether it
-- is enabled when the configuration requests it.
--
constructionApiTests :: RocksDb -> TestTree
constructionApiTests rdb =
    withResourceT (withNodeDbDirs rdb nodes) $ \dbdirs ->
    withResourceT (withNodesAtLatestBehavior v mkConfig =<< liftIO dbdirs) $ \envIo ->

    testCaseSteps "Construction API available" $ \step -> do
        cenv <- _getServiceClientEnv <$> envIo
        step "General Rosetta API is enabled and ready to be used"
        assertRosettaApi cenv

        step "call construction API endpoint"
        void $ callConstrunctionApi cenv >>= \case
            Right _ -> return ()
            Left e -> assertFailure $ show e
  where
    mkConfig = (configRosetta .~ True) . (configRosettaConstructionApi .~ True)

callConstrunctionApi :: ClientEnv -> IO (Either ClientError ConstructionDeriveResp)
callConstrunctionApi = runClientM (rosettaConstructionDeriveApiClient v req)
  where
    netId = nid { _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing) }
    rosettaPubKeySender01 = RosettaPublicKey (fst sender01) CurveEdwards25519
    req = ConstructionDeriveReq netId rosettaPubKeySender01 Nothing

-- ------------------------------------------------------------------ --
-- Test Data

kda :: Currency
kda = Currency "KDA" 12 Nothing

nid :: NetworkId
nid = NetworkId
    { _networkId_blockchain = "kadena"
    , _networkId_network = "fastfork-CPM-peterson"
    , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
    }

genesisId :: BlockId
genesisId = BlockId 0 "dqdUQNqEXcdMDeb6xWXuv1_KvLvDXysgsaEU8ZfLs9Q"

rosettaVersion :: RosettaNodeVersion
rosettaVersion = RosettaNodeVersion
    { _version_rosettaVersion = "1.4.4"
    , _version_nodeVersion = VERSION_chainweb
    , _version_middlewareVersion = Nothing
    , _version_metadata = Just $ KM.fromList
      [ "node-api-version" A..= ("0.0" :: Text)
      , "chainweb-version" A..= ("fastfork-CPM-peterson" :: Text)
      , "rosetta-chainweb-version" A..= ("2.0.0" :: Text)
      ]
    }

rosettaFailures :: [RosettaError]
rosettaFailures = map (`rosettaError` Nothing) (enumFrom RosettaChainUnspecified)

operationStatuses :: [OperationStatus]
operationStatuses =
    [ OperationStatus "Successful" True
    , OperationStatus "Remediation" True
    ]

operationTypes :: [Text]
operationTypes =
    [ "CoinbaseReward"
    , "FundTx"
    , "GasPayment"
    , "TransferOrCreateAcct"
    ]

-- | Validate all useful data for a tx operation
--
validateOp
    :: Word64
      -- ^ op idx
    -> Text
      -- ^ operation type
    -> TestKeySet
      -- ^ operation keyset
    -> ChainwebOperationStatus
      -- ^ operation status
    -> Decimal
      -- ^ operation balance delta
      -- (how balance increased or decreased in given operation)
    -> Operation
      -- ^ the op
    -> Assertion
validateOp idx opType ks st bal o = do
    _operation_operationId o @?= OperationId idx Nothing
    _operation_type o @?= opType
    _operation_status o @?= sshow st
    _operation_account o @?= Just (AccountId acct Nothing acctMeta)
    _operation_amount o @?= Just balRosettaAmt
  where
    balRosettaAmt = kdaToRosettaAmount bal
    acct = _testKeySet_name ks
    _publicKeys = case _testKeySet_key ks of
      Nothing -> []
      Just k -> [fst k]
    _pred' = _testKeySet_pred ks
    acctMeta = Nothing

-- ------------------------------------------------------------------ --
-- Test Pact Cmds

-- | Build a simple transfer from sender00 to sender01
--
mkTransfer :: ChainId -> IO (Time Micros) -> IO SubmitBatch
mkTransfer sid tio = do
    t <- toTxCreationTime <$> tio
    n <- readIORef nonceRef
    c <- buildTextCmd ("nonce-transfer-" <> sshow t <> "-" <> sshow n) v
      $ set cbSigners
        [ mkEd25519Signer' sender00
          [ mkTransferCap "sender00" "sender01" 1.0
          , mkGasCap
          ]
        ]
      $ set cbCreationTime t
      $ set cbChainId sid
      $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
      $ defaultCmd

    modifyIORef' nonceRef (+1)
    return $ SubmitBatch (pure c)

mkKCoinAccount :: ChainId -> IO (Time Micros) -> IO SubmitBatch
mkKCoinAccount sid tio = do
    let kAcct = "k:" <> fst sender00
    t <- toTxCreationTime <$> tio
    n <- readIORef nonceRef
    c <- buildTextCmd ("nonce-transfer-" <> sshow t <> "-" <> sshow n) v
      $ set cbSigners
        [ mkEd25519Signer' sender00
          [ mkTransferCap "sender00" kAcct 20.0
          , mkGasCap ]
        ]
      $ set cbCreationTime t
      $ set cbChainId sid
      $ set cbRPC
          (mkExec ("(coin.transfer-create \"sender00\" \"" <> kAcct <> "\" (read-keyset \"sender00\") 20.0)")
            (mkKeySetData "sender00" [sender00]))

      $ defaultCmd

    modifyIORef' nonceRef (+1)
    return $ SubmitBatch (pure c)

mkOneKCoinAccountAsync
    :: ChainId
    -> IO (Time Micros)
    -> ClientEnv
    -> (RequestKeys -> IO ())
    -> IO PollResponses
mkOneKCoinAccountAsync sid tio cenv callback = do
    batch0 <- mkKCoinAccount sid tio
    void $! callback (f batch0)
    rks <- sending v cid cenv batch0
    polling v cid cenv rks ExpectPactResult
  where
    f (SubmitBatch cs) = RequestKeys (cmdToRequestKey <$> cs)

-- | Transfer one token from sender00 to sender01, applying some callback to
-- the command batch before sending.
--
transferOneAsync
    :: ChainId
    -> IO (Time Micros)
    -> ClientEnv
    -> (RequestKeys -> IO ())
    -> IO PollResponses
transferOneAsync sid tio cenv callback = do
    batch0 <- mkTransfer sid tio
    void $! callback (f batch0)
    rks <- sending v cid cenv batch0
    polling v cid cenv rks ExpectPactResult
  where
    f (SubmitBatch cs) = RequestKeys (cmdToRequestKey <$> cs)

-- | Transfer one token from sender00 to sender01 asynchronously applying some
-- callback (usually putting the requestkeys into some 'MVar'), and forgetting
-- the poll response results. We use this when we want to just execute and poll
-- and do not need the responses.
--
transferOneAsync_
    :: ChainId
    -> IO (Time Micros)
    -> ClientEnv
    -> (RequestKeys -> IO ())
    -> IO ()
transferOneAsync_ sid tio cenv callback
    = void $! transferOneAsync sid tio cenv callback

-- ------------------------------------------------------------------ --
-- Utils

-- | Extract poll response metadata at some request key
--
extractMetadata :: RequestKey -> PollResponses -> IO (KM.KeyMap A.Value)
extractMetadata rk (PollResponses pr) = case HM.lookup rk pr of
    Just cr -> case _crMetaData cr of
      Just (A.Object o) -> return o
      _ -> assertFailure "impossible: empty metadata"
    _ -> assertFailure "test transfer did not succeed"

-- | A composition of an index into a k-v structure with aeson values
-- and conversion to non-JSONified structured, asserting test failure if
-- it fails to decode as the give type @a@.
--
mix
    :: forall a m
    . ( A.FromJSON a
      , Ixed m
      , IxValue m ~ A.Value
      )
    => Index m
    -> Fold m (IO a)
mix i = ix i . to A.fromJSON . to (aeson assertFailure return)

assertRosettaApi :: ClientEnv -> IO ()
assertRosettaApi cenv = do
    resp <- networkStatus v cenv (NetworkReq nid Nothing)
    genesisId @=? _networkStatusResp_genesisBlockId resp

-- ------------------------------------------------------------------ --
-- Key Sets

data TestKeySet = TestKeySet
  { _testKeySet_name :: !Text
  , _testKeySet_key :: !(Maybe SimpleKeyPair)
  , _testKeySet_pred :: !Text
  }

e7f7ks :: TestKeySet
e7f7ks = TestKeySet
  { _testKeySet_name = "e7f7634e925541f368b827ad5c72421905100f6205285a78c19d7b4a38711805"
  , _testKeySet_key = Just ("e7f7634e925541f368b827ad5c72421905100f6205285a78c19d7b4a38711805"
                           , "") -- Never used for signing
  , _testKeySet_pred = "keys-all"
  }

noMinerks :: TestKeySet
noMinerks = TestKeySet
  { _testKeySet_name = "NoMiner"
  , _testKeySet_key = Nothing
  , _testKeySet_pred = "<"
  }

sender00ks :: TestKeySet
sender00ks = TestKeySet
  { _testKeySet_name = "sender00"
  , _testKeySet_key = Just sender00
  , _testKeySet_pred = "keys-all"
  }

sender01ks :: TestKeySet
sender01ks = TestKeySet
  { _testKeySet_name = "sender01"
  , _testKeySet_key = Just sender01
  , _testKeySet_pred = "keys-all"
  }
