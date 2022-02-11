{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Rosetta.RestAPI
( tests
) where


import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens

import qualified Data.Aeson as A
import Data.Decimal
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Foldable

import GHC.Natural
import GHC.Word

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.API
import Pact.Types.Command

import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Command as P
import qualified Pact.ApiReq as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.PactValue as P

-- internal chainweb modules

import Chainweb.Graph
import Chainweb.Pact.Utils (aeson)
import Chainweb.Pact.Transactions.UpgradeTransactions
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

nodes :: Natural
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
defGasPrice = realToFrac $_cbGasPrice defaultCmd

defFundGas :: Decimal
defFundGas = defGasLimit * defGasPrice

gasCost :: Integer -> Decimal
gasCost units = realToFrac units * defGasPrice

defMiningReward :: Decimal
defMiningReward = 2.304523

type RosettaTest = IO (Time Micros) -> IO ClientEnv -> ScheduledTest

-- -------------------------------------------------------------------------- --
-- Test Tree

tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "Chainweb.Test.Rosetta.RestAPI" go
  where
    go = return $
      withNodes v "rosettaRemoteTests-" rdb nodes $ \envIo ->
      withTime $ \tio -> testGroup "Rosetta Api tests" $
        schedule Sequential (tgroup tio $ _getServiceClientEnv <$> envIo)

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
      [ blockTests "Block Test with transfer and potential coin v2 remediation"
      , blockTests "Block Test with transfer and potential chain 20 remediation"
      , blockTransactionTests
      , blockCoinV2RemediationTests
      , block20ChainRemediationTests
      , blockTests "Block Test without potential remediation"
      , accountBalanceTests
      , constructionTransferTests
      , mempoolTests
      , networkListTests
      , networkOptionsTests
      , networkStatusTests
      , blockKAccountAfterPact420
      ]

-- | Rosetta account balance endpoint tests
--
accountBalanceTests :: RosettaTest
accountBalanceTests tio envIo =
    testCaseSchSteps "Account Balance Tests" $ \step -> do
      step "check initial balance"
      cenv <- envIo
      resp0 <- accountBalance cenv req
      checkBalance resp0 99999995.7812

      step "send 1.0 tokens to sender00 from sender01"
      void $! transferOneAsync_ cid tio cenv (void . return)

      step "check post-transfer and gas fees balance"
      resp1 <- accountBalance cenv req
      checkBalance resp1 99999994.7265
  where
    req = AccountBalanceReq nid (AccountId "sender00" Nothing Nothing) Nothing

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
blockKAccountAfterPact420 :: RosettaTest
blockKAccountAfterPact420 tio envIo =
  testCaseSchSteps "Block k:Account After Pact 420 Test" $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "send transaction"
    prs <- mkOneKCoinAccountAsync cid tio cenv (putMVar rkmv)
    rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
    cmdMeta <- extractMetadata rk prs
    bh <- cmdMeta ^?! mix "blockHeight"

    step "check that block endpoint doesn't return TxLog parse error"
    _ <- block cenv (req bh)
    pure ()
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

-- | Rosetta block transaction endpoint tests
--
blockTransactionTests :: RosettaTest
blockTransactionTests tio envIo =
    testCaseSchSteps "Block Transaction Tests" $ \step -> do
      cenv <- envIo
      rkmv <- newEmptyMVar @RequestKeys

      step "send 1.0 from sender00 to sender01 and extract block tx request"
      prs <- transferOneAsync cid tio cenv (putMVar rkmv)
      req <- mkTxReq rkmv prs

      step "send in block tx request"
      resp <- blockTransaction cenv req

      (fundtx,cred,deb,redeem,reward) <-
        case _transaction_operations $ _blockTransactionResp_transaction resp of
          [a,b,c,d,e] -> return (a,b,c,d,e)
          _ -> assertFailure "transfer should have resulted in 5 transactions"


      step "validate initial gas buy at op index 0"
      validateOp 0 "FundTx" sender00ks Successful (negate defFundGas) fundtx

      step "validate sender01 credit at op index 1"
      validateOp 1 "TransferOrCreateAcct" sender01ks Successful 1.0 cred

      step "validate sender00 debit at op index 2"
      validateOp 2 "TransferOrCreateAcct" sender00ks Successful (negate 1.0) deb

      step "validate sender00 gas redemption at op index 3"
      validateOp 3 "GasPayment" sender00ks Successful (defFundGas - transferGasCost) redeem

      step "validate miner gas reward at op index 4"
      validateOp 4 "GasPayment" noMinerks Successful transferGasCost reward

  where
    transferGasCost = gasCost 547

    mkTxReq rkmv prs = do
      rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
      meta <- extractMetadata rk prs
      bh <- meta ^?! mix "blockHeight"
      bhash <- meta ^?! mix "blockHash"

      let bid = BlockId bh bhash
          tid = rkToTransactionId rk

      return $ BlockTransactionReq nid bid tid


-- | Rosetta block endpoint tests
--
blockTests :: String -> RosettaTest
blockTests testname tio envIo = testCaseSchSteps testname $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "fetch genesis block"
    (BlockResp (Just bl0) _) <- block cenv (req 0)
    _block_blockId bl0 @?= genesisId

    step "send transaction"
    prs <- transferOneAsync cid tio cenv (putMVar rkmv)
    rk <- NEL.head . _rkRequestKeys <$> takeMVar rkmv
    cmdMeta <- extractMetadata rk prs
    bh <- cmdMeta ^?! mix "blockHeight"

    step "check tx at block height matches sent tx + remediations"
    resp1 <- block cenv (req bh)
    validateTransferResp bh resp1
  where
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing
    transferGasCost = gasCost 547

    validateTransferResp bh resp = do
      _blockResp_otherTransactions resp @?= Nothing

      let validateBlock someBlock = do
            Just b <- pure someBlock
            _block_metadata b @?= Nothing
            _blockId_index (_block_blockId b) @?= bh
            _blockId_index (_block_parentBlockId b) @?= (bh - 1)

            case _block_transactions b of
              [x,r1,r2,y] -> do
                -- coin v2 remediation block.
                -- No coin table remediation for this version.
                let ops = _transaction_operations x <> _transaction_operations r1 <>
                          _transaction_operations r2 <> _transaction_operations y
                case ops of
                  [a,b',c,d,e,f] -> validateTxs Nothing a b' c d e f
                  _ -> assertFailure "should have 6 ops: coinbase + 5 for transfer tx"

              [x,r1,y] -> do
                -- 20 chain remediation block
                let ops = _transaction_operations x <> _transaction_operations r1 <>
                          _transaction_operations y
                case ops of
                  [a,rop1, b',c,d,e,f] -> validateTxs (Just rop1) a b' c d e f
                  _ -> assertFailure "should have 7 ops: coinbase + 20 chain rem + 5 for transfer tx"

              [x,y] -> do
                -- not a remediation block
                let ops = _transaction_operations x <> _transaction_operations y
                case ops of
                  [a,b',c,d,e,f] -> validateTxs Nothing a b' c d e f
                  _ -> assertFailure "should have 6 ops: coinbase + 5 for transfer tx"
     
              _ -> assertFailure "block should have at least 2 transactions: coinbase + txs"

      validateBlock $ _blockResp_block resp

    validateTxs remeds cbase fundtx cred deb redeem reward = do

      -- coinbase is considered a separate tx list
      validateOp 0 "CoinbaseReward" noMinerks Successful defMiningReward cbase

      -- 20 chain remediation
      case remeds of
        Just rem1 -> validateOp 0 "TransferOrCreateAcct" e7f7ks Remediation (negate 100) rem1
        Nothing -> pure ()

      -- rest txs (i.e. transfer transaction)
      validateOp 0 "FundTx" sender00ks Successful (negate defFundGas) fundtx
      validateOp 1 "TransferOrCreateAcct" sender01ks Successful 1.0 cred
      validateOp 2 "TransferOrCreateAcct" sender00ks Successful (negate 1.0) deb
      validateOp 3 "GasPayment" sender00ks Successful (defFundGas - transferGasCost) redeem
      validateOp 4 "GasPayment" noMinerks Successful transferGasCost reward

blockCoinV2RemediationTests :: RosettaTest
blockCoinV2RemediationTests _ envIo =
  testCaseSchSteps "Block CoinV2 Remediation Tests" $ \step -> do
    cenv <- envIo

    step "fetch coin v2 remediation block"
    resp <- block cenv (req bhCoinV2Rem)

    step "validate block"
    _blockResp_otherTransactions resp @?= Nothing
    Just b <- pure $ _blockResp_block resp
    _block_metadata b @?= Nothing
    _blockId_index (_block_blockId b) @?= bhCoinV2Rem
    _blockId_index (_block_parentBlockId b) @?= (bhCoinV2Rem - 1)

    case _block_transactions b of
      x:y:z:_ -> do
        step "check remediation transactions' request keys"
        -- TODO: are these unique across lifetime of blockchain/chains?
        [ycmd, zcmd] <- upgradeTransactions v cid
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
    bhCoinV2Rem = 1
    req h = BlockReq nid $ PartialBlockId (Just h) Nothing

block20ChainRemediationTests :: RosettaTest
block20ChainRemediationTests _ envIo =
  testCaseSchSteps "Block 20 Chain Remediation Tests" $ \step -> do
    cenv <- envIo

    step "fetch  remediation block"
    resp <- block cenv (req bhChain20Rem)

    step "validate block"
    _blockResp_otherTransactions resp @?= Nothing
    Just b <- pure $ _blockResp_block resp
    _block_metadata b @?= Nothing
    _blockId_index (_block_blockId b) @?= bhChain20Rem
    _blockId_index (_block_parentBlockId b) @?= (bhChain20Rem - 1)

    case _block_transactions b of
      x:y:_ -> do
        step "check remediation transactions' request keys"
        -- TODO: are these unique across lifetime of blockchain/chains?
        [ycmd] <- twentyChainUpgradeTransactions v cidChain3
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
    {-- NOTE: FastTimedCPM 20 chain remediations occurs on chain 3 and block height 2.
              See Chainweb.Pact.Transactions.UpgradeTransactions and Chainweb.Version. --}
    cidChain3 = unsafeChainId 3
    bhChain20Rem = 2
    nidChain3 = NetworkId
      { _networkId_blockchain = "kadena"
      , _networkId_network = "fastTimedCPM-peterson"
      , _networkId_subNetworkId = Just (SubNetworkId "3" Nothing)
      }
    req h = BlockReq nidChain3 $ PartialBlockId (Just h) Nothing


-- | Rosetta construction endpoints tests (i.e. tx formatting and submission)
-- for a transfer tx.
--
constructionTransferTests :: RosettaTest
constructionTransferTests _ envIo =
  testCaseSchSteps "Construction Flow Tests" $ \step -> do
    cenv <- envIo
    let submitToConstructionAPI' ops cid' res =
          submitToConstructionAPI ops cid' "sender00" getKeys res cenv step

    step "--- TRANSFER TO AN EXISTING ACCOUNT ---"
    void $ do
      let amt = 2.0
          fromAcct = "sender00"
          fromGuard = ks sender00ks
          toAcct = "sender01"
          toGuard = ks sender01ks
          srcChainId = cid
          --srcChainId = unsafeChainId 4
          -- TOOD: non-zero chainId fails with "PollingFailure "polling check failed: {}"
          ops = [ mkOp fromAcct (negate amt) fromGuard 0
                , mkOp toAcct amt toGuard 1 ]
          res = P.PLiteral $ P.LString "Write succeeded"
      -- TODO: check that account balance is expected
      submitToConstructionAPI' ops srcChainId res

    step "--- TRANSFER TO A NEW ACCOUNT ---"
    void $ do
      let amt = 2.0
          fromAcct = "sender01"
          fromGuard = ks sender01ks
          toAcct = "someNewAccount"
          toGuard = ks sender00ks
          ops = [ mkOp fromAcct (negate amt) fromGuard 0
                , mkOp toAcct amt toGuard 1 ]
          res = P.PLiteral $ P.LString "Write succeeded"
      -- TODO: check that account balance is expected
      submitToConstructionAPI' ops cid res

    step "--- TRANSFER TO A k: ACCOUNT ---"
    void $ do
      let netId = nid
            { _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing) }
      step "derive k:account name and ownership"
      let rosettaPubKeySender01 = RosettaPublicKey (fst sender01) CurveEdwards25519
          deriveReq = ConstructionDeriveReq netId rosettaPubKeySender01 Nothing
      (ConstructionDeriveResp _ (Just (AccountId acctAddr _ (Just acctMeta))) _) <-
        constructionDerive cenv deriveReq

      Right (AccountIdMetaData toGuardSender01) <- pure $ extractMetaData acctMeta
      let toAcct = acctAddr
          amt = 2.0
          fromAcct = "sender01"
          fromGuard = ks sender01ks
          ops = [ mkOp fromAcct (negate amt) fromGuard 0
                , mkOp toAcct amt toGuardSender01 1 ]
          res = P.PLiteral $ P.LString "Write succeeded"
      -- TODO: check that account balance is expected   
      submitToConstructionAPI' ops cid res

  where    
    mkOp name delta guard idx =
      operation Successful
                TransferOrCreateAcct
                (P.TxId 0) -- dummy variable
                (toAcctLog name 0.0 delta guard) -- total (0.0) is dummy var
                idx
                []

    toAcctLog name total delta guard = AccountLog
      { _accountLogKey = name
      , _accountLogBalanceTotal = total
      , _accountLogBalanceDelta = BalanceDelta delta
      , _accountLogCurrGuard = A.toJSON guard
      , _accountLogPrevGuard = A.toJSON guard
      }

    ks (TestKeySet _ Nothing pred') = P.mkKeySet [] pred'
    ks (TestKeySet _ (Just (pk,_)) pred') =
      P.mkKeySet [P.PublicKey $ T.encodeUtf8 pk] pred'

    getKeys "sender00" = Just sender00
    getKeys "sender01" = Just sender01
    getKeys "someNewAccount" = Just sender01
    getKeys "anotherNewAccount" = Just sender01
    getKeys "yetAnotherNewAccount" = Just sender00
    getKeys _ = Nothing

submitToConstructionAPI
    :: [Operation]
    -> ChainId
    -> Text
    -> (Text -> Maybe SimpleKeyPair)
    -> P.PactValue
    -> ClientEnv
    -> (String -> IO ())
    -> IO RequestKey
submitToConstructionAPI expectOps chainId' payer getKeys expectResult cenv step = do
  step "preprocess intended operations"
  let preMeta = PreprocessReqMetaData (acct payer) Nothing
      preReq = ConstructionPreprocessReq netId expectOps (Just $! toObject preMeta)
               Nothing Nothing

  (ConstructionPreprocessResp (Just preRespMetaObj) (Just reqAccts)) <-
    constructionPreprocess cenv preReq

  step "feed preprocessed tx into metadata endpoint"
  let opts = preRespMetaObj
      pubKeys = concatMap toRosettaPk reqAccts
      metaReq = ConstructionMetadataReq netId opts (Just pubKeys)

  (ConstructionMetadataResp payloadMeta _) <- constructionMetadata cenv metaReq

  step "feed metadata to get payload"
  let payloadReq = ConstructionPayloadsReq netId expectOps (Just payloadMeta) (Just pubKeys)
  ConstructionPayloadsResp unsigned payloads <- constructionPayloads cenv payloadReq

  step "parse unsigned tx"
  let parseReqUnsigned = ConstructionParseReq netId False unsigned
  _ <- constructionParse cenv parseReqUnsigned

  step "combine tx signatures"
  sigs <- mapM sign payloads
  let combineReq = ConstructionCombineReq netId unsigned sigs
  ConstructionCombineResp signed <- constructionCombine cenv combineReq

  step "parse signed tx"
  let parseReqSigned = ConstructionParseReq netId True signed
  _ <- constructionParse cenv parseReqSigned

  step "get hash (request key) of tx"
  let hshReq = ConstructionHashReq netId signed
  (TransactionIdResp (TransactionId tid) _) <- constructionHash cenv hshReq
  Right rk <- pure $ P.fromText' tid

  step "run tx locally"
  Just (EnrichedCommand cmd _ _) <- pure $ textToEnrichedCommand signed
  crDryRun <- local chainId' cenv cmd
  isCorrectResult rk crDryRun

  step "submit tx to blockchain"
  let submitReq = ConstructionSubmitReq netId signed
  _ <- constructionSubmit cenv submitReq

  step "confirm transaction details via poll"
  PollResponses prs <- polling cid cenv (RequestKeys $ pure rk) ExpectPactResult
  case HM.lookup rk prs of
    Nothing -> assertFailure $ "unable to find poll response for: " <> show rk
    Just cr -> isCorrectResult rk cr

  -- TODO: Check that the intended ops are the ops that occurred

  pure rk

  where
    isCorrectResult rk cr = do
      _crReqKey cr @?= rk
      _crResult cr @?= PactResult (Right expectResult)

    toRosettaPk (AccountId n _ _) = case getKeys n of
      Nothing -> []
      Just (pk,_) -> [ RosettaPublicKey pk CurveEdwards25519 ]

    sign payload = do
      Just a <- pure $ _rosettaSigningPayload_accountIdentifier payload
      Just (pk,sk) <- pure $ getKeys $ _accountId_address a
      Right sk' <- pure $ P.parseB16TextOnly sk
      Right pk' <- pure $ P.parseB16TextOnly pk
      let akps = P.ApiKeyPair (P.PrivBS sk') (Just $ P.PubBS pk')
                 Nothing Nothing Nothing 
      [(kp,_)] <- P.mkKeyPairs [akps]
      (Right (hsh :: P.PactHash)) <- pure $ fmap 
        (P.fromUntypedHash . P.Hash) 
        (P.parseB16TextOnly $ _rosettaSigningPayload_hexBytes payload)
      sig <- P.signHash hsh kp
      
      pure $! RosettaSignature
        { _rosettaSignature_signingPayload = payload
        , _rosettaSignature_publicKey =
            RosettaPublicKey pk CurveEdwards25519
        , _rosettaSignature_signatureType = RosettaEd25519
        , _rosettaSignature_hexBytes = P._usSig sig
        }

    acct n = AccountId n Nothing Nothing
    netId = nid
      { _networkId_subNetworkId = Just (SubNetworkId (chainIdToText chainId') Nothing) }

-- | Rosetta mempool endpoint tests
--
mempoolTests :: RosettaTest
mempoolTests tio envIo = testCaseSchSteps "Mempool Tests" $ \step -> do
    cenv <- envIo
    rkmv <- newEmptyMVar @RequestKeys

    step "execute transfer and wait on mempool data"
    void $! async $ transferOneAsync_ cid tio cenv (putMVar rkmv)
    rk NEL.:| [] <- _rkRequestKeys <$> takeMVar rkmv

    let tid = rkToTransactionId rk
    let test (MempoolResp ts) = return $ elem tid ts

    step "compare requestkey against mempool responses"
    void $! repeatUntil test $ mempool cenv req
  where
    req = NetworkReq nid Nothing

-- | Rosetta network list endpoint tests
--
networkListTests :: RosettaTest
networkListTests _ envIo =
    testCaseSchSteps "Network List Tests" $ \step -> do
      cenv <- envIo

      step "send network list request"
      resp <- networkList cenv req

      for_ (_networkListResp_networkIds resp) $ \n -> do
         _networkId_blockchain n @=? "kadena"
         _networkId_network n @=? "fastTimedCPM-peterson"
         assertBool "chain id of subnetwork is valid"
           $ maybe False (\a -> _subNetworkId_network a `elem` cids)
           $ _networkId_subNetworkId n
  where
    req = MetadataReq Nothing

-- | Rosetta network options tests
--
networkOptionsTests :: RosettaTest
networkOptionsTests _ envIo =
    testCaseSchSteps "Network Options Tests" $ \step -> do
      cenv <- envIo

      step "send network options request"
      resp <- networkOptions cenv req0

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
    testCaseSchSteps "Network Status Tests" $ \step -> do
      cenv <- envIo

      step "send network status request"
      resp0 <- networkStatus cenv req

      step "check status response against genesis"
      genesisId @=? _networkStatusResp_genesisBlockId resp0

      step "send in a transaction and update current block"
      transferOneAsync_ cid tio cenv (void . return)
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

nid :: NetworkId
nid = NetworkId
    { _networkId_blockchain = "kadena"
    , _networkId_network = "fastTimedCPM-peterson"
    , _networkId_subNetworkId = Just (SubNetworkId (chainIdToText cid) Nothing)
    }

genesisId :: BlockId
genesisId = BlockId 0 "gl2bDgfL9ZRJCe0VkGZq8pfCl1PazbfYsSAZNHp8giI"

rosettaVersion :: RosettaNodeVersion
rosettaVersion = RosettaNodeVersion
    { _version_rosettaVersion = "1.4.4"
    , _version_nodeVersion = VERSION_chainweb
    , _version_middlewareVersion = Nothing
    , _version_metadata = Just $ HM.fromList
      [ "node-api-version" A..= ("0.0" :: Text)
      , "chainweb-version" A..= ("fastTimedCPM-peterson" :: Text)
      , "rosetta-chainweb-version" A..= ("1.0.0" :: Text)
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
    acctMeta = Nothing --Just $ _accountIdMetadata _publicKeys _pred'
                       --for fixing ownership rotation bug

-- ------------------------------------------------------------------ --
-- Test Pact Cmds

-- | Build a simple transfer from sender00 to sender01
--
mkTransfer :: ChainId -> IO (Time Micros) -> IO SubmitBatch
mkTransfer sid tio = do
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
      $ set cbChainId sid 
      $ mkCmd ("nonce-transfer-" <> sshow t <> "-" <> sshow n)
      $ mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)"

    modifyIORef' nonceRef (+1)
    return $ SubmitBatch (pure c)

mkKCoinAccount :: ChainId -> IO (Time Micros) -> IO SubmitBatch
mkKCoinAccount sid tio = do
    let kAcct = "k:" <> fst sender00
    t <- toTxCreationTime <$> tio
    n <- readIORef nonceRef
    c <- buildTextCmd
      $ set cbSigners
        [ mkSigner' sender00
          [ mkGasCap ]
        ]
      $ set cbCreationTime t
      $ set cbNetworkId (Just v)
      $ set cbChainId sid
      $ mkCmd ("nonce-transfer-" <> sshow t <> "-" <> sshow n)
      $ mkExec ("(coin.create-account \"" <> kAcct <> "\" (read-keyset \"sender00\"))")
      $ mkKeySetData "sender00" [sender00]

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
    rks <- sending cid cenv batch0
    polling cid cenv rks ExpectPactResult
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
    rks <- sending cid cenv batch0
    polling cid cenv rks ExpectPactResult
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
extractMetadata :: RequestKey -> PollResponses -> IO (HM.HashMap Text A.Value)
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

_accountIdMetadata :: [Text] -> Text -> A.Object
_accountIdMetadata keys p = HM.fromList
  [ "current-ownership" A..= A.object
    [ "pred" A..= p
    , "keys" A..= keys ]]


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
