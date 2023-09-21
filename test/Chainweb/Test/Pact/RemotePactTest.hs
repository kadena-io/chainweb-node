{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.RemotePactTest
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via the Http Pact interface (/send,
-- etc.) (inprocess) API in Chainweb
--
module Chainweb.Test.Pact.RemotePactTest
( tests
, withRequestKeys
, polling
, sending
, PollingExpectation(..)
) where

import Control.Concurrent hiding (modifyMVar, newMVar, putMVar, readMVar)
import Control.Concurrent.MVar.Strict
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import Data.Aeson.Lens hiding (values)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SB
import Data.Word (Word64)
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Numeric.Natural

import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

import qualified Pact.ApiReq as Pact
import qualified Pact.JSON.Encode as J
import Pact.Types.API
import Pact.Types.Capability
import qualified Pact.Types.ChainId as Pact
import qualified Pact.Types.ChainMeta as Pact
import Pact.Types.Command
import Pact.Types.Continuation
import Pact.Types.Exp
import Pact.Types.Gas
import Pact.Types.Hash (Hash(..))
import qualified Pact.Types.PactError as Pact
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term

-- internal modules

import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.Mempool
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Validations (defaultMaxTTL)
import Chainweb.Test.Pact.Utils
import Chainweb.Test.RestAPI.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.Version.Mainnet
import Chainweb.Storage.Table.RocksDB


-- -------------------------------------------------------------------------- --
-- Global Settings

nNodes :: Natural
nNodes = 1

v :: ChainwebVersion
v = fastForkingCpmTestVersion petersonChainGraph

cid :: HasCallStack => ChainId
cid = head . toList $ chainIds v

pactCid :: HasCallStack => Pact.ChainId
pactCid = Pact.ChainId $ chainIdToText cid

gp :: GasPrice
gp = 0.1

withRequestKeys
    :: Pact.TxCreationTime
    -> ClientEnv
    -> IO RequestKeys
withRequestKeys t cenv = do
    mNonce <- newMVar 0
    testSend t mNonce cenv

-- ------------------------------------------------------------------------- --
-- Tests. GHCI use `runRocks tests`
-- also:
-- :set -package retry
-- :set -package extra

-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Pact.RemotePactTest"
    [ withResourceT (withNodesAtLatestBehavior v "remotePactTest-" rdb nNodes) $ \net ->
        withResource' getCurrentTimeIntegral $ \(iotm :: IO (Time Micros)) ->
            let cenv = _getServiceClientEnv <$> net
                iot = toTxCreationTime <$> iotm
            in testGroup "remote pact tests"
                [ withResourceT (liftIO $ join $ withRequestKeys <$> iot <*> cenv) $ \reqkeys -> golden "remote-golden" $
                    join $ responseGolden <$> cenv <*> reqkeys
                , after AllSucceed "remote-golden" $
                    testCaseSteps "remote spv" $ \step ->
                        join $ spvTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "remote-golden" $
                    testCaseSteps "remote eth spv" $ \step ->
                        join $ ethSpvTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "remote spv" $
                    testCaseSteps "/send reports validation failure" $ \step ->
                        join $ sendValidationTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "remote spv" $
                    testCase "/poll reports badlisted txs" $
                        join $ pollingBadlistTest <$> cenv
                , after AllSucceed "remote spv" $
                    testCase "trivialLocalCheck" $
                        join $ localTest <$> iot <*> cenv
                , after AllSucceed "remote spv" $
                    testCase "localChainData" $
                        join $ localChainDataTest <$> iot <*> cenv
                , after AllSucceed "remote spv" $
                    testCaseSteps "transaction size gas tests" $ \step ->
                        join $ txTooBigGasTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "remote spv" $
                    testCaseSteps "genesisAllocations" $ \step ->
                        join $ allocationTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "genesisAllocations" $
                    testCaseSteps "caplist TRANSFER and FUND_TX test" $ \step ->
                        join $ caplistTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "caplist TRANSFER and FUND_TX test" $
                    testCaseSteps "local continuation test" $ \step ->
                        join $ localContTest <$> iot <*> cenv <*> pure step
                , after AllSucceed "caplist TRANSFER and FUND_TX test" $
                    testCaseSteps "poll confirmation depth test" $ \step ->
                        join $ pollingConfirmDepth <$> iot <*> cenv <*> pure step
                , after AllSucceed "local continuation test" $
                    testCaseSteps "/poll rejects keys of incorrect length" $ \step ->
                        join $ pollBadKeyTest <$> cenv <*> pure step
                , after AllSucceed "poll bad key test" $
                    testCaseSteps "local preflight sim test" $ \step ->
                        join $ localPreflightSimTest <$> iot <*> cenv <*> pure step
                ]
    ]

responseGolden :: ClientEnv -> RequestKeys -> IO LBS.ByteString
responseGolden cenv rks = do
    PollResponses theMap <- polling cid cenv rks ExpectPactResult
    let values = mapMaybe (\rk -> _crResult <$> HashMap.lookup rk theMap)
                          (NEL.toList $ _rkRequestKeys rks)
    return $ foldMap J.encode values

localTest :: Pact.TxCreationTime -> ClientEnv -> IO ()
localTest t cenv = do
    mv <- newMVar 0
    SubmitBatch batch <- testBatch t mv gp
    let cmd = head $ toList batch
    res <- local (unsafeChainId 0) cenv cmd
    let PactResult e = _crResult res
    assertEqual "expect /local to return gas for tx" (_crGas res) 5
    assertEqual "expect /local to succeed and return 3" e (Right (PLiteral $ LDecimal 3))

localContTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
localContTest t cenv step = do

    step "execute /send with initial pact continuation tx"
    cmd1 <- firstStep
    rks <- sending cid' cenv (SubmitBatch $ pure cmd1)

    step "check /poll responses to extract pact id for continuation"
    PollResponses m <- polling cid' cenv rks ExpectPactResult
    pid <- case _rkRequestKeys rks of
      rk NEL.:| [] -> maybe (assertFailure "impossible") (return . _pePactId)
        $ HashMap.lookup rk m >>= _crContinuation
      _ -> assertFailure "continuation did not succeed"

    step "execute /local continuation dry run"
    cmd2 <- secondStep pid
    r <- _pactResult . _crResult <$> local cid' cenv cmd2
    case r of
      Left err -> assertFailure (show err)
      Right (PLiteral (LDecimal a)) | a == 2 -> return ()
      Right p -> assertFailure $ "unexpected cont return value: " ++ show p

  where
    cid' = unsafeChainId 0
    tx =
      "(namespace 'free)(module m G (defcap G () true) (defpact p () (step (yield { \"a\" : (+ 1 1) })) (step (resume { \"a\" := a } a))))(free.m.p)"
    firstStep = do
      buildTextCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbCreationTime t
        $ set cbNetworkId (Just v)
        $ set cbGasLimit 70000
        $ mkCmd "nonce-cont-1"
        $ mkExec' tx

    secondStep pid = do
      buildTextCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbCreationTime t
        $ set cbNetworkId (Just v)
        $ mkCmd "nonce-cont-2"
        $ mkCont
        $ mkContMsg pid 1

pollingConfirmDepth :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
pollingConfirmDepth t cenv step = do

    step "/send transactions"
    cmd1 <- firstStep tx
    cmd2 <- firstStep tx'
    rks <- sending cid' cenv (SubmitBatch $ cmd1 NEL.:| [cmd2])

    step "/poll for the transactions until they appear"

    beforePolling <- getCurrentBlockHeight v cenv cid'
    PollResponses m <- pollingWithDepth cid' cenv rks (Just $ ConfirmationDepth 10) ExpectPactResult
    afterPolling <- getCurrentBlockHeight v cenv cid'

    assertBool "there are two command results" $ length (HashMap.keys m) == 2

    -- we are checking that we have waited at least 10 blocks using /poll for the transaction
    assertBool "the difference between heights should be no less than the confirmation depth" $ (afterPolling - beforePolling) >= 10
  where
    cid' = unsafeChainId 0
    tx =
      "42"
    tx' =
      "43"
    firstStep transaction = do
      buildTextCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbCreationTime t
        $ set cbNetworkId (Just v)
        $ set cbGasLimit 70000
        $ mkCmd "nonce-cont-1"
        $ mkExec' transaction

localChainDataTest :: Pact.TxCreationTime -> ClientEnv -> IO ()
localChainDataTest t cenv = do
    mv <- newMVar (0 :: Int)
    SubmitBatch batch <- localTestBatch mv
    let cmd = head $ toList batch
    sid <- mkChainId v maxBound 0
    res <- flip runClientM cenv $ pactLocalApiClient v sid cmd
    checkCommandResult res
  where

    checkCommandResult (Left e) = throwM $ LocalFailure (show e)
    checkCommandResult (Right cr) =
        let (PactResult e) = _crResult cr
        in mapM_ expectedResult e

    localTestBatch mnonce = modifyMVar mnonce $ \(!nn) -> do
        let nonce = "nonce" <> sshow nn
        kps <- testKeyPairs sender00 Nothing
        c <- Pact.mkExec "(chain-data)" A.Null (pm t) kps (Just "fastfork-CPM-peterson") (Just nonce)
        pure (succ nn, SubmitBatch (pure c))
        where
          pm = Pact.PublicMeta pactCid "sender00" 1000 0.1 defaultMaxTTL

    expectedResult (PObject (ObjectMap m)) = do
          assert' "chain-id" (PLiteral (LString $ chainIdToText cid))
          assert' "gas-limit" (PLiteral (LInteger 1000))
          assert' "gas-price" (PLiteral (LDecimal 0.1))
          assert' "sender" (PLiteral (LString "sender00"))
        where
          assert' name value = assertEqual name (M.lookup  (FieldKey (T.pack name)) m) (Just value)
    expectedResult _ = assertFailure "Didn't get back an object map!"

localPreflightSimTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
localPreflightSimTest t cenv step = do
    mv <- newMVar (0 :: Int)
    sid <- mkChainId v maxBound 0
    let sigs = [mkSigner' sender00 []]

    step "Execute preflight /local tx - preflight known /send success"
    let psid = Pact.ChainId $ chainIdToText sid
    psigs <- testKeyPairs sender00 Nothing
    cmd0 <- mkRawTx mv psid psigs
    runLocalPreflightClient sid cenv cmd0 >>= \case
      Left e -> assertFailure $ show e
      Right LocalResultLegacy{} ->
        assertFailure "Preflight /local call produced legacy result"
      Right MetadataValidationFailure{} ->
        assertFailure "Preflight produced an impossible result"
      Right LocalResultWithWarns{} -> pure ()

    step "Execute preflight /local tx - preflight+signoverify known /send success"
    cmd0' <- mkRawTx mv psid psigs
    cr <- runClientM
      (pactLocalWithQueryApiClient v sid
         (Just PreflightSimulation) (Just NoVerify) Nothing cmd0') cenv
    void $ case cr of
      Left e -> assertFailure $ show e
      Right{} -> pure ()

    step "Execute preflight /local tx - unparseable chain id"
    sigs0 <- testKeyPairs sender00 Nothing
    cmd1 <- mkRawTx mv (Pact.ChainId "fail") sigs0
    runClientFailureAssertion sid cenv cmd1 "Unparseable transaction chain id"

    step "Execute preflight /local tx - chain id mismatch"
    let fcid = unsafeChainId maxBound
    cmd2 <- mkTx mv =<< mkCmdBuilder sigs v fcid 1000 gp
    runClientFailureAssertion sid cenv cmd2 "Chain id mismatch"

    step "Execute preflight /local tx - tx gas limit too high"
    cmd3 <- mkTx mv =<< mkCmdBuilder sigs v sid 100000000000000 gp
    runClientFailureAssertion sid cenv cmd3
      "Transaction Gas limit exceeds block gas limit"

    step "Execute preflight /local tx - tx gas price precision too high"
    cmd4 <- mkTx mv =<< mkCmdBuilder sigs v sid 1000 0.00000000000000001
    runClientFailureAssertion sid cenv cmd4
      "Gas price decimal precision too high"

    step "Execute preflight /local tx - network id mismatch"
    cmd5 <- mkTx mv =<< mkCmdBuilder sigs Mainnet01 sid 1000 gp
    runClientFailureAssertion sid cenv cmd5 "Network id mismatch"

    step "Execute preflight /local tx - too many sigs"
    let pcid = Pact.ChainId $ chainIdToText sid
    sigs1' <- testKeyPairs sender00 Nothing >>= \case
      [ks] -> pure $ replicate 101 ks
      _ -> assertFailure "/local test keypair construction failed"

    cmd6 <- mkRawTx mv pcid sigs1'
    runClientFailureAssertion sid cenv cmd6 "Signature list size too big"

    step "Execute preflight /local tx - collect warnings"
    cmd7 <- mkRawTx' mv pcid sigs0 "(+ 1 2.0)"

    currentBlockHeight <- getCurrentBlockHeight v cenv sid
    runLocalPreflightClient sid cenv cmd7 >>= \case
      Left e -> assertFailure $ show e
      Right LocalResultLegacy{} ->
        assertFailure "Preflight /local call produced legacy result"
      Right MetadataValidationFailure{} ->
        assertFailure "Preflight produced an impossible result"
      Right (LocalResultWithWarns cr' ws) -> do
        let crbh :: Integer = fromIntegral $ fromMaybe 0 $ getBlockHeight cr'
            expectedbh = 1 + fromIntegral currentBlockHeight
        assertBool "Preflight's metadata should have increment block height"
          -- we don't control the node in remote tests and the data can get oudated,
          -- to make test less flaky we use a small range for validation
          (abs (expectedbh - crbh) <= 2)

        case ws of
          [w] | "decimal/integer operator overload" `T.isInfixOf` w ->
            pure ()
          ws' -> assertFailure $ "Incorrect warns: " ++ show ws'

    let rewindDepth = 10
    currentBlockHeight' <- getCurrentBlockHeight v cenv sid
    runLocalPreflightClientWithDepth sid cenv cmd7 rewindDepth >>= \case
      Left e -> assertFailure $ show e
      Right LocalResultLegacy{} ->
        assertFailure "Preflight /local call produced legacy result"
      Right MetadataValidationFailure{} ->
        assertFailure "Preflight produced an impossible result"
      Right (LocalResultWithWarns cr' ws) -> do
        let crbh :: Integer = fromIntegral $ fromMaybe 0 $ getBlockHeight cr'
            expectedbh = toInteger $ 1 + (fromIntegral currentBlockHeight') - rewindDepth
        assertBool "Preflight's metadata block height should reflect the rewind depth"
          -- we don't control the node in remote tests and the data can get oudated,
          -- to make test less flaky we use a small range for validation
          (abs (expectedbh - crbh) <= 2)

        case ws of
          [w] | "decimal/integer operator overload" `T.isInfixOf` w ->
            pure ()
          ws' -> assertFailure $ "Incorrect warns: " ++ show ws'
  where
    runLocalPreflightClient sid e cmd = flip runClientM e $
      pactLocalWithQueryApiClient v sid
        (Just PreflightSimulation)
        (Just Verify) Nothing cmd

    runLocalPreflightClientWithDepth sid e cmd d = flip runClientM e $
      pactLocalWithQueryApiClient v sid
        (Just PreflightSimulation)
        (Just Verify) (Just $ RewindDepth d) cmd

    runClientFailureAssertion sid e cmd msg =
      runLocalPreflightClient sid e cmd >>= \case
        Left err -> checkClientErrText err msg
        r -> assertFailure $ "Unintended success: " ++ show r

    checkClientErrText (FailureResponse _ (Response _ _ _ body)) e
      | BS.isInfixOf e $ LBS.toStrict body = pure ()
    checkClientErrText _ e = assertFailure $ show e

    -- cmd builder is more correct by construction than
    -- would allow for us to modify the chain id to something
    -- unparsable. Hence we need to do this in the unparsable
    -- chain id case and nowhere else.
    mkRawTx mv pcid kps = mkRawTx' mv pcid kps "(+ 1 2)"

    mkRawTx' mv pcid kps code = modifyMVar mv $ \(nn :: Int) -> do
      let nonce = "nonce" <> sshow nn
          pm = Pact.PublicMeta pcid "sender00" 1000 0.1 defaultMaxTTL

      c <- Pact.mkExec code A.Null (pm t) kps (Just "fastfork-CPM-peterson") (Just nonce)
      pure (succ nn, c)

    mkCmdBuilder sigs nid pcid limit price = do
      pure
        $ set cbGasLimit limit
        $ set cbGasPrice price
        $ set cbChainId pcid
        $ set cbSigners sigs
        $ set cbCreationTime t
        $ set cbNetworkId (Just nid)
        $ mkCmd ""
        $ mkExec' "(+ 1 2)"

    mkTx mv tx = modifyMVar mv $ \nn -> do
      let n = "nonce-" <> sshow nn
      tx' <- buildTextCmd $ set cbNonce n tx
      pure (succ nn, tx')

pollingBadlistTest :: ClientEnv -> IO ()
pollingBadlistTest cenv = do
    let rks = RequestKeys $ NEL.fromList [pactDeadBeef]
    sid <- mkChainId v maxBound 0
    void $ polling sid cenv rks ExpectPactError

-- | Check request key length validation in the /poll endpoints
--
pollBadKeyTest :: ClientEnv -> (String -> IO ()) -> IO ()
pollBadKeyTest cenv step = do
    let tooBig = toRk $ BS.replicate 33 0x3d
        tooSmall = toRk $ BS.replicate 31 0x3d

    sid <- liftIO $ mkChainId v maxBound 0

    step "RequestKeys of length > 32 fail fast"
    runClientM (pactPollApiClient v sid (Poll tooBig)) cenv >>= \case
      Left _ -> return ()
      Right r -> assertFailure $ "Poll succeeded with response: " <> show r

    step "RequestKeys of length < 32 fail fast"
    runClientM (pactPollApiClient v sid (Poll tooSmall)) cenv >>= \case
      Left _ -> return ()
      Right r -> assertFailure $ "Poll succeeded with response: " <> show r
  where
    toRk = (NEL.:| []) . RequestKey . Hash . SB.toShort

sendValidationTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
sendValidationTest t cenv step = do
        step "check sending poisoned TTL batch"
        mv <- newMVar 0
        SubmitBatch batch1 <-
          testBatch' t 10_000 mv gp
        SubmitBatch batch2 <-
          testBatch' (toTxCreationTime (Time (TimeSpan 0) :: Time Micros)) 2 mv gp
        let batch = SubmitBatch $ batch1 <> batch2
        expectSendFailure "Transaction time is invalid or TTL is expired" $
          flip runClientM cenv $
            pactSendApiClient v cid batch

        step "check sending mismatched chain id"
        cid0 <- mkChainId v maxBound 0
        batch3 <- testBatch'' "40" t 20_000 mv gp
        expectSendFailure "Transaction metadata (chain id, chainweb version) conflicts with this endpoint" $
          flip runClientM cenv $
            pactSendApiClient v cid0 batch3

        step "check insufficient gas"
        batch4 <- testBatch' t 10_000 mv 10_000_000_000
        expectSendFailure
          "Attempt to buy gas failed with: : Failure: Tx Failed: Insufficient funds" $
          flip runClientM cenv $
            pactSendApiClient v cid batch4

        step "check bad sender"
        batch5 <- mkBadGasTxBatch "(+ 1 2)" "invalid-sender" sender00 Nothing
        expectSendFailure
          "Attempt to buy gas failed with: : Failure: Tx Failed: read: row not found: invalid-sender" $
          flip runClientM cenv $
            pactSendApiClient v cid0 batch5

  where
    mkBadGasTxBatch code senderName senderKeyPair capList = do
      ks <- testKeyPairs senderKeyPair capList
      let pm = Pact.PublicMeta (Pact.ChainId "0") senderName 100_000 0.01 defaultMaxTTL t
      let cmd (n :: Int) = liftIO $ Pact.mkExec code A.Null pm ks (Just "fastfork-CPM-peterson") (Just $ sshow n)
      cmds <- mapM cmd (0 NEL.:| [1..5])
      return $ SubmitBatch cmds

expectSendFailure
    :: NFData a
    => NFData b
    => Show a
    => Show b
    => String
    -> IO (Either b a)
    -> Assertion
expectSendFailure expectErr act = tryAllSynchronous act >>= \case
    (Right (Left e)) -> test $ show e
    (Right (Right out)) -> assertFailure $ "expected exception on bad tx, got: " <> show out
    (Left e) -> test $ show e
  where
    test er = assertSatisfies ("Expected message containing '" ++ expectErr ++ "'") er (L.isInfixOf expectErr)

ethSpvTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
ethSpvTest t cenv step = do

    req <- A.eitherDecodeFileStrict' "test/pact/eth-spv-request.json" >>= \case
        Left e -> assertFailure $ "failed to decode test/pact/eth-spv-request: " <> e
        Right x -> return (x :: EthSpvRequest)

    c <- mkChainId v maxBound 1
    r <- flip runClientM cenv $ do

        void $ liftIO $ step "ethSpvApiClient: submit eth proof request"
        proof <- liftIO $ ethSpv c cenv req

        batch <- liftIO $ mkTxBatch proof

        void $ liftIO $ step "sendApiClient: submit batch for proof validation"
        rks <- liftIO $ sending c cenv batch

        void $ liftIO $ step "pollApiClient: poll until key is found"
        void $ liftIO $ polling c cenv rks ExpectPactResult

        return ()

    case r of
        Left e -> assertFailure $ "eth proof roundtrip failed: " <> sshow e
        Right _ -> return ()
  where

    mkTxBatch proof = do
      ks <- liftIO $ testKeyPairs sender00 Nothing
      let pm = Pact.PublicMeta (Pact.ChainId "1") "sender00" 100_000 0.01 defaultMaxTTL t
      cmd <- liftIO $ Pact.mkExec txcode (txdata proof) pm ks (Just "fastfork-CPM-peterson") (Just "1")
      return $ SubmitBatch (pure cmd)

    txcode = "(verify-spv 'ETH (read-msg))"

    txdata proof = A.toJSON proof

spvTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
spvTest t cenv step = do
    batch <- mkTxBatch
    sid <- mkChainId v maxBound 1
    r <- flip runClientM cenv $ do

      void $ liftIO $ step "sendApiClient: submit batch"
      rks <- liftIO $ sending sid cenv batch

      void $ liftIO $ step "pollApiClient: poll until key is found"
      void $ liftIO $ polling sid cenv rks ExpectPactResult

      void $ liftIO $ step "spvApiClient: submit request key"
      liftIO $ spv sid cenv (SpvRequest (NEL.head $ _rkRequestKeys rks) tid)

    case r of
      Left e -> assertFailure $ "output proof failed: " <> sshow e
      Right _ -> return ()
  where
    tid = Pact.ChainId "2"

    mkTxBatch = do
      ks <- liftIO $ testKeyPairs sender00
        (Just [mkGasCap, mkXChainTransferCap "sender00" "sender01" 1.0 "2"])
      let pm = Pact.PublicMeta (Pact.ChainId "1") "sender00" 100_000 0.01 defaultMaxTTL t
      cmd1 <- liftIO $ Pact.mkExec txcode txdata pm ks (Just "fastfork-CPM-peterson") (Just "1")
      cmd2 <- liftIO $ Pact.mkExec txcode txdata pm ks (Just "fastfork-CPM-peterson") (Just "2")
      return $ SubmitBatch (pure cmd1 <> pure cmd2)

    txcode = T.unlines
      [ "(coin.transfer-crosschain"
      , "  'sender00"
      , "  'sender01"
      , "  (read-keyset 'sender01-keyset)"
      , "  (read-msg 'target-chain-id)"
      , "  1.0)"
      ]

    txdata = A.object
        [ "sender01-keyset" A..= [fst sender01]
        , "target-chain-id" A..= J.toJsonViaEncode tid
        ]

txTooBigGasTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
txTooBigGasTest t cenv step = do

    let
      runSend batch expectation = try @IO @PactTestFailure $ do
          void $ step "sendApiClient: submit transaction"
          rks <- sending sid cenv batch

          void $ step "pollApiClient: polling for request key"
          PollResponses resp <- polling sid cenv rks expectation
          return (HashMap.lookup (NEL.head $ _rkRequestKeys rks) resp)

      runLocal (SubmitBatch cmds) = do
          void $ step "localApiClient: submit transaction"
          local sid cenv (head $ toList cmds)

    -- batch with big tx and insufficient gas
    batch0 <- mkTxBatch txcode0 A.Null 1 (Just "0")

    -- batch to test that gas for tx size discounted from the total gas supply
    batch1 <- mkTxBatch txcode1 A.Null 5 (Just "1")

    res0Send <- runSend batch0 ExpectPactError
    res1Send <- runSend batch1 ExpectPactError

    res0Local <- runLocal batch0
    res1Local <- runLocal batch1

    void $ liftIO $ step "when tx too big, gas pact error thrown"
    assertEqual "LOCAL: expect gas error for big tx" gasError0 (Just $ resultOf res0Local)
    case res0Send of
      Left e -> assertFailure $ "test failure for big tx with insufficient gas: " <> show e
      Right cr -> assertEqual "SEND: expect gas error for big tx" gasError0Mem (resultOf <$> cr)

    let getFailureMsg (Left (Pact.PactError _ _ _ m)) = m
        getFailureMsg p = pretty $ "Expected failure result, got " ++ show p

    void $ liftIO $ step "discounts initial gas charge from gas available for pact execution"
    assertEqual "LOCAL: expect gas error after discounting initial gas charge"
      gasError1 (getFailureMsg $ resultOf res1Local)

    case res1Send of
      Left e -> assertFailure $ "test failure for discounting initial gas charge: " <> show e
      Right cr -> assertEqual "SEND: expect gas error after discounting initial gas charge"
        (Just gasError1Mem) (getFailureMsg . resultOf <$> cr)

  where
    sid = unsafeChainId 0
    gasError0 = Just $ Left $
      Pact.PactError Pact.GasError def [] "Tx too big (4), limit 1"
    gasError0Mem = Just $ Left $
      Pact.PactError Pact.TxFailure def [] "Transaction is badlisted because it previously failed to validate."
    gasError1 = "Gas limit (5) exceeded: 6"
    gasError1Mem = "Transaction is badlisted because it previously failed to validate."

    mkTxBatch code cdata limit n = do
      ks <- testKeyPairs sender00 Nothing
      let pm = Pact.PublicMeta (Pact.ChainId "0") "sender00" limit 0.01 defaultMaxTTL t
      cmd <- liftIO $ Pact.mkExec code cdata pm ks (Just "fastfork-CPM-peterson") n
      return $ SubmitBatch (pure cmd)

    txcode0 = T.concat ["[", T.replicate 10 " 1", "]"]
    txcode1 = txcode0 <> "(identity 1)"


caplistTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
caplistTest t cenv step = do
    let testCaseStep = liftIO . step

    r <- flip runClientM cenv $ do
      batch <- liftIO
        $ mkSingletonBatch t sender00 tx0 n0 (pm "sender00") clist

      testCaseStep "send transfer request with caplist sender00 -> sender01"
      rks <- liftIO $ sending sid cenv batch

      testCaseStep "poll for transfer results"
      PollResponses rs <- liftIO $ polling sid cenv rks ExpectPactResult

      return (HashMap.lookup (NEL.head $ _rkRequestKeys rks) rs)

    case r of
      Left e -> assertFailure $ "test failure for TRANSFER + FUND_TX: " <> show e
      Right res -> do
        assertEqual "TRANSFER + FUND_TX test" result0 (resultOf <$> res)
        assertSatisfies "meta in output" (preview (_Just . crMetaData . _Just . _Object . at "blockHash") res) isJust

  where
    sid = unsafeChainId 0
    n0 = Just "transfer-clist0"
    pm sendr = Pact.PublicMeta (Pact.ChainId "0") sendr 100_000 0.01 defaultMaxTTL

    result0 = Just (Right (PLiteral (LString "Write succeeded")))

    clist :: Maybe [SigCapability]
    clist = Just $
      [ mkCoinCap "GAS" []
      , mkCoinCap "TRANSFER"
          [ PLiteral $ LString "sender00"
          , PLiteral $ LString "sender01"
          , PLiteral $ LDecimal 100.0
          ]
      ]

    tx0 = PactTransaction "(coin.transfer \"sender00\" \"sender01\" 100.0)" Nothing



allocation01KeyPair :: SimpleKeyPair
allocation01KeyPair =
    ( "b4c8a3ea91d3146b0560994740f0e3eed91c59d2eeca1dc99f0c2872845c294d"
    , "5dbbbd8b765b7d0cf8426d6992924b057c70a2138ecd4cf60cfcde643f304ea9"
    )

allocation02KeyPair :: SimpleKeyPair
allocation02KeyPair =
    ( "e9e4e71bd063dcf7e06bd5b1a16688897d15ca8bd2e509c453c616219c186cc5"
    , "45f026b7a6bb278ed4099136c13e842cdd80138ab7c5acd4a1f0e6c97d1d1e3c"
    )

allocation02KeyPair' :: SimpleKeyPair
allocation02KeyPair' =
    ( "0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"
    , "2f75b5d875dd7bf07cc1a6973232a9e53dc1d4ffde2bab0bbace65cd87e87f53"
    )

allocationTest :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
allocationTest t cenv step = do

    step "positive allocation test: allocation00 release"
    p <- do
      batch0 <- liftIO
        $ mkSingletonBatch t allocation00KeyPair tx0 n0 (pm "allocation00") Nothing

      SubmitBatch batch1 <- liftIO
        $ mkSingletonBatch t allocation00KeyPair tx1 n1 (pm "allocation00") Nothing
      step "sendApiClient: submit allocation release request"
      rks0 <- liftIO $ sending sid cenv batch0

      step "pollApiClient: polling for allocation key"
      _ <- liftIO $ polling sid cenv rks0 ExpectPactResult

      step "localApiClient: submit local account balance request"
      liftIO $ localTestToRetry sid cenv (head (toList batch1)) (localAfterBlockHeight 4)

    assertEqual "00 expect /local allocation balance" accountInfo (resultOf p)

    step "negative allocation test: allocation01 release"
    do
      batch0 <- mkSingletonBatch t allocation01KeyPair tx2 n2 (pm "allocation01") Nothing

      step "sendApiClient: submit allocation release request"
      cr <- local sid cenv (NEL.head $ _sbCmds batch0)

      case resultOf cr of
        Left e -> do
          assertBool "expect negative allocation test failure"
            $ T.isInfixOf "Failure: Tx Failed: funds locked"
            $ sshow e
        _ -> assertFailure "unexpected pact result success in negative allocation test"

    step "positive key-rotation test: allocation2"
    r <- do

      batch0 <- mkSingletonBatch t allocation02KeyPair tx3 n3 (pm "allocation02") Nothing

      step "senderApiClient: submit keyset rotation request"
      rks <- sending sid cenv batch0

      step "pollApiClient: polling for successful rotation"
      void $ polling sid cenv rks ExpectPactResult

      step "senderApiClient: submit allocation release request"
      batch1 <- mkSingletonBatch t allocation02KeyPair' tx4 n4 (pm "allocation02") Nothing

      rks' <- sending sid cenv batch1
      step "pollingApiClient: polling for successful release"
      pr <- polling sid cenv rks' ExpectPactResult

      step "localApiClient: retrieving account info for allocation02"
      SubmitBatch batch2 <- mkSingletonBatch t allocation02KeyPair' tx5 n5 (pm "allocation02") Nothing

      localTestToRetry sid cenv (head (toList batch2)) (localAfterPollResponse pr)

    assertEqual "02 expect /local allocation balance" accountInfo' (resultOf r)

  where
    n0 = Just "allocation-0"
    n1 = Just "allocation-1"
    n2 = Just "allocation-2"
    n3 = Just "allocation-3"
    n4 = Just "allocation-4"
    n5 = Just "allocation-5"

    sid = unsafeChainId 0

    localAfterPollResponse (PollResponses prs) cr =
        getBlockHeight cr > getBlockHeight (snd $ head $ HashMap.toList prs)

    localAfterBlockHeight bh cr =
      getBlockHeight cr > Just bh

    accountInfo = Right
      $ PObject
      $ ObjectMap
      $ M.fromList
        [ (FieldKey "account", PLiteral $ LString "allocation00")
        , (FieldKey "balance", PLiteral $ LDecimal 1_099_993.89) -- balance = (1k + 1mm) - gas
        , (FieldKey "guard", PGuard $ GKeySetRef (KeySetName "allocation00" Nothing))
        ]

    pm sendr = Pact.PublicMeta (Pact.ChainId "0") sendr 100_000 0.01 defaultMaxTTL

    tx0 = PactTransaction "(coin.release-allocation \"allocation00\")" Nothing
    tx1 = PactTransaction "(coin.details \"allocation00\")" Nothing
    tx2 = PactTransaction "(coin.release-allocation \"allocation01\")" Nothing
    tx3 =
      let
        c = "(define-keyset \"allocation02\" (read-keyset \"allocation02-keyset\"))"
        d = mkKeySet
          ["0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"]
          "keys-all"
      in PactTransaction c $ Just (A.object [ "allocation02-keyset" A..= J.toJsonViaEncode d ])
    tx4 = PactTransaction "(coin.release-allocation \"allocation02\")" Nothing
    tx5 = PactTransaction "(coin.details \"allocation02\")" Nothing

    accountInfo' = Right
      $ PObject
      $ ObjectMap
      $ M.fromList
        [ (FieldKey "account", PLiteral $ LString "allocation02")
        , (FieldKey "balance", PLiteral $ LDecimal 1_099_991) -- 1k + 1mm - gas
        , (FieldKey "guard", PGuard $ GKeySetRef (KeySetName "allocation02" Nothing))
        ]


-- -------------------------------------------------------------------------- --
-- Utils


data PactTransaction = PactTransaction
  { _pactCode :: Text
  , _pactData :: Maybe A.Value
  } deriving (Eq, Show)

resultOf :: CommandResult l -> Either Pact.PactError PactValue
resultOf = _pactResult . _crResult

mkSingletonBatch
    :: Pact.TxCreationTime
    -> SimpleKeyPair
    -> PactTransaction
    -> Maybe Text
    -> (Pact.TxCreationTime -> Pact.PublicMeta)
    -> Maybe [SigCapability]
    -> IO SubmitBatch
mkSingletonBatch t kps (PactTransaction c d) nonce pmk clist = do
    ks <- testKeyPairs kps clist
    let dd = fromMaybe A.Null d
    cmd <- liftIO $ Pact.mkExec c dd (pmk t) ks (Just "fastfork-CPM-peterson") nonce
    return $ SubmitBatch (cmd NEL.:| [])

testSend :: Pact.TxCreationTime -> MVar Int -> ClientEnv -> IO RequestKeys
testSend t mNonce env = testBatch t mNonce gp >>= sending cid env

testBatch'' :: Pact.ChainId -> Pact.TxCreationTime -> Pact.TTLSeconds -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch'' chain t ttl mnonce gp' = modifyMVar mnonce $ \(!nn) -> do
    let nonce = "nonce" <> sshow nn
    kps <- testKeyPairs sender00 Nothing
    c <- Pact.mkExec "(+ 1 2)" A.Null (pm t) kps (Just "fastfork-CPM-peterson") (Just nonce)
    pure (succ nn, SubmitBatch (pure c))
  where
    pm :: Pact.TxCreationTime -> Pact.PublicMeta
    pm = Pact.PublicMeta chain "sender00" 1000 gp' ttl

testBatch' :: Pact.TxCreationTime -> Pact.TTLSeconds -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch' = testBatch'' pactCid

testBatch :: Pact.TxCreationTime -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch t mnonce = testBatch' t defaultMaxTTL mnonce

pactDeadBeef :: RequestKey
pactDeadBeef = let (TransactionHash b) = deadbeef
               in RequestKey $ Hash b

-- avoiding `scientific` dep here
getBlockHeight :: CommandResult a -> Maybe Word64
getBlockHeight = preview (crMetaData . _Just . key "blockHeight" . _Number . to ((fromIntegral :: Integer -> Word64 ) . round . toRational))
