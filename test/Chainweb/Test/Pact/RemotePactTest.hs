{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Bifunctor (first)
import Control.Monad.Trans.Except (runExceptT, except)
import Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SB
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Word (Word64)
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Logger.Types (LogLevel(..))

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
import Pact.Types.Persistence (RowKey(..), TxLog(..))
import Pact.Types.RowData (RowData(..))
import Pact.Types.Term

-- internal modules

import Chainweb.ChainId
import Chainweb.Graph
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.Utils qualified as Backend
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.RestAPI.EthSpv
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Validations (defaultMaxTTL)
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Pact.Utils qualified as Utils
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

nNodes :: Word
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
                pactDir = do
                  m <- _getNodeDbDirs <$> net
                  -- This looks up the pactDbDir for node 0. This is
                  -- kind of a hack, because there is only one node in
                  -- this test. However, it doesn't matter much, because
                  -- we are dealing with both submitting /local txs
                  -- and compaction, so picking an arbitrary node
                  -- to run these two operations on is fine.
                  pure (fst (head m))

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
                    testCase "txlogsCompactionTest" $
                        join $ txlogsCompactionTest <$> iot <*> cenv <*> pactDir
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
                , after AllSucceed "poll returns correct results" $
                    testCaseSteps "poll correct results test" $ \step ->
                        join $ pollingCorrectResults <$> iot <*> cenv <*> pure step
                , after AllSucceed "webauthn signatures" $ testCase "webauthn sig" $ join $ webAuthnSignatureTest <$> iot <*> cenv
                ]
    ]

responseGolden :: ClientEnv -> RequestKeys -> IO LBS.ByteString
responseGolden cenv rks = do
    PollResponses theMap <- polling cid cenv rks ExpectPactResult
    let values = mapMaybe (\rk -> _crResult <$> HM.lookup rk theMap)
                          (NEL.toList $ _rkRequestKeys rks)
    return $ foldMap J.encode values

-- | Check that txlogs don't problematically access history
--   post-compaction.
--
--   At a high level, the test does this:
--     - Submits a tx that creates a module with a table named `persons`.
--
--       This module exposes a few functions for reading, inserting,
--       and overwriting rows to the `persons` table.
--
--       The tx also inserts some people into `persons` for
--       some initial state.
--
--       This module also exposes a way to access the `txlogs`
--       of the `persons` table (what this test is concerned with).
--
--     - Submits a tx that overwrites a row in the
--       `persons` table.
--
--     - Compacts to the latest blockheight on each chain. This should
--       get rid of any out-of-date rows.
--
--     - Submits a /local tx that reads the `txlogs` on the `persons`
--       table. Call this `txLogs`.
--
--       If this read fails, Pact is doing something problematic!
--
--       If this read doesn't fail, we need to check that `txLogs`
--       matches the latest pact state post-compaction. Because
--       compaction sweeps away the out-of-date rows, they shouldn't
--       appear in the `txLogs` anymore, and the two should be equivalent.
txlogsCompactionTest :: Pact.TxCreationTime -> ClientEnv -> FilePath -> IO ()
txlogsCompactionTest t cenv pactDbDir = do
    let cmd :: Text -> Text -> CmdBuilder
        cmd nonce tx = do
          set cbSigners [mkEd25519Signer' sender00 []]
            $ set cbTTL defaultMaxTTL
            $ set cbCreationTime t
            $ set cbChainId cid
            $ mkCmd nonce
            $ mkExec tx
            $ mkKeySetData "sender00" [sender00]

    createTableTx <- buildTextCmd v
      $ set cbGasLimit 300_000
      $ cmd "create-table-persons"
      $ T.unlines
          [ "(namespace 'free)"
          , "(module m0 G"
          , "  (defcap G () true)"
          , "  (defschema person"
          , "    name:string"
          , "    age:integer"
          , "  )"
          , "  (deftable persons:{person})"
          , "  (defun read-persons (k) (read persons k))"
          , "  (defun insert-persons (id name age) (insert persons id { 'name:name, 'age:age }))"
          , "  (defun write-persons (id name age) (write persons id { 'name:name, 'age:age }))"
          , "  (defun persons-txlogs (i) (map (txlog persons) (txids persons i)))"
          , ")"
          , "(create-table persons)"
          , "(insert-persons \"A\" \"Lindsey Lohan\" 42)"
          , "(insert-persons \"B\" \"Nico Robin\" 30)"
          , "(insert-persons \"C\" \"chessai\" 420)"
          ]

    nonceSupply <- newIORef @Word 1 -- starts at 1 since 0 is always the create-table tx
    let nextNonce = do
          cur <- readIORef nonceSupply
          modifyIORef' nonceSupply (+ 1)
          pure cur

    let submitAndCheckTx tx = do
          submitResult <- flip runClientM cenv $
            pactSendApiClient v cid $ SubmitBatch $ NEL.fromList [tx]
          case submitResult of
            Left err -> do
              assertFailure $ "Error when sending tx: " ++ show err
            Right rks -> do
              PollResponses m <- polling cid cenv rks ExpectPactResult
              case HM.lookup (NEL.head (_rkRequestKeys rks)) m of
                Just cr -> do
                  case _crResult cr of
                    PactResult (Left err) -> do
                      assertFailure $ "validation failure on tx: " ++ show err
                    PactResult _ -> do
                      pure ()
                Nothing -> do
                  assertFailure "impossible"

    submitAndCheckTx createTableTx

    let getLatestState :: IO (M.Map RowKey RowData)
        getLatestState = C.withDefaultLogger Error $ \logger -> do
          Backend.withSqliteDb cid logger pactDbDir False $ \(SQLiteEnv db _) -> do
            st <- Utils.getLatestPactState db
            case M.lookup "free.m0_persons" st of
              Just ps -> fmap M.fromList $ forM (M.toList ps) $ \(rkBytes, rdBytes) -> do
                let rk = RowKey (T.decodeUtf8 rkBytes)
                case A.eitherDecodeStrict' @RowData rdBytes of
                  Left err -> do
                    assertFailure $ "Failed decoding rowdata: " ++ err
                  Right rd -> do
                    pure (rk, rd)
              Nothing -> error "getting state of free.m0_persons failed"

    let createTxLogsTx :: Word -> IO (Command Text)
        createTxLogsTx n = do
          -- cost is about 360k.
          -- cost = flatCost(module) + flatCost(map) + flatCost(txIds) + numTxIds * (costOf(txlog)) + C
          --      = 60_000 + 4 + 100_000 + 2 * 100_000 + C
          --      = 360_004 + C
          -- Note there are two transactions that write to `persons`, which is
          -- why `numTxIds` = 2 (and not the number of rows).
          let gasLimit = 400_000
          buildTextCmd v
            $ set cbGasLimit gasLimit
            $ cmd ("test-txlogs-" <> sshow n)
            $ T.unlines
                [ "(namespace 'free)"
                , "(module m" <> sshow n <> " G"
                , "  (defcap G () true)"
                , "  (defun persons-txlogs (i) (m0.persons-txlogs i))"
                , ")"
                , "(persons-txlogs 0)"
                ]

    let createWriteTx :: Word -> IO (Command Text)
        createWriteTx n = do
          -- module = 60k, write = 100
          let gasLimit = 70_000
          buildTextCmd v
            $ set cbGasLimit gasLimit
            $ cmd ("test-write-" <> sshow n)
            $ T.unlines
                [ "(namespace 'free)"
                , "(module m" <> sshow n <> " G"
                , "  (defcap G () true)"
                , "  (defun test-write (id name age) (m0.write-persons id name age))"
                , ")"
                , "(test-write \"C\" \"chessai\" 69)"
                ]

    let -- This can't be a Map because the RowKeys aren't
        -- necessarily unique, unlike in `getLatestPactState`.
        crGetTxLogs :: CommandResult Hash -> IO [(RowKey, A.Value)]
        crGetTxLogs cr = do
          e <- runExceptT $ do
            pv0 <- except (first show (_pactResult (_crResult cr)))
            case pv0 of
              PList arr -> do
                fmap concat $ forM arr $ \pv -> do
                  txLogs <- except (A.eitherDecode @[TxLog A.Value] (J.encode pv))
                  pure $ flip map txLogs $ \txLog ->
                    (RowKey (_txKey txLog), _txValue txLog)
              _ -> do
                throwError "expected outermost PList when decoding TxLogs"
          case e of
            Left err -> do
              assertFailure $ "crGetTxLogs failed: " ++ err
            Right txlogs -> do
              pure txlogs

    submitAndCheckTx =<< createWriteTx =<< nextNonce

    C.withDefaultLogger Error $ \logger -> do
      let flags = [C.NoVacuum, C.NoGrandHash]
      let resetDb = False

      Backend.withSqliteDb cid logger pactDbDir resetDb $ \(SQLiteEnv db _) -> do
        void $ C.compact C.Latest logger db flags

    txLogs <- crGetTxLogs =<< local cid cenv =<< createTxLogsTx =<< nextNonce

    latestState <- getLatestState
    assertEqual
      "txlogs match latest state"
      txLogs
      (map (\(rk, rd) -> (rk, J.toJsonViaEncode (_rdData rd))) (M.toList latestState))

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
        $ HM.lookup rk m >>= _crContinuation
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
      buildTextCmd v
        $ set cbSigners [mkEd25519Signer' sender00 []]
        $ set cbCreationTime t
        $ set cbGasLimit 70000
        $ mkCmd "nonce-cont-1"
        $ mkExec' tx

    secondStep pid = do
      buildTextCmd v
        $ set cbSigners [mkEd25519Signer' sender00 []]
        $ set cbCreationTime t
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

    assertBool "there are two command results" $ length (HM.keys m) == 2

    -- we are checking that we have waited at least 10 blocks using /poll for the transaction
    assertBool "the difference between heights should be no less than the confirmation depth" $ (afterPolling - beforePolling) >= 10
  where
    cid' = unsafeChainId 0
    tx =
      "42"
    tx' =
      "43"
    firstStep transaction = do
      buildTextCmd v
        $ set cbSigners [mkEd25519Signer' sender00 []]
        $ set cbCreationTime t
        $ set cbGasLimit 70000
        $ mkCmd "nonce-cont-1"
        $ mkExec' transaction

pollingCorrectResults :: Pact.TxCreationTime -> ClientEnv -> (String -> IO ()) -> IO ()
pollingCorrectResults t cenv step = do
    step "/send transactions"

    -- submit the first one, then poll to confirm its in a block
    cmd1 <- stepTx tx
    rks1@(RequestKeys (rk1 NEL.:| [])) <- sending cid' cenv (SubmitBatch $ cmd1 NEL.:| [])
    PollResponses _ <- pollingWithDepth cid' cenv rks1 (Just $ ConfirmationDepth 10) ExpectPactResult

    -- submit the second...
    cmd2 <- stepTx tx'
    RequestKeys (rk2 NEL.:| []) <- sending cid' cenv (SubmitBatch $ cmd2 NEL.:| [])

    -- now request both. the second transaction will by definition go into another block.
    -- do it in two different orders, and ensure it works either way.
    let
      together1 = RequestKeys $ rk1 NEL.:| [rk2]
      together2 = RequestKeys $ rk2 NEL.:| [rk1]

    PollResponses resp1 <- polling cid' cenv together1 ExpectPactResult
    PollResponses resp2 <- polling cid' cenv together2 ExpectPactResult

    assertEqual "the two responses should be the same" resp1 resp2
  where
    cid' = unsafeChainId 0
    (tx, tx')  = ("42", "43")

    stepTx transaction = do
      buildTextCmd v
        $ set cbSigners [mkEd25519Signer' sender00 []]
        $ set cbCreationTime t
        $ set cbGasLimit 70_000
        $ mkCmd "nonce-cont-2"
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
    let sigs = [mkEd25519Signer' sender00 []]

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
    cmd2 <- mkTx v mv =<< mkCmdBuilder sigs fcid 1000 gp
    runClientFailureAssertion sid cenv cmd2 "Chain id mismatch"

    step "Execute preflight /local tx - tx gas limit too high"
    cmd3 <- mkTx v mv =<< mkCmdBuilder sigs sid 100000000000000 gp
    runClientFailureAssertion sid cenv cmd3
      "Transaction Gas limit exceeds block gas limit"

    step "Execute preflight /local tx - tx gas price precision too high"
    cmd4 <- mkTx v mv =<< mkCmdBuilder sigs sid 1000 0.00000000000000001
    runClientFailureAssertion sid cenv cmd4
      "Gas price decimal precision too high"

    step "Execute preflight /local tx - network id mismatch"
    cmd5 <- mkTx Mainnet01 mv =<< mkCmdBuilder sigs sid 1000 gp
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
        let crbh :: Integer = fromIntegral $ fromMaybe 0 $ crGetBlockHeight cr'
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
        let crbh :: Integer = fromIntegral $ fromMaybe 0 $ crGetBlockHeight cr'
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

    mkCmdBuilder sigs pcid limit price = do
      pure
        $ set cbGasLimit limit
        $ set cbGasPrice price
        $ set cbChainId pcid
        $ set cbSigners sigs
        $ set cbCreationTime t
        $ mkCmd ""
        $ mkExec' "(+ 1 2)"

    mkTx v' mv tx = modifyMVar mv $ \nn -> do
      let n = "nonce-" <> sshow nn
      tx' <- buildTextCmd v' $ set cbNonce n tx
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
          return (HM.lookup (NEL.head $ _rkRequestKeys rks) resp)

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

      return (HM.lookup (NEL.head $ _rkRequestKeys rks) rs)

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
        crGetBlockHeight cr > crGetBlockHeight (snd $ head $ HM.toList prs)

    localAfterBlockHeight bh cr =
      crGetBlockHeight cr > Just bh

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

-- Test that transactions signed with (mock) WebAuthn keypairs are accepted
-- by the pact service.
webAuthnSignatureTest :: Pact.TxCreationTime -> ClientEnv -> IO ()
webAuthnSignatureTest t cenv = do

  cmd1 <- buildTextCmd v
    $ set cbSigners [mkWebAuthnSigner' sender02WebAuthn [], mkEd25519Signer' sender00 []]
    $ set cbCreationTime t
    $ set cbGasLimit 1000
    $ mkCmd "nonce-webauthn-1"
    $ mkExec' "(concat [\"chainweb-\" \"node\"])"

  rks1 <- sending cid' cenv (SubmitBatch $ pure cmd1)
  PollResponses _resp1 <- polling cid' cenv rks1 ExpectPactResult

  cmd2 <- buildTextCmd v
    $ set cbSigners [mkWebAuthnSigner' sender02WebAuthn [], mkEd25519Signer' sender00 []]
    $ set cbCreationTime t
    $ set cbGasLimit 1000
    $ mkCmd "nonce-webauthn-2"
    $ mkExec' "(concat [\"chainweb-\" \"node\"])"


  rks2 <- sending cid' cenv (SubmitBatch $ pure cmd2)
  PollResponses _resp2 <- polling cid' cenv rks2 ExpectPactResult

  return ()

  where
    cid' = unsafeChainId 0



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
crGetBlockHeight :: CommandResult a -> Maybe Word64
crGetBlockHeight = preview (crMetaData . _Just . key "blockHeight" . _Number . to ((fromIntegral :: Integer -> Word64 ) . round . toRational))
