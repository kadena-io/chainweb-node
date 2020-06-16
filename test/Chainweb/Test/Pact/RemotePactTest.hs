{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
, withNodes
, withRequestKeys
, polling
, sending
, PollingExpectation(..)
, ChainwebNetwork(..)
) where

import Control.Concurrent hiding (modifyMVar, newMVar, putMVar, readMVar)
import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry

import qualified Data.Aeson as A
import Data.Aeson.Lens hiding (values)
import qualified Data.ByteString.Short as SB
import Data.Decimal
import Data.Default (def)
import Data.Either
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Streaming.Network (HostPreference)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import NeatInterpolation

import Network.Connection as HTTP
import Network.HTTP.Client.TLS as HTTP

import Numeric.Natural

import Servant.Client

import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

import qualified Pact.ApiReq as Pact
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

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Chainweb.ChainResources
import Chainweb.Chainweb.PeerResources
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Graph
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Config
import Chainweb.Miner.Pact (noMiner)
import Chainweb.NodeId
import Chainweb.Pact.RestAPI.Client
import Chainweb.Pact.Service.Types
import Chainweb.Test.P2P.Peer.BootstrapConfig
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Data.CAS.RocksDB

import P2P.Node.Configuration
import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Global Settings

debug :: String -> IO ()
#if DEBUG_TEST
debug = putStrLn
#else
debug = const $ return ()
#endif

nNodes :: Natural
nNodes = 1

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

cid :: HasCallStack => ChainId
cid = head . toList $ chainIds v

pactCid :: HasCallStack => Pact.ChainId
pactCid = Pact.ChainId $ chainIdToText cid

gp :: GasPrice
gp = 0.1

-- ------------------------------------------------------------------------- --
-- Tests. GHCI use `runSchedRocks tests`

-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: RocksDb -> ScheduledTest
tests rdb = testGroupSch "Chainweb.Test.Pact.RemotePactTest"
    [ withNodes rdb nNodes $ \net ->
        withMVarResource 0 $ \iomvar ->
          withTime $ \iot ->
            testGroup "remote pact tests"
              [ testCase "await network" $
                awaitNetworkHeight net 20
              , after AllSucceed "await network" $
                withRequestKeys iot iomvar net $ responseGolden net
              , after AllSucceed "remote-golden" $
                testGroup "remote spv" [spvTest iot net]
              , after AllSucceed "remote spv" $
                sendValidationTest iot net
              , after AllSucceed "remote spv" $
                pollingBadlistTest net
              , after AllSucceed "remote spv" $
                testCase "trivialLocalCheck" $
                localTest iot net
              , after AllSucceed "remote spv" $
                testCase "localChainData" $
                localChainDataTest iot net
              , after AllSucceed "remote spv" $
                testGroup "gasForTxSize"
                [ txTooBigGasTest iot net ]
              , after AllSucceed "remote spv" $
                testGroup "genesisAllocations"
                [ allocationTest iot net ]
              , after AllSucceed "genesis allocations" $
                testGroup "caplistTests"
                [ caplistTest iot net ]
              , after AllSucceed "caplistTests" $
                testCase "local continuations" $
                localContTest iot net
              ]
    ]

-- | Network initialization takes some time. Within my ghci session it took
-- about 10 seconds. Once initialization is complete even large numbers of empty
-- blocks were mined almost instantaneously.
--
awaitNetworkHeight :: IO ChainwebNetwork -> CutHeight -> IO ()
awaitNetworkHeight nio h = do
    cenv <- _getClientEnv <$> nio
    ch <- awaitCutHeight cenv h
    debug $ "cut height: " <> sshow (_cutHashesHeight ch)

responseGolden :: IO ChainwebNetwork -> IO RequestKeys -> TestTree
responseGolden networkIO rksIO = golden "remote-golden" $ do
    rks <- rksIO
    cenv <- _getClientEnv <$> networkIO
    PollResponses theMap <- polling cid cenv rks ExpectPactResult
    let values = mapMaybe (\rk -> _crResult <$> HashMap.lookup rk theMap)
                          (NEL.toList $ _rkRequestKeys rks)
    return $! toS $! foldMap A.encode values

localTest :: IO (Time Micros) -> IO ChainwebNetwork -> IO ()
localTest iot nio = do
    cenv <- fmap _getClientEnv nio
    mv <- newMVar 0
    SubmitBatch batch <- testBatch iot mv gp
    let cmd = head $ toList batch
    res <- local (unsafeChainId 0) cenv cmd
    let (PactResult e) = _crResult res
    assertEqual "expect /local to return gas for tx" (_crGas res) 5
    assertEqual "expect /local to succeed and return 3" e (Right (PLiteral $ LDecimal 3))

localContTest :: IO (Time Micros) -> IO ChainwebNetwork -> IO ()
localContTest iot nio = do
    cenv <- _getClientEnv <$> nio
    mv <- newMVar 0
    SubmitBatch (cmd NEL.:| []) <- firstStep mv

    pid <- (_crContinuation <$> local (unsafeChainId 0) cenv cmd) >>= \case
      Nothing -> assertFailure "failed to initialize continuation"
      Just pe -> return $ _pePactId pe

    SubmitBatch batch2 <- secondStep mv pid
    let cmd3 = head $ toList batch2
    PactResult e <- _crResult <$> local (unsafeChainId 0) cenv cmd3
    case e of
      Left err -> assertFailure (show err)
      Right _ -> return ()
  where
    tc =
      "(namespace 'free)(module m G (defcap G () true) (defpact p () (step (yield { \"a\" : (+ 1 1) })) (step (resume { \"a\" := a } a))))(free.m.p)"

    ttl = 2 * 24 * 60 * 60
    pm = Pact.PublicMeta (Pact.ChainId "0") "sender00" 100_000 0.01 ttl

    firstStep mv = modifyMVar mv $ \ !n -> do
      t <- toTxCreationTime <$> iot
      kps <- testKeyPairs sender00 Nothing
      c <- Pact.mkExec tc A.Null (pm t) kps (Just "fastTimedCPM-peterson") Nothing
      return (succ n, SubmitBatch $ pure c)

    secondStep mv pid = do
      nonce <- (<>) "nonce" . sshow @Natural <$> readMVar mv
      t <- toTxCreationTime <$> iot
      kps <- testKeyPairs sender00 Nothing
      c <- Pact.mkCont pid 1 False A.Null (pm t) kps (Just nonce) Nothing (Just "fastTimedCPM-peterson")
      return $ SubmitBatch (pure c)

localChainDataTest :: IO (Time Micros) -> IO ChainwebNetwork -> IO ()
localChainDataTest iot nio = do
    cenv <- fmap _getClientEnv nio
    mv <- newMVar (0 :: Int)
    SubmitBatch batch <- localTestBatch iot mv
    let cmd = head $ toList batch
    sid <- mkChainId v maxBound (0 :: Int)
    res <- flip runClientM cenv $ pactLocalApiClient v sid cmd
    checkCommandResult res
  where

    checkCommandResult (Left e) = throwM $ LocalFailure (show e)
    checkCommandResult (Right cr) =
        let (PactResult e) = _crResult cr
        in mapM_ expectedResult e

    localTestBatch iott mnonce = modifyMVar mnonce $ \(!nn) -> do
        let nonce = "nonce" <> sshow nn
        t <- toTxCreationTime <$> iott
        kps <- testKeyPairs sender00 Nothing
        c <- Pact.mkExec "(chain-data)" A.Null (pm t) kps (Just "fastTimedCPM-peterson") (Just nonce)
        pure (succ nn, SubmitBatch (pure c))
        where
          ttl = 2 * 24 * 60 * 60
          pm = Pact.PublicMeta pactCid "sender00" 1000 0.1 (fromInteger ttl)

    expectedResult (PObject (ObjectMap m)) = do
          assert' "chain-id" (PLiteral (LString "8"))
          assert' "gas-limit" (PLiteral (LInteger 1000))
          assert' "gas-price" (PLiteral (LDecimal 0.1))
          assert' "sender" (PLiteral (LString "sender00"))
        where
          assert' name value = assertEqual name (M.lookup  (FieldKey (toS name)) m) (Just value)
    expectedResult _ = assertFailure "Didn't get back an object map!"

pollingBadlistTest :: IO ChainwebNetwork -> TestTree
pollingBadlistTest nio = testCase "/poll reports badlisted txs" $ do
    cenv <- fmap _getClientEnv nio
    let rks = RequestKeys $ NEL.fromList [pactDeadBeef]
    sid <- liftIO $ mkChainId v maxBound (0 :: Int)
    void $ polling sid cenv rks ExpectPactError


sendValidationTest :: IO (Time Micros) -> IO ChainwebNetwork -> TestTree
sendValidationTest iot nio =
    testCaseSteps "/send reports validation failure" $ \step -> do
        step "check sending poisoned TTL batch"
        cenv <- fmap _getClientEnv nio
        mv <- newMVar 0
        SubmitBatch batch1 <- testBatch' iot 10_000 mv gp
        SubmitBatch batch2 <- testBatch' (return $ Time $ TimeSpan 0) 2 mv gp
        let batch = SubmitBatch $ batch1 <> batch2
        expectSendFailure "Transaction time is invalid or TTL is expired" $
          flip runClientM cenv $
            pactSendApiClient v cid batch

        step "check sending mismatched chain id"
        cid0 <- mkChainId v maxBound (0 :: Int)
        batch3 <- testBatch'' "40" iot 20_000 mv gp
        expectSendFailure "Transaction metadata (chain id, chainweb version) conflicts with this endpoint" $
          flip runClientM cenv $
            pactSendApiClient v cid0 batch3

        step "check insufficient gas"
        batch4 <- testBatch' iot 10_000 mv 10_000_000_000
        expectSendFailure
          "(enforce (<= amount balance) \\\"...: Failure: Tx Failed: Insufficient funds\"" $
          flip runClientM cenv $
            pactSendApiClient v cid batch4

        step "check bad sender"
        batch5 <- mkBadGasTxBatch "(+ 1 2)" "invalid-sender" sender00 Nothing
        expectSendFailure
          "(read coin-table sender): Failure: Tx Failed: read: row not found: invalid-sender" $
          flip runClientM cenv $
            pactSendApiClient v cid0 batch5

  where
    mkBadGasTxBatch code senderName senderKeyPair capList = do
      ks <- testKeyPairs senderKeyPair capList
      t <- toTxCreationTime <$> iot
      let ttl = 2 * 24 * 60 * 60
          pm = Pact.PublicMeta (Pact.ChainId "0") senderName 100_000 0.01 ttl t
      let cmd (n :: Int) = liftIO $ Pact.mkExec code A.Null pm ks (Just "fastTimedCPM-peterson") (Just $ sshow n)
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


spvTest :: IO (Time Micros) -> IO ChainwebNetwork -> TestTree
spvTest iot nio = testCaseSteps "spv client tests" $ \step -> do
    cenv <- fmap _getClientEnv nio
    batch <- mkTxBatch
    sid <- mkChainId v maxBound (1 :: Int)
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
    ttl = 2 * 24 * 60 * 60

    mkTxBatch = do
      ks <- liftIO $ testKeyPairs sender00 Nothing
      t <- toTxCreationTime <$> iot
      let pm = Pact.PublicMeta (Pact.ChainId "1") "sender00" 100_000 0.01 ttl t
      cmd1 <- liftIO $ Pact.mkExec txcode txdata pm ks (Just "fastTimedCPM-peterson") (Just "1")
      cmd2 <- liftIO $ Pact.mkExec txcode txdata pm ks (Just "fastTimedCPM-peterson") (Just "2")
      return $ SubmitBatch (pure cmd1 <> pure cmd2)

    txcode = show
      [text|
         (coin.cross-chain-transfer
           'sender00
           (read-msg 'target-chain-id)
           'sender00
           (read-keyset 'sender00-keyset)
           1.0)
         |]

    txdata =
      -- sender00 keyset
      let ks = mkKeySet
            ["6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"]
            "keys-all"
      in A.object
        [ "sender01-keyset" A..= ks
        , "target-chain-id" A..= tid
        ]

txTooBigGasTest :: IO (Time Micros) -> IO ChainwebNetwork -> TestTree
txTooBigGasTest iot nio = testCaseSteps "transaction size gas tests" $ \step -> do
    cenv <- fmap _getClientEnv nio
    sid <- mkChainId v maxBound (0 :: Int)

    let runSend batch expectation = flip runClientM cenv $ do
          void $ liftIO $ step "sendApiClient: submit transaction"
          rks <- liftIO $ sending sid cenv batch

          void $ liftIO $ step "pollApiClient: polling for request key"
          (PollResponses resp) <- liftIO $ polling sid cenv rks expectation
          return (HashMap.lookup (NEL.head $ _rkRequestKeys rks) resp)

    let runLocal (SubmitBatch cmds) = do
          void $ step "localApiClient: submit transaction"
          local sid cenv (head $ toList cmds)

    -- batch with big tx and insufficient gas
    batch0 <- mkTxBatch txcode0 A.Null 1

    -- batch to test that gas for tx size discounted from the total gas supply
    batch1 <- mkTxBatch txcode1 A.Null 5

    res0Send <- runSend batch0 ExpectPactError
    res1Send <- runSend batch1 ExpectPactError

    res0Local <- runLocal batch0
    res1Local <- runLocal batch1

    void $ liftIO $ step "when tx too big, gas pact error thrown"
    assertEqual "LOCAL: expect gas error for big tx" gasError0 (Just $ resultOf res0Local)
    case res0Send of
      Left e -> assertFailure $ "test failure for big tx with insuffient gas: " <> show e
      Right cr -> assertEqual "SEND: expect gas error for big tx" gasError0 (resultOf <$> cr)

    let getFailureMsg (Left (Pact.PactError _ _ _ m)) = m
        getFailureMsg p = pretty $ "Expected failure result, got " ++ show p

    void $ liftIO $ step "discounts initial gas charge from gas available for pact execution"
    assertEqual "LOCAL: expect gas error after discounting initial gas charge"
      gasError1 (getFailureMsg $ resultOf res1Local)

    case res1Send of
      Left e -> assertFailure $ "test failure for discounting initial gas charge: " <> show e
      Right cr -> assertEqual "SEND: expect gas error after discounting initial gas charge"
        (Just gasError1) (getFailureMsg . resultOf <$> cr)

  where
    resultOf (CommandResult _ _ (PactResult pr) _ _ _ _) = pr
    gasError0 = Just $ Left $
      Pact.PactError Pact.GasError def [] "Tx too big (4), limit 1"
    gasError1 = "Gas limit (5) exceeded: 6"

    mkTxBatch code cdata limit = do
      ks <- testKeyPairs sender00 Nothing
      t <- toTxCreationTime <$> iot
      let ttl = 2 * 24 * 60 * 60
          pm = Pact.PublicMeta (Pact.ChainId "0") "sender00" limit 0.01 ttl t
      cmd <- liftIO $ Pact.mkExec code cdata pm ks (Just "fastTimedCPM-peterson") (Just "0")
      return $ SubmitBatch (pure cmd)

    txcode0 = T.unpack $ T.concat ["[", T.replicate 10 " 1", "]"]
    txcode1 = txcode0 <> "(identity 1)"


caplistTest :: IO (Time Micros) -> IO ChainwebNetwork -> TestTree
caplistTest iot nio = testCaseSteps "caplist TRANSFER + FUND_TX test" $ \step -> do

    let testCaseStep = void . liftIO . step

    cenv <- fmap _getClientEnv nio
    sid <- liftIO $ mkChainId v maxBound (0 :: Int)

    r <- flip runClientM cenv $ do
      batch <- liftIO
        $ mkSingletonBatch iot sender00 tx0 n0 (pm "sender00") clist

      testCaseStep "send transfer request with caplist sender00 -> sender01"
      rks <- liftIO $ sending sid cenv batch

      testCaseStep "poll for transfer results"
      PollResponses rs <- liftIO $ polling sid cenv rks ExpectPactResult

      return (HashMap.lookup (NEL.head $ _rkRequestKeys rks) rs)

    case r of
      Left e -> assertFailure $ "test failure for TRANSFER + FUND_TX: " <> show e
      Right t -> do
        assertEqual "TRANSFER + FUND_TX test" result0 (resultOf <$> t)
        assertSatisfies "meta in output" (preview (_Just . crMetaData . _Just . _Object . at "blockHash") t) isJust

  where
    n0 = Just "transfer-clist0"
    ttl = 2 * 24 * 60 * 60
    pm t = Pact.PublicMeta (Pact.ChainId "0") t 100_000 0.01 ttl

    resultOf (CommandResult _ _ (PactResult pr) _ _ _ _) = pr
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


allocation00KeyPair :: SimpleKeyPair
allocation00KeyPair =
    ( "d82d0dcde9825505d86afb6dcc10411d6b67a429a79e21bda4bb119bf28ab871"
    , "c63cd081b64ae9a7f8296f11c34ae08ba8e1f8c84df6209e5dee44fa04bcb9f5"
    )

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

allocationTest :: IO (Time Micros) -> IO ChainwebNetwork -> TestTree
allocationTest iot nio = testCaseSteps "genesis allocation tests" $ \step -> do

    let testCaseStep = void . liftIO . step

    cenv <- fmap _getClientEnv nio
    sid <- liftIO $ mkChainId v maxBound (0 :: Int)

    step "positive allocation test: allocation00 release"
    p <- flip runClientM cenv $ do
      batch0 <- liftIO
        $ mkSingletonBatch iot allocation00KeyPair tx0 n0 (pm "allocation00") Nothing

      SubmitBatch batch1 <- liftIO
        $ mkSingletonBatch iot allocation00KeyPair tx1 n1 (pm "allocation00") Nothing
      testCaseStep "sendApiClient: submit allocation release request"
      rks0 <- liftIO $ sending sid cenv batch0

      testCaseStep "pollApiClient: polling for allocation key"
      pr <- liftIO $ polling sid cenv rks0 ExpectPactResult

      testCaseStep "localApiClient: submit local account balance request"
      liftIO $ localTestToRetry sid cenv (head (toList batch1)) (localAfterPollResponse pr)

    case p of
      Left e -> assertFailure $ "test failure: " <> show e
      Right cr -> assertEqual "00 expect /local allocation balance" accountInfo (resultOf cr)


    step "negative allocation test: allocation01 release"
    q <- flip runClientM cenv $ do
      batch0 <- liftIO
        $ mkSingletonBatch iot allocation01KeyPair tx2 n2 (pm "allocation01") Nothing

      testCaseStep "sendApiClient: submit allocation release request"
      rks <- liftIO $ sending sid cenv batch0

      testCaseStep "pollApiClient: polling for allocation key"
      PollResponses r <- liftIO $ polling sid cenv rks ExpectPactError
      return $ toList r

    case q of
      Right [cr] -> case resultOf cr of
        Left e -> assertBool "expect negative allocation test failure"
          $ T.isInfixOf "Failure: Tx Failed: funds locked until \"2020-10-31T18:00:00Z\""
          $ sshow e
        _ -> assertFailure "unexpected pact result success in negative allocation test"
      _ -> assertFailure "unexpected failure in negative allocation test"


    step "positive key-rotation test: allocation2"
    r <- flip runClientM cenv $ do

      batch0 <- liftIO
        $ mkSingletonBatch iot allocation02KeyPair tx3 n3 (pm "allocation02") Nothing

      testCaseStep "senderApiClient: submit keyset rotation request"
      rks <- liftIO $ sending sid cenv batch0

      testCaseStep "pollApiClient: polling for successful rotation"
      void $ liftIO $ polling sid cenv rks ExpectPactResult

      testCaseStep "senderApiClient: submit allocation release request"
      batch1 <- liftIO
        $ mkSingletonBatch iot allocation02KeyPair' tx4 n4 (pm "allocation02") Nothing

      rks' <- liftIO $ sending sid cenv batch1
      testCaseStep "pollingApiClient: polling for successful release"
      pr <- liftIO $ polling sid cenv rks' ExpectPactResult

      testCaseStep "localApiClient: retrieving account info for allocation02"
      SubmitBatch batch2 <- liftIO
        $ mkSingletonBatch iot allocation02KeyPair' tx5 n5 (pm "allocation02") Nothing

      liftIO $ localTestToRetry sid cenv (head (toList batch2)) (localAfterPollResponse pr)

    case r of
      Left e -> assertFailure $ "test failure: " <> show e
      Right cr -> assertEqual "02 expect /local allocation balance" accountInfo' (resultOf cr)

  where
    n0 = Just "allocation-0"
    n1 = Just "allocation-1"
    n2 = Just "allocation-2"
    n3 = Just "allocation-3"
    n4 = Just "allocation-4"
    n5 = Just "allocation-5"

    localAfterPollResponse (PollResponses prs) cr =
        getBlockHeight cr > getBlockHeight (snd $ head $ HashMap.toList prs)

    -- avoiding `scientific` dep here
    getBlockHeight :: CommandResult a -> Maybe Decimal
    getBlockHeight = preview (crMetaData . _Just . key "blockHeight" . _Number . to (fromRational . toRational))

    resultOf (CommandResult _ _ (PactResult pr) _ _ _ _) = pr
    accountInfo = Right
      $ PObject
      $ ObjectMap
      $ M.fromList
        [ (FieldKey "account", PLiteral $ LString "allocation00")
        , (FieldKey "balance", PLiteral $ LDecimal 1_099_995.84) -- balance = (1k + 1mm) - gas
        , (FieldKey "guard", PGuard $ GKeySetRef (KeySetName "allocation00"))
        ]

    ttl = 2 * 24 * 60 * 60
    pm t = Pact.PublicMeta (Pact.ChainId "0") t 100_000 0.01 ttl

    tx0 = PactTransaction "(coin.release-allocation \"allocation00\")" Nothing
    tx1 = PactTransaction "(coin.details \"allocation00\")" Nothing
    tx2 = PactTransaction "(coin.release-allocation \"allocation01\")" Nothing
    tx3 =
      let
        c = "(define-keyset \"allocation02\" (read-keyset \"allocation02-keyset\"))"
        d = mkKeySet
          ["0c8212a903f6442c84acd0069acc263c69434b5af37b2997b16d6348b53fcd0a"]
          "keys-all"
      in PactTransaction c $ Just (A.object [ "allocation02-keyset" A..= d ])
    tx4 = PactTransaction "(coin.release-allocation \"allocation02\")" Nothing
    tx5 = PactTransaction "(coin.details \"allocation02\")" Nothing

    accountInfo' = Right
      $ PObject
      $ ObjectMap
      $ M.fromList
        [ (FieldKey "account", PLiteral $ LString "allocation02")
        , (FieldKey "balance", PLiteral $ LDecimal 1_099_995.13) -- 1k + 1mm - gas
        , (FieldKey "guard", PGuard $ GKeySetRef (KeySetName "allocation02"))
        ]


-- -------------------------------------------------------------------------- --
-- Utils


data PactTransaction = PactTransaction
  { _pactCode :: Text
  , _pactData :: Maybe A.Value
  } deriving (Eq, Show)


data PactTestFailure
    = PollingFailure String
    | SendFailure String
    | LocalFailure String
    | SpvFailure String
    | SlowChain String
    deriving Show

instance Exception PactTestFailure


mkSingletonBatch
    :: IO (Time Micros)
    -> SimpleKeyPair
    -> PactTransaction
    -> Maybe String
    -> (Pact.TxCreationTime -> Pact.PublicMeta)
    -> Maybe [SigCapability]
    -> IO SubmitBatch
mkSingletonBatch iot kps (PactTransaction c d) nonce pmk clist = do
    ks <- testKeyPairs kps clist
    pm <- pmk . toTxCreationTime <$> iot
    let dd = fromMaybe A.Null d
    cmd <- liftIO $ Pact.mkExec (T.unpack c) dd pm ks (Just "fastTimedCPM-peterson") nonce
    return $ SubmitBatch (cmd NEL.:| [])

withRequestKeys
    :: IO (Time Micros)
    -> IO (MVar Int)
    -> IO ChainwebNetwork
    -> (IO RequestKeys -> TestTree)
    -> TestTree
withRequestKeys iot ioNonce networkIO f = withResource mkKeys (\_ -> return ()) f
  where
    mkKeys :: IO RequestKeys
    mkKeys = do
        cenv <- _getClientEnv <$> networkIO
        mNonce <- ioNonce
        testSend iot mNonce cenv

testSend :: IO (Time Micros) -> MVar Int -> ClientEnv -> IO RequestKeys
testSend iot mNonce env = testBatch iot mNonce gp >>= sending cid env

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = do
    let mgrSettings = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple True False False) Nothing
    mgr <- HTTP.newTlsManagerWith mgrSettings
    return $ mkClientEnv mgr url

awaitCutHeight
    :: ClientEnv
    -> CutHeight
    -> IO CutHashes
awaitCutHeight cenv i = do
    result <- retrying testRetryPolicy checkRetry
        $ const $ runClientM (cutGetClient v) cenv
    case result of
        Left e -> throwM e
        Right x
            | _cutHashesHeight x >= i -> return x
            | otherwise -> throwM $ SlowChain
                $ "retries exhausted: waiting for cut height " <> sshow i
                <> " but only got " <> sshow (_cutHashesHeight x)
  where
    checkRetry _ Left{} = return True
    checkRetry s (Right c)
        | _cutHashesHeight c >= i = return False
        | otherwise = do
            debug
                $ "awaiting cut of height " <> show i
                <> ". Current height: " <> show (_cutHashesHeight c)
                <> " [" <> show (view rsIterNumberL s) <> "]"
            return True

-- | Calls to /local via the pact local api client with retry
--
local
    :: ChainId
    -> ClientEnv
    -> Command Text
    -> IO (CommandResult Hash)
local sid cenv cmd =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting local cmd for " <> (take 18 $ show cmd)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (pactLocalApiClient v sid cmd) cenv >>= \case
        Left e -> throwM $ LocalFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      LocalFailure _ -> return True
      _ -> return False

localTestToRetry
    :: ChainId
    -> ClientEnv
    -> Command Text
    -> (CommandResult Hash -> Bool)
    -> IO (CommandResult Hash)
localTestToRetry sid cenv cmd test = retrying testRetryPolicy check (\_ -> go)
  where
    go = local sid cenv cmd
    check _ cr = return $ not $ test cr

-- | Request an SPV proof using exponential retry logic
--
spv
    :: ChainId
    -> ClientEnv
    -> SpvRequest
    -> IO TransactionOutputProofB64
spv sid cenv r =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "requesting spv proof for " <> show r
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- send a single spv request and return the result
      --
      runClientM (pactSpvApiClient v sid r) cenv >>= \case
        Left e -> throwM $ SpvFailure (show e)
        Right t -> return t
  where
    h _ = Handler $ \case
      SpvFailure _ -> return True
      _ -> return False

-- | Backoff up to a constant 250ms, limiting to ~40s
-- (actually saw a test have to wait > 22s)
testRetryPolicy :: RetryPolicy
testRetryPolicy = stepped <> limitRetries 150
  where
    stepped = retryPolicy $ \rs -> case rsIterNumber rs of
      0 -> Just 20_000
      1 -> Just 50_000
      2 -> Just 100_000
      _ -> Just 250_000

-- | Send a batch with retry logic waiting for success.
sending
    :: ChainId
    -> ClientEnv
    -> SubmitBatch
    -> IO RequestKeys
sending sid cenv batch =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "sending requestkeys " <> show (_cmdHash <$> toList ss)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Send and return naively
      --
      runClientM (pactSendApiClient v sid batch) cenv >>= \case
        Left e -> throwM $ SendFailure (show e)
        Right rs -> return rs

  where
    ss = _sbCmds batch

    h _ = Handler $ \case
      SendFailure _ -> return True
      _ -> return False

-- | Poll with retry using an exponential backoff
--
data PollingExpectation = ExpectPactError | ExpectPactResult

polling
    :: ChainId
    -> ClientEnv
    -> RequestKeys
    -> PollingExpectation
    -> IO PollResponses
polling sid cenv rks pollingExpectation =
    recovering testRetryPolicy [h] $ \s -> do
      debug
        $ "polling for requestkeys " <> show (toList rs)
        <> " [" <> show (view rsIterNumberL s) <> "]"

      -- Run the poll cmd loop and check responses
      -- by making sure results are successful and request keys
      -- are sane

      runClientM (pactPollApiClient v sid $ Poll rs) cenv >>= \case
        Left e -> throwM $ PollingFailure (show e)
        Right r@(PollResponses mp) ->
          if all (go mp) (toList rs)
          then return r
          else throwM $ PollingFailure $ T.unpack $ "polling check failed: " <> encodeToText r
  where
    h _ = Handler $ \case
      PollingFailure _ -> return True
      _ -> return False

    rs = _rkRequestKeys rks

    validate (PactResult a) = case pollingExpectation of
      ExpectPactResult -> isRight a
      ExpectPactError -> isLeft a

    go m rk = case m ^. at rk of
      Just cr ->  _crReqKey cr == rk && validate (_crResult cr)
      Nothing -> False

testBatch'' :: Pact.ChainId -> IO (Time Micros) -> Integer -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch'' chain iot ttl mnonce gp' = modifyMVar mnonce $ \(!nn) -> do
    let nonce = "nonce" <> sshow nn
    t <- toTxCreationTime <$> iot
    kps <- testKeyPairs sender00 Nothing
    c <- Pact.mkExec "(+ 1 2)" A.Null (pm t) kps (Just "fastTimedCPM-peterson") (Just nonce)
    pure (succ nn, SubmitBatch (pure c))
  where
    pm :: Pact.TxCreationTime -> Pact.PublicMeta
    pm = Pact.PublicMeta chain "sender00" 1000 gp' (fromInteger ttl)

testBatch' :: IO (Time Micros) -> Integer -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch' = testBatch'' pactCid

testBatch :: IO (Time Micros) -> MVar Int -> GasPrice -> IO SubmitBatch
testBatch iot mnonce = testBatch' iot ttl mnonce
  where
    ttl = 2 * 24 * 60 * 60

--------------------------------------------------------------------------------
-- test node(s), config, etc. for this test
--------------------------------------------------------------------------------

newtype ChainwebNetwork = ChainwebNetwork { _getClientEnv :: ClientEnv }

withNodes
    :: RocksDb
    -> Natural
    -> (IO ChainwebNetwork -> TestTree)
    -> TestTree
withNodes rdb n f = withResource start
    (cancel . fst)
    (f . fmap (ChainwebNetwork . snd))
  where
    start :: IO (Async (), ClientEnv)
    start = do
        peerInfoVar <- newEmptyMVar
        a <- async $ runTestNodes rdb Quiet v n peerInfoVar
        i <- readMVar peerInfoVar
        cwEnv <- getClientEnv $ getCwBaseUrl $ _hostAddressPort $ _peerAddr i
        return (a, cwEnv)

    getCwBaseUrl :: Port -> BaseUrl
    getCwBaseUrl p = BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = fromIntegral p
        , baseUrlPath = ""
        }

runTestNodes
    :: RocksDb
    -> LogLevel
    -> ChainwebVersion
    -> Natural
    -> MVar PeerInfo
    -> IO ()
runTestNodes rdb loglevel ver n portMVar =
    forConcurrently_ [0 .. int n - 1] $ \i -> do
        threadDelay (1000 * int i)
        let baseConf = config ver n (NodeId i)
        conf <- if
            | i == 0 ->
                return $ bootstrapConfig baseConf
            | otherwise ->
                setBootstrapPeerInfo <$> readMVar portMVar <*> pure baseConf
        node rdb loglevel portMVar conf

node :: RocksDb -> LogLevel -> MVar PeerInfo -> ChainwebConfiguration -> IO ()
node rdb loglevel peerInfoVar conf = do
    rocksDb <- testRocksDb ("remotePactTest-" <> encodeUtf8 (toText nid)) rdb
    System.IO.Extra.withTempDir $ \dir -> withChainweb conf logger rocksDb (Just dir) False $ \cw -> do

        -- If this is the bootstrap node we extract the port number and publish via an MVar.
        when (nid == NodeId 0) $ do
            let bootStrapInfo = view (chainwebPeer . peerResPeer . peerInfo) cw
            putMVar peerInfoVar bootStrapInfo

        poisonDeadBeef cw
        runChainweb cw `finally` do
            logFunctionText logger Info "write sample data"
            logFunctionText logger Info "shutdown node"
        return ()
  where
    nid = _configNodeId conf
    logger :: GenericLogger
    logger = addLabel ("node", toText nid) $ genericLogger loglevel print

    poisonDeadBeef cw = mapM_ poison crs
      where
        crs = map snd $ HashMap.toList $ view chainwebChains cw
        poison cr = mempoolAddToBadList (view chainResMempool cr) deadbeef

deadbeef :: TransactionHash
deadbeef = TransactionHash "deadbeefdeadbeefdeadbeefdeadbeef"

pactDeadBeef :: RequestKey
pactDeadBeef = let (TransactionHash b) = deadbeef
               in RequestKey $ Hash $ SB.fromShort b

host :: Hostname
host = unsafeHostnameFromText "::1"

interface :: HostPreference
interface = "::1"

config
    :: ChainwebVersion
    -> Natural
    -> NodeId
    -> ChainwebConfiguration
config ver n nid = defaultChainwebConfiguration ver
    & set configNodeId nid
    & set (configP2p . p2pConfigPeer . peerConfigHost) host
    & set (configP2p . p2pConfigPeer . peerConfigInterface) interface
    & set (configP2p . p2pConfigKnownPeers) mempty
    & set (configP2p . p2pConfigIgnoreBootstrapNodes) True
    & set (configP2p . p2pConfigMaxPeerCount) (n * 2)
    & set (configP2p . p2pConfigMaxSessionCount) 4
    & set (configP2p . p2pConfigSessionTimeout) 60
    & set (configMining . miningInNode) miner
    & set configReintroTxs True
    & set (configTransactionIndex . enableConfigEnabled) True
    & set (configBlockGasLimit) 1_000_000
  where
    miner = NodeMiningConfig
        { _nodeMiningEnabled = True
        , _nodeMiner = noMiner
        , _nodeTestMiners = MinerCount n }

bootstrapConfig :: ChainwebConfiguration -> ChainwebConfiguration
bootstrapConfig conf = conf
    & set (configP2p . p2pConfigPeer) peerConfig
    & set (configP2p . p2pConfigKnownPeers) []
  where
    peerConfig = head (bootstrapPeerConfig $ _configChainwebVersion conf)
        & set peerConfigPort 0
        & set peerConfigHost host

setBootstrapPeerInfo :: PeerInfo -> ChainwebConfiguration -> ChainwebConfiguration
setBootstrapPeerInfo =
    over (configP2p . p2pConfigKnownPeers) . (:)
