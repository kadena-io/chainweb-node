{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact4.PactExec
( tests
) where

import Control.Lens hiding ((.=))
import Control.Exception.Safe (tryAny)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.String
import Data.Text (Text, pack)
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Graph hiding (KnownGraph(Pair))
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Checkpointer
import Chainweb.Pact.PactService.Pact4.ExecBlock
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.InMemory (newPayloadDb)
import Chainweb.Storage.Table.RocksDB (RocksDb)
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import qualified Chainweb.Pact4.Transaction as Pact4
import qualified Chainweb.Pact4.Types as Pact4
import Chainweb.Version (ChainwebVersion(..), PactVersionT(..))
import Chainweb.Version.Utils (someChainId)
import Chainweb.Utils hiding (check)

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Persistence
import Pact.Types.Pretty

import qualified Pact.JSON.Encode as J
import Data.Functor.Product
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL

testVersion :: ChainwebVersion
testVersion = slowForkingCpmTestVersion petersenChainGraph

testEventsVersion :: ChainwebVersion
testEventsVersion = fastForkingCpmTestVersion singletonChainGraph

cid :: ChainId
cid = someChainId testVersion

tests :: TestTree
tests =
    withResource' newPayloadDb $ \pdb ->
    withResourceT withRocksResource $ \rocksIO ->
    testGroup label

    -- The test pact context evaluates the test code at block height 1.
    -- fungible-v2 is installed at that block height 1. Because applying the
    -- update twice results in an validation failures, we have to run each test on
    -- a fresh pact environment. Unfortunately, that's a bit slow.
    [ withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTest testVersion ctx testReq2
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTest testVersion ctx testReq3
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTest testVersion ctx testReq4
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTest testVersion ctx testReq5
    , withPactCtxSQLite logger testEventsVersion (bhdbIO testEventsVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTxsTest testEventsVersion ctx "testTfrGas" testTfrGas
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTxsTest testVersion ctx "testGasPayer" testGasPayer
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTxsTest testVersion ctx "testContinuationGasPayer" testContinuationGasPayer
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
        \ctx -> execTxsTest testVersion ctx "testExecGasPayer" testExecGasPayer
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
      \ctx -> execTest testVersion ctx testReq6
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
      \ctx -> execTxsTest testVersion ctx "testTfrNoGasFails" testTfrNoGasFails
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
      \ctx -> execTxsTest testVersion ctx "testBadSenderFails" testBadSenderFails
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
      \ctx -> execTxsTest testVersion ctx "testFailureRedeem" testFailureRedeem
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb testPactServiceConfig $
      \ctx -> execLocalTest ctx "testAllowReadsLocalFails" testAllowReadsLocalFails
    , withPactCtxSQLite logger testVersion (bhdbIO testVersion rocksIO) pdb allowReads $
      \ctx -> execLocalTest ctx "testAllowReadsLocalSuccess" testAllowReadsLocalSuccess
    ]
  where
    bhdbIO :: ChainwebVersion -> IO RocksDb -> IO BlockHeaderDb
    bhdbIO v rocksIO = do
        rdb <- rocksIO
        let genesisHeader = genesisBlockHeader v cid
        testBlockHeaderDb rdb genesisHeader

    label = "Chainweb.Test.Pact4.PactExec"
    allowReads = testPactServiceConfig { _pactAllowReadsInLocal = True }

    logger = dummyLogger

-- -------------------------------------------------------------------------- --
-- Pact test datatypes

type RunTest a = IO (TestResponse a) -> TestTree

-- | A test request is comprised of a list of commands, a textual discription,
-- and an test runner function, that turns an IO acttion that produces are
-- 'TestResponse' into a 'TestTree'.
--
data TestRequest = TestRequest
    { _trCmds :: ![TestSource]
    , _trDisplayStr :: !String
    , _trEval :: !(RunTest TestSource)
    }


data TestSource = File FilePath | Code String
  deriving (Show, Generic, ToJSON)

data TestResponse a = TestResponse
    { _trOutputs :: ![(a, CommandResult Hash)]
    , _trCoinBaseOutput :: !(CommandResult Hash)
    }
    deriving (Generic, Show)

type TxsTest = (IO (V.Vector Pact4.Transaction), Either String (TestResponse String) -> Assertion)

-- -------------------------------------------------------------------------- --
-- sample data

testReq2 :: TestRequest
testReq2 = TestRequest
    { _trCmds = [ File "test1.pact" ]
    , _trEval = checkSuccessOnly' "load module test1.pact"
    , _trDisplayStr = "Loads a pact module"
    }

testReq3 :: TestRequest
testReq3 = TestRequest
    { _trCmds = [ Code "(create-table test1.accounts)" ]
    , _trEval = fileCompareTxLogs "create-table"
    , _trDisplayStr = "Creates tables"
    }

testReq4 :: TestRequest
testReq4 = TestRequest
    { _trCmds = [ Code "(test1.create-global-accounts)" ]
    , _trEval = fileCompareTxLogs "create-accounts"
    , _trDisplayStr = "Creates two accounts"
    }

testReq5 :: TestRequest
testReq5 = TestRequest
    { _trCmds = [ Code "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" ]
    , _trEval = fileCompareTxLogs "transfer-accounts"
    , _trDisplayStr = "Transfers from one account to another"
    }

testReq6 :: TestRequest
testReq6 = TestRequest
    { _trCmds =
        [ Code "(+ 1 1)"
        , File "test1.pact"
        , Code "(create-table test1.accounts)"
        , Code "(test1.create-global-accounts)"
        , Code "(test1.transfer \"Acct1\" \"Acct2\" 1.00)"
        ]
    , _trEval = checkSuccessOnly' "load test1.pact, create table, transfer"
    , _trDisplayStr = "Transfers from one account to another"
    }


assertResultFail :: Show a => HasCallStack => String -> String -> Either String a -> Assertion
assertResultFail msg expectErr (Left e) = assertSatisfies msg e ((L.isInfixOf expectErr).show)
assertResultFail msg _ (Right a) = assertFailure $ msg ++ ", received: " ++ show a

checkResultSuccess :: HasCallStack => ([PactResult] -> Assertion) -> Either String (TestResponse String) -> Assertion
checkResultSuccess _ (Left e) = assertFailure $ "Expected success, got: " ++ show e
checkResultSuccess test (Right (TestResponse outs _)) = test $ map (_crResult . snd) outs

checkPactResultSuccess :: HasCallStack => String -> PactResult -> (PactValue -> Assertion) -> Assertion
checkPactResultSuccess _ (PactResult (Right pv)) test = test pv
checkPactResultSuccess msg (PactResult (Left e)) _ = assertFailure $ msg ++ ": expected tx success, got " ++ show e

checkPactResultSuccessLocal :: HasCallStack => String -> (PactValue -> Assertion) -> PactResult -> Assertion
checkPactResultSuccessLocal msg test r = checkPactResultSuccess msg r test

checkPactResultFailure :: HasCallStack => String -> String -> PactResult -> Assertion
checkPactResultFailure msg _ (PactResult (Right pv)) = assertFailure $ msg ++ ": expected tx failure, got " ++ show pv
checkPactResultFailure msg expectErr (PactResult (Left e))  = assertSatisfies msg e ((L.isInfixOf expectErr).show)

testTfrNoGasFails :: TxsTest
testTfrNoGasFails =
  (V.singleton <$> tx,
   assertResultFail "Expected missing (GAS) failure" "Keyset failure")
  where
    tx = buildCwCmd "testTfrNoGas" testVersion
         $ set cbSigners
           [ mkEd25519Signer' sender00
             [ mkTransferCap "sender00" "sender01" 1.0 ]
           ]
         $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
         $ defaultCmd

testTfrGas :: TxsTest
testTfrGas = (V.singleton <$> tx,test)
  where
    tx = buildCwCmd "testTfrGas" testEventsVersion $ set cbSigners
         [ mkEd25519Signer' sender00
           [ mkTransferCap "sender00" "sender01" 1.0
           , mkGasCap
           ]
         ]
         $ set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)")
         $ defaultCmd
    test (Left e) = assertFailure $ "Expected success, got " ++ show e
    test (Right (TestResponse [(_,cr)] _)) = do
      checkPactResultSuccess "transfer succeeds" (_crResult cr) $ \pv ->
        assertEqual "transfer succeeds" (pString "Write succeeded") pv
      e <- mkTransferEvent "sender00" "sender01" 1.0 "coin" "_S6HOO3J8-dEusvtnjSF4025dAxKu6eFSIOZocQwimA"
      assertEqual "event found" [e] (_crEvents cr)
    test r = assertFailure $ "expected single test response: " ++ show r

testBadSenderFails :: TxsTest
testBadSenderFails =
  (V.singleton <$> tx,
   assertResultFail "Expected failure on bad sender"
   "row not found: some-unknown-sender")
  where
    tx = buildCwCmd "testBadSenderFails" testVersion
         $ set cbSigners [ mkEd25519Signer' sender00 [] ]
         $ set cbSender "some-unknown-sender"
         $ set cbRPC (mkExec' "(+ 1 2)")
         $ defaultCmd

testGasPayer :: TxsTest
testGasPayer = (txs,checkResultSuccess test)
  where
    loadCode = fmap V.fromList $ do

      impl <- loadGP
      forM [ impl, setupUser, fundGasAcct ] $ \rpc ->
        buildCwCmd "testGasPayer" testVersion $
        set cbSigners [s01] $
        set cbSender "sender01" $
        set cbRPC rpc $
        defaultCmd

      where

        loadGP = (`mkExec` mkKeySetData "gas-payer-operate" [sender01]) <$>
          getPactCode (File "../../pact/gas-payer/gas-payer-v1-reference.pact")

        setupUser = mkExec
          "(gas-payer-v1-reference.fund-user \"sender00\" (read-keyset \"sender00\") 100.0)" $
          mkKeySetData "sender00" [sender00]

        fundGasAcct = mkExec'
          "(coin.transfer-create \"sender01\" \"gas-payer\" (gas-payer-v1-reference.create-gas-payer-guard) 100.0)"

        s01 = mkEd25519Signer' sender01
          [ mkTransferCap "sender01" "gas-payer" 100.0
          , mkGasCap
          , mkCapability "user.gas-payer-v1-reference" "FUND_USER" []
          ]


    runPaidTx = fmap V.singleton $ buildCwCmd "testGasPayer" testVersion $
      set cbSigners
        [mkEd25519Signer' sender00
          [mkCapability "user.gas-payer-v1-reference" "GAS_PAYER"
           [pString "sender00",pInteger 10_000,pDecimal 0.01]]] $
      set cbRPC (mkExec' "(+ 1 2)") $
      defaultCmd

    txs = do
      l <- loadCode
      r <- runPaidTx
      return $! l <> r

    test [impl,setupUser,fundGasAcct,paidTx] = do
      checkPactResultSuccess "impl" impl $
        assertEqual "impl" (pString "TableCreated")
      checkPactResultSuccess "setupUser" setupUser $
        assertEqual "setupUser" (pString "Write succeeded")
      checkPactResultSuccess "fundGasAcct" fundGasAcct $
        assertEqual "fundGasAcct" (pString "Write succeeded")
      checkPactResultSuccess "paidTx" paidTx $
        assertEqual "paidTx" (pDecimal 3)
    test r = assertFailure $ "Expected 4 results, got: " ++ show r


testContinuationGasPayer :: TxsTest
testContinuationGasPayer = (txs,checkResultSuccess test)
  where
    setupExprs = do
      implCode <- getPactCode (File "../pact/continuation-gas-payer.pact")
      return [ implCode
             , "(coin.transfer-create \"sender00\" \"cont-gas-payer\" (gas-payer-for-cont.create-gas-payer-guard) 100.0)"
             , "(simple-cont-module.some-two-step-pact)"
             , "(coin.get-balance \"cont-gas-payer\")" ]

    setupTest = fmap V.fromList $ do
      setupExprs' <- setupExprs
      forM setupExprs' $ \se -> buildCwCmd "testContinuationGasPayer" testVersion $
        set cbSigners
          [ mkEd25519Signer' sender00
            [ mkTransferCap "sender00" "cont-gas-payer" 100.0
            , mkGasCap
            ]] $
        set cbRPC (mkExec' se) $
        defaultCmd

    contPactId = "toj9L2EGOyUUkFQCniHv8qGy8sStXDmaWSpVB3XKEuE"

    runStepTwoWithGasPayer = fmap V.singleton $ buildCwCmd "testContinuationGasPayer" testVersion $
      set cbSigners
        [ mkEd25519Signer' sender01
          [ mkCapability "user.gas-payer-for-cont" "GAS_PAYER"
            [pString "sender01",pInteger 10_000,pDecimal 0.01]
          ]] $
      set cbSender "cont-gas-payer" $
      set cbRPC (mkCont $ mkContMsg (fromString contPactId) 1) $
      defaultCmd

    balanceCheck = fmap V.singleton $ buildCwCmd "testContinuationGasPayer2" testVersion $
      set cbSigners [mkEd25519Signer' sender00 []] $
      set cbRPC (mkExec' "(coin.get-balance \"cont-gas-payer\")") $
      defaultCmd

    txs = do
      s <- setupTest
      assertEqual "pact ID correct" (Just $ "\"" <> contPactId <> "\"") $
        preview (ix 2 . cmdHash . to show) s
      r <- runStepTwoWithGasPayer
      b <- balanceCheck
      return $! s <> r <> b

    test [impl,fundGasAcct,contFirstStep,balCheck1,paidSecondStep,balCheck2] = do
      checkPactResultSuccess "impl" impl $ assertEqual "impl"
        (pString "Loaded module user.simple-cont-module, hash pCtVh0IDPvRIdVFXxznBFTwsZcwbIcYAnfv7yzr4wRI")
      checkPactResultSuccess "fundGasAcct" fundGasAcct $ assertEqual "fundGasAcct"
        (pString "Write succeeded")
      checkPactResultSuccess "contFirstStep" contFirstStep $ assertEqual "contFirstStep"
        (pString "Step One")
      checkPactResultSuccess "balCheck1" balCheck1 $
        assertEqual "balCheck1" (pDecimal 100)
      checkPactResultSuccess "paidSecondStep" paidSecondStep $
        assertEqual "paidSecondStep"
        (pString "Step Two")
      checkPactResultSuccess "balCheck2" balCheck2 $
        assertEqual "balCheck2" (pDecimal 99.999_4)
    test r = assertFailure $ "Expected 6 results, got: " ++ show r

testExecGasPayer :: TxsTest
testExecGasPayer = (txs,checkResultSuccess test)
  where
    setupExprs = do
      implCode <- getPactCode (File "../pact/exec-gas-payer.pact")
      return [ implCode
             , "(coin.transfer-create \"sender00\" \"exec-gas-payer\" (gas-payer-for-exec.create-gas-payer-guard) 100.0)"
             , "(coin.get-balance \"exec-gas-payer\")" ]
    setupTest = fmap V.fromList $ do
      setupExprs' <- setupExprs
      forM setupExprs' $ \se -> buildCwCmd "testExecGasPayer" testVersion $
        set cbSigners
          [ mkEd25519Signer' sender00
            [ mkTransferCap "sender00" "exec-gas-payer" 100.0
            , mkGasCap
            ]] $
        set cbRPC (mkExec' se) $
        defaultCmd

    runPaidTx = fmap V.singleton $ buildCwCmd "testExecGasPayer" testVersion $
      set cbSigners
      [mkEd25519Signer' sender01
        [mkCapability "user.gas-payer-for-exec" "GAS_PAYER"
         [pString "sender01",pInteger 10_000,pDecimal 0.01]]] $
      set cbSender "exec-gas-payer" $
      set cbRPC (mkExec' "(+ 1 2)") $
      defaultCmd

    balanceCheck = fmap V.singleton $ buildCwCmd "testExecGasPayer" testVersion $
      set cbSigners [mkEd25519Signer' sender00 []] $
      set cbRPC (mkExec' "(coin.get-balance \"exec-gas-payer\")") $
      defaultCmd

    txs = do
      s <- setupTest
      r <- runPaidTx
      b <- balanceCheck
      return $! s <> r <> b

    test [impl,fundGasAcct,balCheck1,paidTx,balCheck2] = do
      checkPactResultSuccess "impl" impl $ assertEqual "impl"
        (pString "Loaded module user.gas-payer-for-exec, hash _S7ASfb_Lvr5wmjERG_XwPUoojW6GHBWI2u0W6jmID0")
      checkPactResultSuccess "fundGasAcct" fundGasAcct $ assertEqual "fundGasAcct"
        (pString "Write succeeded")
      checkPactResultSuccess "balCheck1" balCheck1 $ assertEqual "balCheck1" (pDecimal 100)
      checkPactResultSuccess "paidTx" paidTx $ assertEqual "paidTx" (pDecimal 3)
      checkPactResultSuccess "balCheck2" balCheck2 $ assertEqual "balCheck2" (pDecimal 99.999_4)
    test r = assertFailure $ "Expected 6 results, got: " ++ show r

testFailureRedeem :: TxsTest
testFailureRedeem = (txs,checkResultSuccess test)
  where
    txs = fmap V.fromList $ forM exps $ \e -> buildCwCmd "testFailureRedeem" testVersion $
      set cbSigners [mkEd25519Signer' sender00 []] $
      set cbGasPrice 0.01 $
      set cbGasLimit 1000 $
      set cbRPC (mkExec' e) $
      defaultCmd

    exps =
      ["(coin.get-balance \"sender00\")"
      ,"(coin.get-balance \"miner\")"
      ,"(enforce false \"forced error\")"
      ,"(coin.get-balance \"sender00\")"
      ,"(coin.get-balance \"miner\")"]
    test [sbal0,mbal0,ferror,sbal1,mbal1] = do
      -- sender 00 first is 100000000 - full gas debit during tx (1)
      checkPactResultSuccess "sender bal 0" sbal0 $
        assertEqual "sender bal 0" (pDecimal 99_999_990)
      -- miner first is reward + tx gas for [0]
      checkPactResultSuccess "miner bal 0" mbal0 $
        assertEqual "miner bal 0" (pDecimal 2.504523)
      -- this should reward 10 more to miner
      checkPactResultFailure "forced error" "forced error" ferror
      -- sender 00 second is down epsilon size costs
      -- from [0,1] + 10 for error + 10 full gas debit during tx ~ 99999980
      checkPactResultSuccess "sender bal 1" sbal1 $
        assertEqual "sender bal 1" (pDecimal 99_999_979.6)
      -- miner second is up 10 from error plus epsilon from [1,2,3] ~ 12
      checkPactResultSuccess "miner bal 1" mbal1 $
        assertEqual "miner bal 1" (pDecimal 12.904523)
    test r = assertFailure $ "Expected 5 results, got: " ++ show r


checkLocalSuccess :: HasCallStack => (PactResult -> Assertion) -> Either String (CommandResult Hash) -> Assertion
checkLocalSuccess _ (Left e) = assertFailure $ "Expected success, got: " ++ show e
checkLocalSuccess test (Right cr) = test $ _crResult cr

testAllowReadsLocalFails :: LocalTest
testAllowReadsLocalFails = (tx,test)
  where
    tx = buildCwCmd "testAllowReadsLocalFails" testVersion $
         set cbRPC (mkExec' "(read coin.coin-table \"sender00\")") $
         defaultCmd
    test = checkLocalSuccess $
      checkPactResultFailure "testAllowReadsLocalFails" "Enforce non-upgradeability"

testAllowReadsLocalSuccess :: LocalTest
testAllowReadsLocalSuccess = (tx,test)
  where
    tx = buildCwCmd "testAllowReadsLocalSuccess" testVersion $
         set cbRPC (mkExec' "(at 'balance (read coin.coin-table \"sender00\"))") $
         defaultCmd
    test = checkLocalSuccess $
      checkPactResultSuccessLocal "testAllowReadsLocalSuccess" $
      assertEqual "sender00 bal" (pDecimal 100_000_000.0)


-- -------------------------------------------------------------------------- --
-- Utils

execTest
    :: (Logger logger)
    => ChainwebVersion
    -> WithPactCtxSQLite logger tbl
    -> TestRequest
    -> TestTree
execTest v runPact request = _trEval request $ do
    cmdStrs <- mapM getPactCode $ _trCmds request
    trans <- mkCmds cmdStrs
    results <- runPact $ (throwIfNoHistory =<<) $
      readFrom (Just $ ParentHeader $ genesisBlockHeader v cid) $
        SomeBlockM $ Pair
          (execTransactions defaultMiner
            trans (Pact4.EnforceCoinbaseFailure True) (Pact4.CoinbaseUsePrecompiled True) Nothing Nothing
            >>= throwCommandInvalidError
            )
          (error "Pact5")

    let outputs = V.toList $ snd <$> _transactionPairs results
    return $ TestResponse
        (zip (_trCmds request) (hashPact4TxLogs <$> outputs))
        (hashPact4TxLogs $ _transactionCoinbase results)
  where
    mkCmds cmdStrs =
      fmap V.fromList $ forM (zip cmdStrs [0..]) $ \(code,n :: Int) ->
      buildCwCmd ("1" <> sshow n) testVersion $
      set cbSigners [mkEd25519Signer' sender00 []] $
      set cbGasPrice 0.01 $
      set cbTTL 1_000_000 $
      set cbRPC (mkExec code $ mkKeySetData "test-admin-keyset" [sender00]) $
      defaultCmd

execTxsTest
    :: (Logger logger)
    => ChainwebVersion
    -> WithPactCtxSQLite logger tbl
    -> String
    -> TxsTest
    -> TestTree
execTxsTest v runPact name (trans',check) = testCase name (go >>= check)
  where
    go = do
      trans <- trans'
      results' <- tryAllSynchronous $ runPact $ (throwIfNoHistory =<<) $
        readFrom (Just $ ParentHeader $ genesisBlockHeader v cid) $
          SomeBlockM $ Pair
            (execTransactions defaultMiner trans
              (Pact4.EnforceCoinbaseFailure True) (Pact4.CoinbaseUsePrecompiled True) Nothing Nothing
              >>= throwCommandInvalidError
              )
            (error "Pact5")
      case results' of
        Right results -> Right <$> do
          let outputs = V.toList $ snd <$> _transactionPairs results
              tcode = _pNonce . Pact4.payloadObj . _cmdPayload
              inputs = map (showPretty . tcode) $ V.toList trans
          return $ TestResponse
            (zip inputs (hashPact4TxLogs <$> outputs))
            (hashPact4TxLogs $ _transactionCoinbase results)
        Left e -> return $ Left $ show e

type LocalTest = (IO Pact4.Transaction,Either String (CommandResult Hash) -> Assertion)

execLocalTest
    :: (Logger logger, CanReadablePayloadCas tbl)
    => WithPactCtxSQLite logger tbl
    -> String
    -> LocalTest
    -> TestTree
execLocalTest runPact name (trans',check) = testCase name (go >>= check)
  where
    go = do
      trans <- trans'
      results' <- tryAny $ runPact $
        execLocal (Pact4.unparseTransaction trans) Nothing Nothing Nothing
      case results' of
        Right (preview _MetadataValidationFailure -> Just e) ->
          return $ Left $ show e
        Right (preview _LocalTimeout -> Just ()) ->
          return $ Left "LocalTimeout"
        Right (preview _LocalResultLegacy -> Just cr) -> do
          Just decodedCr <- return $ Aeson.decode (TL.encodeUtf8 $ TL.fromStrict $ J.getJsonText cr)
          return $ Right decodedCr
        Right (preview _LocalResultWithWarns -> Just (cr, _)) -> do
          Just decodedCr <- return $ Aeson.decode (TL.encodeUtf8 $ TL.fromStrict $ J.getJsonText cr)
          return $ Right decodedCr
        Right _ -> error "unknown local result"
        Left e ->
          return $ Left $ show e

getPactCode :: TestSource -> IO Text
getPactCode (Code str) = return (pack str)
getPactCode (File filePath) = pack <$> readFile' (testPactFilesDir ++ filePath)

checkSuccessOnly :: CommandResult Hash -> Assertion
checkSuccessOnly cr = case _crResult cr of
  PactResult (Right _) -> return ()
  r -> assertFailure $ "Failure status returned: " ++ show r

checkSuccessOnly' :: String -> IO (TestResponse TestSource) -> TestTree
checkSuccessOnly' msg f = testCase msg $ f >>= \case
    TestResponse res@(_:_) _ -> checkSuccessOnly (snd $ last res)
    TestResponse res _ -> fail (show res) -- TODO


-- | A test runner for golden tests.
--
fileCompareTxLogs :: String -> IO (TestResponse TestSource) -> TestTree
fileCompareTxLogs label respIO = golden label $ do
    resp <- respIO
    return $ BL.fromStrict $ Y.encode
        $ coinbase (_trCoinBaseOutput resp)
        : (result <$> _trOutputs resp)
  where
    result (cmd, out) = object
        [ "output" .= J.toJsonViaEncode (_crLogs out)
        , "cmd" .= cmd
        ]
    coinbase out = object
        [ "output" .= J.toJsonViaEncode (_crLogs out)
        , "cmd" .= ("coinbase" :: String)
        ]


_showValidationFailure :: IO ()
_showValidationFailure = do
  txs <- fmap V.singleton $ buildCwCmd "nonce" testVersion $
    set cbSigners [mkEd25519Signer' sender00 []] $
    set cbRPC (mkExec' "(coin.transfer \"sender00\" \"sender01\" 1.0)") $
    defaultCmd
  let cr1 = CommandResult
        { _crReqKey = RequestKey pactInitialHash
        , _crTxId = Nothing
        , _crResult = PactResult $ Right $ pString "hi"
        , _crGas = 0
        , _crLogs = Just [encodeTxLog $ TxLog "Domain" "Key" (object [ "stuff" .= True ])]
        , _crContinuation = Nothing
        , _crMetaData = Nothing
        , _crEvents = []
        }
      outs1 = Transactions
        { _transactionPairs = V.zip txs (V.singleton cr1)
        , _transactionCoinbase = cr1
        }
      miner = defaultMiner
      header = genesisBlockHeader testVersion $ someChainId testVersion
      pwo = toPayloadWithOutputs Pact4T miner outs1
      cr2 = set crGas 1 cr1
      outs2 = Transactions
        { _transactionPairs = V.zip txs (V.singleton cr2)
        , _transactionCoinbase = cr2
        }
      r = validateHashes header (CheckablePayloadWithOutputs pwo) miner outs2

  BL.putStrLn $ case r of
    Left e -> J.encode $ J.text (sshow e)
    Right x -> encode x
