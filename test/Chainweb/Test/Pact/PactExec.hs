{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact.PactExec
( tests
) where

import Control.Lens (view, _1)
import Control.Monad.Catch
import Data.Aeson
import Data.CAS.RocksDB (RocksDb)
import Data.Decimal
import Data.Default
import Data.List
import Data.String.Conv (toS)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import GHC.Generics (Generic)

import System.IO.Extra (readFile')

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash (nullBlockHash)
import Chainweb.BlockHeader.Genesis (genesisBlockHeader)
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Graph
import Chainweb.Miner.Pact
import Chainweb.Pact.PactService (execTransactions)
import Chainweb.Pact.Types
import Chainweb.Payload.PayloadStore.InMemory (newPayloadDb)
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Transaction
import Chainweb.Version (ChainwebVersion(..), someChainId)

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Pretty

testVersion :: ChainwebVersion
testVersion = FastTimedCPM petersonChainGraph

tests :: ScheduledTest
tests = ScheduledTest label $
        withResource newPayloadDb killPdb $ \pdb ->
        withRocksResource $ \rocksIO ->
        testGroup label
    [ withPactCtxSQLite testVersion Nothing (bhdbIO rocksIO) pdb $
        \ctx -> testGroup "single transactions" $ schedule Sequential
            [ execTest ctx testReq2
            , execTest ctx testReq3
            , execTest ctx testReq4
            , execTest ctx testReq5
            , execTxsTest ctx "testTfrGas" testTfrGas
            , execTxsTest ctx "testGasPayer" testGasPayer
            ]
    , withPactCtxSQLite testVersion Nothing (bhdbIO rocksIO) pdb $
      \ctx2 -> _schTest $ execTest ctx2 testReq6
      -- failures mess up cp state so run alone
    , withPactCtxSQLite testVersion Nothing (bhdbIO rocksIO) pdb $
      \ctx -> _schTest $ execTxsTest ctx "testTfrNoGasFails" testTfrNoGasFails
    , withPactCtxSQLite testVersion Nothing (bhdbIO rocksIO) pdb $
      \ctx -> _schTest $ execTxsTest ctx "testBadSenderFails" testBadSenderFails

    ]
  where
    bhdbIO :: IO RocksDb -> IO BlockHeaderDb
    bhdbIO rocksIO = do
        rdb <- rocksIO
        let genesisHeader = genesisBlockHeader testVersion cid
        testBlockHeaderDb rdb genesisHeader

    label = "Chainweb.Test.Pact.PactExec"
    killPdb _ = return ()
    cid = someChainId testVersion


-- -------------------------------------------------------------------------- --
-- Pact test datatypes

type RunTest a = IO (TestResponse a) -> ScheduledTest

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
    { _trOutputs :: ![(a, HashCommandResult)]
    , _trCoinBaseOutput :: !HashCommandResult
    }
    deriving (Generic, ToJSON)

type TxsTest = (IO (V.Vector ChainwebTransaction), Either String (TestResponse String) -> Assertion)

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


pString :: Text -> PactValue
pString = PLiteral . LString
pDecimal :: Decimal -> PactValue
pDecimal = PLiteral . LDecimal
pInteger :: Integer -> PactValue
pInteger = PLiteral . LInteger

assertResultFail :: HasCallStack => String -> String -> Either String (TestResponse String) -> Assertion
assertResultFail msg expectErr (Left e) = assertSatisfies msg e ((isInfixOf expectErr).show)
assertResultFail msg _ (Right _) = assertFailure $ msg

checkResultSuccess :: HasCallStack => ([PactResult] -> Assertion) -> Either String (TestResponse String) -> Assertion
checkResultSuccess _ (Left e) = assertFailure $ "Expected success, got: " ++ show e
checkResultSuccess test (Right (TestResponse outs _)) = test $ map (_crResult . snd) outs

checkPactResultSuccess :: HasCallStack => String -> PactResult -> (PactValue -> Assertion) -> Assertion
checkPactResultSuccess _ (PactResult (Right pv)) test = test pv
checkPactResultSuccess msg (PactResult (Left e)) _ = assertFailure $ msg ++ ": expected tx success, got " ++ show e

testTfrNoGasFails :: TxsTest
testTfrNoGasFails = (txs,assertResultFail "Expected missing (GAS) failure" "Keyset failure")
  where
    txs = ks >>= \ks' ->
      mkTestExecTransactions "sender00" "0" ks' "testTfrNoGas" 10000 0.01 1000000 0 $
      V.fromList [PactTransaction "(coin.transfer \"sender00\" \"sender01\" 1.0)" Nothing]
    ks = testKeyPairs sender00KeyPair $ Just
      [ SigCapability (QualifiedName "coin" "TRANSFER" def) [pString "sender00",pString "sender01",pDecimal 1.0]
      ]

testTfrGas :: TxsTest
testTfrGas = (txs,checkResultSuccess test)
  where
    txs = ks >>= \ks' ->
      mkTestExecTransactions "sender00" "0" ks' "testTfrGas" 10000 0.01 1000000 0 $
      V.fromList [PactTransaction "(coin.transfer \"sender00\" \"sender01\" 1.0)" Nothing]
    ks = testKeyPairs sender00KeyPair $ Just
      [ SigCapability (QualifiedName "coin" "TRANSFER" def) [pString "sender00",pString "sender01",pDecimal 1.0]
      , SigCapability (QualifiedName "coin" "GAS" def) []
      ]
    test [PactResult (Right pv)] = assertEqual "transfer succeeds" (pString "Write succeeded") pv
    test r = assertFailure $ "Expected single result, got: " ++ show r

testBadSenderFails :: TxsTest
testBadSenderFails = (txs,assertResultFail "Expected failure on bad sender"
                             "row not found: some-unknown-sender")
  where
    txs = ks >>= \ks' ->
      mkTestExecTransactions "some-unknown-sender" "0" ks' "testBadSenderFails" 10000 0.01 1000000 0 $
      V.fromList [PactTransaction "(+ 1 2)" Nothing]
    ks = testKeyPairs sender00KeyPair Nothing

testGasPayer :: TxsTest
testGasPayer = (txs,checkResultSuccess test)
  where
    loadCode = do
      -- setup is all signed by sender01
      -- load impl, operate is sender01
      let operateKs = Just $ object
            [ "gas-payer-operate" .= [ view _1 sender01KeyPair ] ]
      impl <- (`PactTransaction` operateKs) <$>
        getPactCode (File "../../pact/gas-payer/gas-payer-v1-reference.pact")
      let
        -- setup gas user sender00 with 100.0 balance
        sender00ks = Just $ object
          [ "sender00" .= [ view _1 sender00KeyPair ] ]
        setupUser = PactTransaction
          "(gas-payer-v1-reference.fund-user \"sender00\" (read-keyset \"sender00\") 100.0)"
          sender00ks
        -- transfer-create gas-payer by funding from sender01
        fundGasAcct = PactTransaction
          "(coin.transfer-create \"sender01\" \"gas-payer\" (gas-payer-v1-reference.create-gas-payer-guard) 100.0)"
          Nothing
      -- sign with sender01 and caps for transfer, gas
      ks <- testKeyPairs sender01KeyPair $ Just
        [ SigCapability (QualifiedName "coin" "TRANSFER" def)
          [pString "sender01",pString "gas-payer",pDecimal 100.0]
        , SigCapability (QualifiedName "coin" "GAS" def) []
        , SigCapability (QualifiedName (ModuleName "gas-payer-v1-reference" (Just "user")) "FUND_USER" def) []
        ]
      mkTestExecTransactions "sender01" "0" ks "testGasPayer" 10000 0.01 1000000 0 $
        V.fromList [impl,setupUser,fundGasAcct]

    runPaidTx = do
      ks <- testKeyPairs sender00KeyPair $ Just
        [ SigCapability (QualifiedName (ModuleName "gas-payer-v1-reference" (Just "user")) "GAS_PAYER" def)
          [pString "sender00",pInteger 10000,pDecimal 0.01] ]
      mkTestExecTransactions "gas-payer" "0" ks "testGasPayer" 10000 0.01 1000000 0 $
        V.fromList [PactTransaction "(+ 1 2)" Nothing]

    txs = do
      l <- loadCode
      r <- runPaidTx
      return $! l <> r

    test [impl,setupUser,fundGasAcct,paidTx] = do
      checkPactResultSuccess "impl" impl $ assertEqual "impl" (pString "TableCreated")
      checkPactResultSuccess "setupUser" setupUser $ assertEqual "setupUser" (pString "Write succeeded")
      checkPactResultSuccess "fundGasAcct" fundGasAcct $ assertEqual "fundGasAcct" (pString "Write succeeded")
      checkPactResultSuccess "paidTx" paidTx $ assertEqual "paidTx" (pDecimal 3)
    test r = assertFailure $ "Expected 5 results, got: " ++ show r


-- -------------------------------------------------------------------------- --
-- Utils

execTest
    :: WithPactCtxSQLite cas
    -> TestRequest
    -> ScheduledTest
execTest runPact request = _trEval request $ do
    cmdStrs <- mapM getPactCode $ _trCmds request
    d <- adminData
    trans <- goldenTestTransactions . V.fromList $ fmap (k d) cmdStrs
    results <- runPact $ execTransactions (Just nullBlockHash) defaultMiner trans
    let outputs = V.toList $ snd <$> _transactionPairs results
    return $ TestResponse
        (zip (_trCmds request) outputs)
        (_transactionCoinbase results)
  where
    k d c = PactTransaction c d



execTxsTest
    :: WithPactCtxSQLite cas
    -> String
    -> TxsTest
    -> ScheduledTest
execTxsTest runPact name (trans',check) = testCaseSch name (go >>= check)
  where
    go = do
      trans <- trans'
      results' <- try $ runPact $ execTransactions (Just nullBlockHash) defaultMiner trans
      case results' of
        Right results -> Right <$> do
          let outputs = V.toList $ snd <$> _transactionPairs results
              tcode = _pNonce . payloadObj . _cmdPayload
              inputs = map (showPretty . tcode) $ V.toList trans
          return $ TestResponse
            (zip inputs outputs)
            (_transactionCoinbase results)
        Left (e :: SomeException) -> return $ Left $ show e

getPactCode :: TestSource -> IO Text
getPactCode (Code str) = return (pack str)
getPactCode (File filePath) = pack <$> readFile' (testPactFilesDir ++ filePath)

checkSuccessOnly :: HashCommandResult -> Assertion
checkSuccessOnly cr = case _crResult cr of
  PactResult (Right _) -> return ()
  r -> assertFailure $ "Failure status returned: " ++ show r

checkSuccessOnly' :: String -> IO (TestResponse TestSource) -> ScheduledTest
checkSuccessOnly' msg f = testCaseSch msg $ f >>= \case
    TestResponse res@(_:_) _ -> checkSuccessOnly (snd $ last res)
    TestResponse res _ -> fail (show res) -- TODO


-- | A test runner for golden tests.
--
fileCompareTxLogs :: String -> IO (TestResponse TestSource) -> ScheduledTest
fileCompareTxLogs label respIO = goldenSch label $ do
    resp <- respIO
    return $ toS $ Y.encode
        $ coinbase (_trCoinBaseOutput resp)
        : (result <$> _trOutputs resp)
  where
    result (cmd, out) = object
        [ "output" .= _crLogs out
        , "cmd" .= cmd
        ]
    coinbase out = object
        [ "output" .= _crLogs out
        , "cmd" .= ("coinbase" :: String)
        ]
