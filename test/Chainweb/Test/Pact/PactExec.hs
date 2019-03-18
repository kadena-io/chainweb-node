{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact.PactExec where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Aeson
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Scientific
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.FilePath
import System.IO.Extra

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Gas
import Pact.Types.Logger
import Pact.Types.Server

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils

tests :: IO TestTree
tests = do
    setup <- pactTestSetup
    stdTests <- pactExecTests setup StdBlock
    -- genesisTests <- pactExecTests setup GenesisBlock
    pure $ testGroup "Simple pact execution tests" stdTests

pactTestSetup :: IO PactTestSetup
pactTestSetup = do
    let loggers = alwaysLog
    let logger = newLogger loggers $ LogName "PactService"
    pactCfg <- setupConfig $ testPactFilesDir ++ "pact.yaml"
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 (_ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (_ccGasRate cmdConfig)
    let gasEnv = GasEnv (fromIntegral gasLimit) 0.0
                          (constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case _ccSqlite cmdConfig of
            Nothing -> do
                env <- mkPureEnv loggers
                liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- mkSQLiteEnv logger False sqlc loggers
                liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)

    -- Coin contract must be created and embedded in the genesis
    -- block prior to initial save
    ccState <- createCoinContract theState
    void $! saveInitial (_cpeCheckpointer checkpointEnv) ccState

    pure $ PactTestSetup checkpointEnv ccState


pactExecTests :: PactTestSetup -> BlockType -> IO [TestTree]
pactExecTests (PactTestSetup env st) t =
    fst <$> runStateT (runReaderT (execTests t) env) st

execTests :: BlockType -> PactT [TestTree]
execTests t = do
    cmdStrs <- liftIO $ mapM (getPactCode . _trCmd) testPactRequests
    trans <- liftIO $ mkPactTestTransactions cmdStrs
    (results, _dbState) <- execTransactions (isGenesis t) defaultMiner trans
    let outputs = snd <$> _transactionPairs results
    let testResponses = V.toList $ V.zipWith TestResponse testPactRequests outputs
    liftIO $ checkResponses testResponses

getPactCode :: TestSource -> IO String
getPactCode (Code str) = return str
getPactCode (File filePath) = readFile' $ testPactFilesDir ++ filePath

checkResponses :: [TestResponse] -> IO [TestTree]
checkResponses responses = traverse (\resp -> _trEval (_trRequest resp ) resp) responses

checkSuccessOnly :: TestResponse -> Assertion
checkSuccessOnly resp =
    case _flCommandResult $ _trOutput resp of
        (Object o) -> HM.lookup "status" o @?= Just "success"
        _ -> assertFailure "Status returned does not equal \"success\""

checkScientific :: Scientific -> TestResponse -> Assertion
checkScientific sci resp = do
    let resultValue = _flCommandResult $ _trOutput resp
    parseScientific resultValue @?= Just sci

parseScientific :: Value -> Maybe Scientific
parseScientific (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (Number sci) -> Just sci
    Just _ -> Nothing
parseScientific _ = Nothing

ignoreTextMatch :: Text -> TestResponse -> Assertion
ignoreTextMatch _ _ = True @?= True

fullTextMatch :: Text -> TestResponse -> Assertion
fullTextMatch matchText resp = do
    let resultValue = _flCommandResult $ _trOutput resp
    parseText resultValue @?= Just matchText

parseText :: Value -> Maybe Text
parseText (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (String t) -> Just t
    Just _ -> Nothing
parseText _ = Nothing

fileCompareTxLogs :: FilePath -> TestResponse -> IO TestTree
fileCompareTxLogs fp resp =
    return $ goldenVsString (takeBaseName fp) (testPactFilesDir ++ fp) ioBs
    where
        ioBs = return $ toS $ show <$> take 1 . _flTxLogs $ _trOutput resp

----------------------------------------------------------------------------------------------------
-- Pact test datatypes
----------------------------------------------------------------------------------------------------

data TestRequest = TestRequest
    { _trCmd :: TestSource
    , _trEval :: TestResponse -> IO TestTree
    , _trDisplayStr :: String
    }

data TestSource = File FilePath | Code String
  deriving Show

data TestResponse = TestResponse
    { _trRequest :: TestRequest
    , _trOutput :: FullLogTxOutput
    }

instance Show TestRequest where
    show tr = "cmd: " ++ show (_trCmd tr) ++ "\nDisplay string: "
              ++ show (_trDisplayStr tr)

instance Show TestResponse where
    show tr =
        let tOutput = _trOutput tr
            cmdResultStr = show $ _flCommandResult tOutput
            txLogsStr = unlines $ fmap show (_flTxLogs tOutput)
        in "\n\nCommandResult: " ++ cmdResultStr ++ "\n\n"
           ++ "TxLogs: " ++ txLogsStr

data PactTestSetup = PactTestSetup
  { _checkpointEnv :: CheckpointEnv
  , _pactDbState :: PactDbState
  }

data BlockType = GenesisBlock | StdBlock

isGenesis :: BlockType -> Bool
isGenesis = \case
  GenesisBlock -> True
  _ -> False

----------------------------------------------------------------------------------------------------
-- sample data
----------------------------------------------------------------------------------------------------

testPactRequests :: Vector TestRequest
testPactRequests = V.fromList
    [ testReq1
    , testReq2
    , testReq3
    , testReq4
    , testReq5
    ]

testReq1 :: TestRequest
testReq1 = TestRequest
    { _trCmd = Code "(+ 1 1)"
    , _trEval = return . testCase "addition" . checkScientific (scientific 2 0)
    , _trDisplayStr = "Executes 1 + 1 in Pact and returns 2.0" }

testReq2 :: TestRequest
testReq2 = TestRequest
    { _trCmd = File "test1.pact"
    , _trEval = return . testCase "load module" . checkSuccessOnly
    , _trDisplayStr = "Loads a pact module" }

testReq3 :: TestRequest
testReq3 = TestRequest
    { _trCmd = Code "(create-table test1.accounts)"
    , _trEval = fileCompareTxLogs "create-table-expected.txt"
    , _trDisplayStr = "Creates tables" }

testReq4 :: TestRequest
testReq4 = TestRequest
    { _trCmd = Code "(test1.create-global-accounts)"
    , _trEval = fileCompareTxLogs "create-accounts-expected.txt"
    , _trDisplayStr = "Creates two accounts" }

testReq5 :: TestRequest
testReq5 = TestRequest
    { _trCmd = Code "(test1.transfer \"Acct1\" \"Acct2\" 1.00)"
    , _trEval = fileCompareTxLogs "transfer-accounts-expected.txt"
    , _trDisplayStr = "Transfers from one account to another" }
