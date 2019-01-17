{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact where

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types

import qualified Pact.ApiReq as P (KeyPair(..))
import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.RPC as P
import qualified Pact.Types.Server as P

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Zip

import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T

import GHC.Word
import System.IO.Extra
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Pact unit tests" pactExecTests

pactExecTests :: IO ()
pactExecTests = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig $ testPactFilesDir ++ "pact.yaml"
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 (P._ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (P._ccGasRate cmdConfig)
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)
    void $ runStateT (runReaderT execTests checkpointEnv) theState

execTests :: PactT ()
execTests = do
    -- create test nonce values of the form <current-time>:0, <current-time>:1, etc.
    prefix <- liftIO (( ++ ":") . show <$> getCurrentTime)
    let intSeq = [0, 1 ..] :: [Word64]
    let nonces = fmap (T.pack . (prefix ++) . show) intSeq
    cmdStrs <- liftIO $ mapM (getPactCode . _trCmd) testPactRequests
    let trans = zipWith3 (mkPactTransaction testKeyPairs Null) nonces intSeq cmdStrs
    outputs <- execPactTransactions trans
    let testResponses = zipWith TestResponse testPactRequests outputs
    liftIO $ checkResponses testResponses

getPactCode :: TestSource -> IO String
getPactCode (Code str) = return str
getPactCode (File filePath) = readFile' $ testPactFilesDir ++ filePath
{-
data:
  demo-admin-keyset:
    "keys": ["demoadmin"]
    "pred": ">"
codeFile: demo.pact
keyPairs:
  - public: 06c9c56daa8a068e1f19f5578cdf1797b047252e1ef0eb4a1809aa3c2226f61e
    secret: 7ce4bae38fccfe33b6344b8c260bffa21df085cf033b3dc99b4781b550e1e922
batchCmd: |-
  (demo.transfer "Acct1" "Acct2" 1.00)
-}

mkPactTransaction :: [P.KeyPair] -> Value -> Text -> Word64 -> String -> Transaction
mkPactTransaction keyPair theData nonce txId theCode =
    let pubMeta = def :: P.PublicMeta
        cmd = P.mkCommand
              (map (\P.KeyPair {..} -> (P.ED25519, _kpSecret, _kpPublic)) keyPair)
              pubMeta
              nonce
              (P.Exec (P.ExecMsg (T.pack theCode) theData))
    in Transaction {_tTxId = txId, _tCmd = cmd}

testKeyPairs :: [P.KeyPair]
testKeyPairs =
    let mPair = mzip (P.importPrivate testPrivateBs) (P.importPublic testPublicBs)
        mKeyPair = fmap (\(sec, pub) -> P.KeyPair {_kpSecret = sec, _kpPublic = pub} ) mPair
    in maybeToList mKeyPair

checkResponses :: [TestResponse] -> IO ()
checkResponses responses = do
    putStrLn "TestResponses:"
    forM_ responses (\resp -> do
        print resp
        let evalFn = _trEval $ _trRequest resp
        evalFn resp )

testPrivateBs :: ByteString
testPrivateBs = "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

testPublicBs :: ByteString
testPublicBs = "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"

execPactTransactions :: [Transaction] -> PactT [TransactionOutput]
execPactTransactions trans = do
    env <- ask
    dbState <- lift get
    liftIO $ execTransactions env dbState trans

checkScientific :: Scientific -> TestResponse -> Assertion
checkScientific sci resp = do
    let resultValue = P._crResult $ _getCommandResult $ _trOutput resp
    parseScientific resultValue @?= Just sci

parseScientific :: Value -> Maybe Scientific
parseScientific (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (Number sci) -> Just sci
    Just _ -> Nothing
parseScientific _ = Nothing

----------------------------------------------------------------------------------------------------
data TestRequest = TestRequest
    { _trCmd :: TestSource
    , _trEval :: TestResponse -> Assertion
    , _trDisplayStr :: String
    }

data TestSource = File FilePath | Code String
  deriving Show

data TestResponse = TestResponse
    { _trRequest :: TestRequest
    , _trOutput :: TransactionOutput
    }

instance Show TestRequest where
    show tr = "cmd: " ++ show (_trCmd tr) ++ "\nDisplay string: " ++ show (_trDisplayStr tr)

instance Show TestResponse where
    show tr = take 100 (show (P._crResult $ _getCommandResult (_trOutput tr)) ++ "...")

----------------------------------------------------------------------------------------------------
testPactFilesDir :: String
testPactFilesDir = "test/config/"

testPactRequests :: [TestRequest]
testPactRequests = [testReq1, testReq2]

testReq1 :: TestRequest
testReq1 = TestRequest
    { _trCmd = Code "(+ 1 1)"
    , _trEval = checkScientific (scientific 2 0)
    , _trDisplayStr = "Executes 1 + 1 in Pact and returns 2.0" }

testReq2 :: TestRequest
testReq2 = TestRequest
    { _trCmd = File "pact-test-1.txt"
    , _trEval = checkScientific (scientific 2 0)
    , _trDisplayStr = "Executes 1 + 1 in Pact and returns 2.0" }
