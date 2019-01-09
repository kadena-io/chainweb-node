{-# LANGUAGE DeriveGeneric #-}
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
import qualified Pact.Types.API as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.RPC as P
import qualified Pact.Types.Server as P

import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Zip
import Data.Aeson (Value(..))
import Data.ByteString (ByteString)
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Pact unit tests" pactExecTests

pactExecTests :: IO ()
pactExecTests = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
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
    void $ runRWST execTests checkpointEnv theState

execTests :: PactT ()
execTests = do
    let reqIds = [1000, 1001 ..] :: [Int64]
    let textIds = fmap (T.pack . show) reqIds
    let cmdStrs = fmap _trCmd testPactRequests
    let cmds = zipWith (mkPactCommand testKeyPairs Null) textIds cmdStrs
    _results <- execCommands cmds
    return ()

mkPactCommand :: [P.KeyPair] -> Value -> Text -> String -> P.Command ByteString
mkPactCommand keyPair theData reqId theCode =
    P.mkCommand
      (map (\P.KeyPair {..} -> (P.ED25519, _kpSecret, _kpPublic)) keyPair)
      Nothing
      reqId -- nonce
      (P.Exec (P.ExecMsg (T.pack theCode) theData))

testKeyPairs :: [P.KeyPair]
testKeyPairs =
    let mPair = mzip (P.importPrivate testPrivateBs) (P.importPublic testPublicBs)
        mKeyPair = fmap (\(sec, pub) -> P.KeyPair {_kpSecret = sec, _kpPublic = pub} ) mPair
    in maybeToList mKeyPair

testPrivateBs :: ByteString
testPrivateBs = "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

testPublicBs :: ByteString
testPublicBs = "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"

execCommands :: [P.Command ByteString] -> PactT [TransactionOutput]
execCommands _cmds = undefined

checkScientific :: Scientific -> TestResponse -> Assertion
checkScientific sci tr = do
  _trResultSuccess tr @? "resultSuccess was not set to True"
  parseScientific (P._arResult $ _trApiResult tr) @?= Just sci

parseScientific :: Value -> Maybe Scientific
parseScientific (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (Number sci) -> Just sci
    Just _ -> Nothing
parseScientific _ = Nothing

----------------------------------------------------------------------------------------------------
data TestRequest = TestRequest
    { _trCmd :: String
    , _trEval :: TestResponse -> Assertion
    , _trDisplayStr :: String
    }

instance Show TestRequest where
    show tr = "cmd: " ++ _trCmd tr ++ "\nDisplay string: " ++ _trDisplayStr tr

data TestResponse = TestResponse
    { _trResultSuccess :: Bool
    , _trApiResult :: P.ApiResult
    , _trBatchCount :: Int64
    } deriving (Eq, Generic)

instance Show TestResponse where
    show tr = "resultSuccess: " ++ show (_trResultSuccess tr) ++ "\n"
        ++ "Batch count: " ++ show (_trBatchCount tr) ++ "\n"
        ++ take 100 (show (_trApiResult tr)) ++ "..."

----------------------------------------------------------------------------------------------------
testPactRequests :: [TestRequest]
testPactRequests = [testReq1]

testReq1 :: TestRequest
testReq1 = TestRequest
    { _trCmd = "exec (+ 1 1)"
    , _trEval = (\tr -> checkScientific (scientific 2 0) tr)
    , _trDisplayStr = "Executes 1 + 1 in Pact and returns 2.0" }
