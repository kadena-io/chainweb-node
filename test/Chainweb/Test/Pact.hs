-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb


module Chainweb.Test.Pact where

import qualified Pact.Gas as P
import qualified Pact.Types.Logger as P
import Test.Tasty

tests :: TestTree
tests = testGroup "Chainweb Pact tests"
    [ simpleExecTests
    ]

simpleExecTests :: TestTree
simpleExecTests = do
  testGroup "Simple Pact execution tests"
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
       liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv) (mkPureState env cmdConfig)
     Just sqlc -> do
       env <- P.mkSQLiteEnv logger False sqlc loggers
       liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv) (mkSQLiteState env cmdConfig)
  void $ runRWST serviceRequests checkpointEnv theState

execSimple :: PactT
execSimple = undefined

----------------------------------------------------------------------------------------------------
data TestRequest = TestRequest
  { cmd :: String
  , eval :: TestResponse -> Expectation
  , displayStr :: String
  }

instance Show TestRequest where
  show tr = "cmd: " ++ cmd tr ++ "\nDisplay string: " ++ displayStr tr

data TestResponse = TestResponse
  { resultSuccess :: Bool
  , apiResult :: ApiResult
  , _batchCount :: Int64
  } deriving (Eq, Generic)

instance Show TestResponse where
  show tr = "resultSuccess: " ++ show (resultSuccess tr) ++ "\n"
    ++ "Batch count: " ++ show (_batchCount tr) ++ "\n"
    ++ take 100 (show (apiResult tr)) ++ "..."

----------------------------------------------------------------------------------------------------
testReq1 :: TestRequest
testReq1 = TestRequest
  { cmd = "exec (+ 1 1)"
  , eval = (\tr -> checkScientific (scientific 2 0) tr)
  , displayStr = "Executes 1 + 1 in Pact and returns 2.0" }
