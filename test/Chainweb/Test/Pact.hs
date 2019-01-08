{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.API as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Server as P

import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson (Value(..))
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Scientific
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Pact unit tests" pactExecTests
    -- [ testGroup "Simple Pact Execution"
        -- [testCase "simpleExec" simpleExec]
    -- ]

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
execTests = undefined

----------------------------------------------------------------------------------------------------
data TestRequest = TestRequest
    { cmd :: String
    , eval :: TestResponse -> Assertion
    , displayStr :: String
    }

instance Show TestRequest where
    show tr = "cmd: " ++ cmd tr ++ "\nDisplay string: " ++ displayStr tr

data TestResponse = TestResponse
    { resultSuccess :: Bool
    , apiResult :: P.ApiResult
    , _batchCount :: Int64
    } deriving (Eq, Generic)

instance Show TestResponse where
    show tr = "resultSuccess: " ++ show (resultSuccess tr) ++ "\n"
        ++ "Batch count: " ++ show (_batchCount tr) ++ "\n"
        ++ take 100 (show (apiResult tr)) ++ "..."

----------------------------------------------------------------------------------------------------
checkScientific :: Scientific -> TestResponse -> Assertion
checkScientific sci tr = do
  --resultSuccess tr `shouldBe` True
  --parseScientific (P._arResult $ apiResult tr) `shouldBe` Just sci
  resultSuccess tr @? "resultSuccess was not set to True"
  parseScientific (P._arResult $ apiResult tr) @?= Just sci

parseScientific :: Value -> Maybe Scientific
parseScientific (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (Number sci) -> Just sci
    Just _ -> Nothing
parseScientific _ = Nothing

----------------------------------------------------------------------------------------------------
testReq1 :: TestRequest
testReq1 = TestRequest
    { cmd = "exec (+ 1 1)"
    , eval = (\tr -> checkScientific (scientific 2 0) tr)
    , displayStr = "Executes 1 + 1 in Pact and returns 2.0" }
