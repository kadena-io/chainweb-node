{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader

import Data.Aeson
import Data.Default (def)
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String.Conv (toS)
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import GHC.Generics (Generic)

import System.IO.Extra

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Gas
import Pact.Types.Logger
import qualified Pact.Types.Runtime as P
import Pact.Types.Server

import Chainweb.BlockHash
import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Version (ChainwebVersion(..), someChainId)

testVersion :: ChainwebVersion
testVersion = Testnet00

tests :: ScheduledTest
tests = testGroupSch "Simple pact execution tests"
    [ withPactCtx $ \ctx -> testGroup "single transactions"
        $ schedule Sequential
            [ execTest ctx testReq2
            , execTest ctx testReq3
            , execTest ctx testReq4
            , execTest ctx testReq5
            ]
    , withPactCtx $ \ctx2 -> _schTest $ execTest ctx2 testReq6
    ]

-- -------------------------------------------------------------------------- --
-- Pact test datatypes

-- | A test request is comprised of a list of commands, a textual discription,
-- and an test runner function, that turns an IO acttion that produces are
-- 'TestResponse' into a 'TestTree'.
--
data TestRequest = TestRequest
    { _trCmds :: ![TestSource]
    , _trDisplayStr :: !String
    , _trEval :: !(IO TestResponse -> ScheduledTest)
    }

data TestSource = File FilePath | Code String
  deriving (Show, Generic, ToJSON)

data TestResponse = TestResponse
    { _trOutputs :: ![(TestSource, FullLogTxOutput)]
    , _trCoinBaseOutput :: !FullLogTxOutput
    }
    deriving (Generic, ToJSON)

data PactTestSetup = PactTestSetup
  { _pactServiceEnv :: !PactServiceEnv
  , _pactDbState :: !PactDbState
  }

-- -------------------------------------------------------------------------- --
-- sample data

testReq2 :: TestRequest
testReq2 = TestRequest
    { _trCmds = [ File "test1.pact" ]
    , _trEval = \f -> testCaseSch "load module" $ do
        (TestResponse [res] _) <- f
        checkSuccessOnly (snd res)
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
    , _trEval = fileCompareTxLogs "testReq6"
    , _trDisplayStr = "Transfers from one account to another"
    }

-- -------------------------------------------------------------------------- --
-- Utils

-- | This enforces that only a single test can use the pact context at a time.
-- It's up to the user to ensure that tests are scheduled in the right order.
--
withPactCtx :: ((forall a . PactServiceM a -> IO a) -> TestTree) -> TestTree
withPactCtx f
    = withResource start stop $ \ctxIO -> f $ \pact -> do
        (pactStateVar, env) <- ctxIO
        modifyMVar pactStateVar $ \s -> do
            (a,s') <- runStateT (runReaderT pact env) s
            return (s',a)

  where
    start :: IO (MVar PactServiceState, PactServiceEnv)
    start = do
        (PactTestSetup env dbSt) <- pactTestSetup
        let pss = PactServiceState dbSt Nothing
            cid = someChainId testVersion
        pss' <- flip execStateT pss $ flip runReaderT env $ do
            initialPayloadState testVersion cid
        pactStateVar <- newMVar pss'
        return (pactStateVar, env)

    stop :: (MVar PactServiceState, PactServiceEnv) -> IO ()
    stop (var, _) = void $ takeMVar var

pactTestSetup :: IO PactTestSetup
pactTestSetup = do
    (cpe, theState) <-
        case _ccSqlite cmdConfig of
            Nothing -> do
                env <- mkPureEnv loggers
                liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- mkSQLiteEnv logger False sqlc loggers
                liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)

    void $! saveInitial (_cpeCheckpointer cpe) theState
    let env = PactServiceEnv Nothing cpe P.noSPVSupport def

    pure $ PactTestSetup env theState
  where
    loggers = pactTestLogger
    logger = newLogger loggers $ LogName "PactService"
    pactCfg = pactDbConfig testVersion
    cmdConfig = toCommandConfig pactCfg
    gasLimit = fromMaybe 0 (_ccGasLimit cmdConfig)
    gasRate = fromMaybe 0 (_ccGasRate cmdConfig)
    gasEnv = GasEnv (fromIntegral gasLimit) 0.0 (constGasModel (fromIntegral gasRate))

execTest :: (forall a . PactServiceM a -> IO a) -> TestRequest -> ScheduledTest
execTest runPact request = _trEval request $ do
    cmdStrs <- mapM getPactCode $ _trCmds request
    trans <- mkPactTestTransactions $ V.fromList cmdStrs
    results <- runPact $ execTransactions (Just nullBlockHash) defaultMiner trans
    let outputs = V.toList $ snd <$> _transactionPairs results
    return $ TestResponse
        (zip (_trCmds request) outputs)
        (_transactionCoinbase results)

getPactCode :: TestSource -> IO String
getPactCode (Code str) = return str
getPactCode (File filePath) = readFile' $ testPactFilesDir ++ filePath

checkSuccessOnly :: FullLogTxOutput -> Assertion
checkSuccessOnly resp =
    case _flCommandResult resp of
        (Object o) -> HM.lookup "status" o @?= Just "success"
        _ -> assertFailure "Status returned does not equal \"success\""

-- | A test runner for golden tests.
--
fileCompareTxLogs :: String -> IO TestResponse -> ScheduledTest
fileCompareTxLogs label respIO = pactGoldenSch label $ do
    resp <- respIO
    return $ toS $ Y.encode
        $ coinbase (_trCoinBaseOutput resp)
        : (result <$> _trOutputs resp)
  where
    result (cmd, out) = object
        [ "output" .= _flTxLogs out
        , "cmd" .= cmd
        ]
    coinbase out = object
        [ "output" .= _flTxLogs out
        , "cmd" .= ("coinbase" :: String)
        ]
