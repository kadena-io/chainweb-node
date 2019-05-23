{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Data.Aeson
import Data.String.Conv (toS)
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Data.Text (Text, pack)

import GHC.Generics (Generic)

import System.IO.Extra

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHash
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Version (ChainwebVersion(..))

import Pact.Types.Command

testVersion :: ChainwebVersion
testVersion = Testnet00

tests :: ScheduledTest
tests = testGroupSch "Simple pact execution tests"
    [ withPactCtx testVersion Nothing $ \ctx -> testGroup "single transactions"
        $ schedule Sequential
            [ execTest ctx testReq2
            , execTest ctx testReq3
            , execTest ctx testReq4
            , execTest ctx testReq5
            ]
    , withPactCtx testVersion Nothing $ \ctx2 -> _schTest $ execTest ctx2 testReq6
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
    { _trOutputs :: ![(TestSource, HashCommandResult)]
    , _trCoinBaseOutput :: !HashCommandResult
    }
    deriving (Generic, ToJSON)

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

-- -------------------------------------------------------------------------- --
-- Utils

execTest :: (forall a . PactServiceM a -> IO a) -> TestRequest -> ScheduledTest
execTest runPact request = _trEval request $ do
    cmdStrs <- mapM getPactCode $ _trCmds request
    d <- adminData
    trans <- goldenTestTransactions
      $ V.fromList
      $ fmap (k d) cmdStrs

    results <- runPact $ execTransactions (Just nullBlockHash) defaultMiner trans
    let outputs = V.toList $ snd <$> _transactionPairs results
    return $ TestResponse
        (zip (_trCmds request) outputs)
        (_transactionCoinbase results)
  where
    k d c = PactTransaction c d

getPactCode :: TestSource -> IO Text
getPactCode (Code str) = return (pack str)
getPactCode (File filePath) = pack <$> readFile' (testPactFilesDir ++ filePath)

checkSuccessOnly :: HashCommandResult -> Assertion
checkSuccessOnly CommandResult{..} = case _crResult of
  PactResult (Right _) -> return ()
  r -> assertFailure $ "Failure status returned: " ++ show r

checkSuccessOnly' :: String -> IO TestResponse -> ScheduledTest
checkSuccessOnly' msg f = testCaseSch msg $ do
        f' <- f
        case f' of
          (TestResponse res@(_:_) _) ->
            checkSuccessOnly (snd $ last res)
          (TestResponse res _) -> fail (show res) -- TODO

-- | A test runner for golden tests.
--
fileCompareTxLogs :: String -> IO TestResponse -> ScheduledTest
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
