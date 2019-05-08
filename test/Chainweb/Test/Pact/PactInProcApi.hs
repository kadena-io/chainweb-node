{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb
--
module Chainweb.Test.Pact.PactInProcApi
( tests
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM
import Control.Exception (Exception)

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Transaction
import Chainweb.Version (ChainwebVersion(..), someChainId)

import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.PactQueue (sendCloseMsg)

testVersion :: ChainwebVersion
testVersion = Testnet00

tests :: ScheduledTest
tests = testGroupSch "Chainweb.Test.Pact.PactInProcApi"
    [ withPact testMemPoolAccess $ \reqQIO -> testGroup "pact tests"
        $ schedule Sequential
            [ validateTest reqQIO
            , localTest reqQIO
            ]
    , withPact testMemPoolAccess $ \reqQIO ->
        newBlockTest "new-block-0" reqQIO
    , withPact testEmptyMemPool $ \reqQIO ->
        newBlockTest "empty-block-tests" reqQIO
    ]

withPact :: MemPoolAccess -> (IO (TQueue RequestMsg) -> TestTree) -> TestTree
withPact mempool f = withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically newTQueue
        a <- async (PS.initPactService testVersion cid logger reqQ mempool mv)
        return (a, reqQ)

    stopPact (a, reqQ) = do
        sendCloseMsg reqQ
        cancel a

    logger = genericLogger Warn T.putStrLn
    cid = someChainId testVersion

newBlockTest :: String -> IO (TQueue RequestMsg) -> TestTree
newBlockTest label reqIO = golden label $ do
    reqQ <- reqIO
    let genesisHeader = genesisBlockHeader testVersion cid
    respVar <- newBlock noMiner genesisHeader reqQ
    goldenBytes "new-block" =<< takeMVar respVar
  where
    cid = someChainId testVersion

validateTest :: IO (TQueue RequestMsg) -> ScheduledTest
validateTest reqIO = goldenSch "validateBlock-0" $ do
    reqQ <- reqIO
    let genesisHeader = genesisBlockHeader testVersion cid
    respVar0 <- newBlock noMiner genesisHeader reqQ
    plwo <- takeMVar respVar0 >>= \case
        Left e -> assertFailure (show e)
        Right r -> return r

    -- validate the same transactions sent to newBlockTest above
    let matchingPlHash = _payloadWithOutputsPayloadHash plwo
    let plData = PayloadData
            { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions plwo
            , _payloadDataMiner = _payloadWithOutputsMiner plwo
            , _payloadDataPayloadHash = matchingPlHash
            , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash plwo
            , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash plwo
            }

    let headers = V.fromList $ getBlockHeaders cid 2
    let toValidateHeader = (headers ! 1)
            { _blockPayloadHash = matchingPlHash
            , _blockParent = _blockHash genesisHeader
            }
    respVar1 <- validateBlock toValidateHeader plData reqQ
    goldenBytes "validateBlock-0" =<< takeMVar respVar1
  where
    cid = someChainId testVersion

localTest :: IO (TQueue RequestMsg) -> ScheduledTest
localTest reqIO = goldenSch "local" $ do
    reqQ <- reqIO
    locVar0c <- testLocal >>= \t -> local t reqQ
    goldenBytes "local" =<< takeMVar locVar0c

goldenBytes :: Y.ToJSON a => Exception e => String -> Either e a -> IO BL.ByteString
goldenBytes label (Left e) = assertFailure $ label ++ ": " ++ show e
goldenBytes label (Right a) = return $ BL.fromStrict $ Y.encode $ object
    [ "test-group" .= label
    , "results" .= a
    ]

getBlockHeaders :: ChainId -> Int -> [BlockHeader]
getBlockHeaders cid n = gbh0 : take (n - 1) (testBlockHeaders gbh0)
  where
    gbh0 = genesisBlockHeader testVersion cid

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess _bHeight _bHash = do
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let cmdStrs = V.fromList
          [ moduleStr
          , "(create-table test1.accounts)"
          , "(test1.create-global-accounts)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)"
          ]
    mkPactTestTransactions cmdStrs

testEmptyMemPool :: MemPoolAccess
testEmptyMemPool _bHeight _bHash = mkPactTestTransactions V.empty

testLocal :: IO ChainwebTransaction
testLocal = fmap (head . V.toList) $ mkPactTestTransactions $ V.fromList
    [ "(test1.read-account \"Acct1\")"
    ]

{-
cmdBlocks :: Vector (Vector String)
cmdBlocks =  V.fromList
    [ V.fromList
          [ "(test1.transfer \"Acct1\" \"Acct2\" 5.00)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 6.00)" ]
    , V.fromList
          [ "(test1.transfer \"Acct1\" \"Acct2\" 10.00)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 11.00)" ]
    ]
-}
