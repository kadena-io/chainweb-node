{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API in Chainweb
--
module Chainweb.Test.Pact.PactInProcApi
( tests
) where

import Control.Concurrent.Async
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (when)

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService as PS
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue (sendCloseMsg)
import Chainweb.Pact.Service.Types
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils (withLink)
import Chainweb.Version (ChainwebVersion(..), someChainId)
import Data.CAS.RocksDB

testVersion :: ChainwebVersion
testVersion = Development

tests :: ScheduledTest
tests = ScheduledTest label $ withRocksResource $ \rocksIO ->
        testGroup label
    [ withTime $ \t -> withPact rocksIO (testMemPoolAccess t) $ \reqQIO ->
        newBlockTest "new-block-0" reqQIO
    , withPact rocksIO testEmptyMemPool $ \reqQIO ->
        newBlockTest "empty-block-tests" reqQIO
    ]
  where
    label = "Chainweb.Test.Pact.PactInProcApi"

withPact
    :: IO RocksDb
    -> MemPoolAccess
    -> (IO (TQueue RequestMsg) -> TestTree)
    -> TestTree
withPact rocksIO mempool f = withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically newTQueue
        rdb <- rocksIO
        let genesisHeader = genesisBlockHeader testVersion cid
        bhdb <- testBlockHeaderDb rdb genesisHeader
        pdb <- newPayloadDb

        a <- withLink $ withTempDir $ \dir ->
             PS.initPactService testVersion cid logger reqQ mempool mv
                                bhdb pdb (Just dir) Nothing False
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
    creationTime <- getCurrentTimeIntegral
    respVar <- newBlock noMiner genesisHeader (BlockCreationTime creationTime) reqQ
    goldenBytes "new-block" =<< takeMVar respVar
  where
    cid = someChainId testVersion

_localTest :: IO (TQueue RequestMsg) -> ScheduledTest
_localTest reqIO = goldenSch "local" $ do
    reqQ <- reqIO
    locVar0c <- _testLocal >>= \t -> local t reqQ
    goldenBytes "local" =<< takeMVar locVar0c

goldenBytes :: Y.ToJSON a => Exception e => String -> Either e a -> IO BL.ByteString
goldenBytes label (Left e) = assertFailure $ label ++ ": " ++ show e
goldenBytes label (Right a) = return $ BL.fromStrict $ Y.encode $ object
    [ "test-group" .= label
    , "results" .= a
    ]

_getBlockHeaders :: ChainId -> Int -> [BlockHeader]
_getBlockHeaders cid n = gbh0 : take (n - 1) (testBlockHeaders gbh0)
  where
    gbh0 = genesisBlockHeader testVersion cid

testMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
testMemPoolAccess iot = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header ->
        getTestBlock validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    getTestBlock validate bHeight bHash = do
        moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
        d <- adminData
        let txs = V.fromList
              [ PactTransaction (T.pack moduleStr) d
              , PactTransaction "(create-table test1.accounts)" d
              , PactTransaction "(test1.create-global-accounts)" d
              , PactTransaction "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" d
              , PactTransaction "(at 'prev-block-hash (chain-data))" d
              , PactTransaction "(at 'block-time (chain-data))" d
              , PactTransaction "(at 'block-height (chain-data))" d
              , PactTransaction "(at 'gas-limit (chain-data))" d
              , PactTransaction "(at 'gas-price (chain-data))" d
              , PactTransaction "(at 'chain-id (chain-data))" d
              , PactTransaction "(at 'sender (chain-data))" d
              ]
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
        outtxs' <- goldenTestTransactions txs
        (Time t) <- iot
        let outtxs = flip V.map outtxs' $ \tx -> case timeSpanToSeconds t of
              Seconds s ->
                fmap (f (TxCreationTime $ ParsedInteger s)) tx
        oks <- validate bHeight bHash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show txs
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks ]
        return outtxs


testEmptyMemPool :: MemPoolAccess
testEmptyMemPool = MemPoolAccess
    { mpaGetBlock = \_ _ _ _ -> goldenTestTransactions V.empty
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }

_testLocal :: IO ChainwebTransaction
_testLocal = do
    d <- adminData
    fmap (head . V.toList)
      $ goldenTestTransactions
      $ V.fromList [ PactTransaction "(test1.read-account \"Acct1\")" d ]
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
