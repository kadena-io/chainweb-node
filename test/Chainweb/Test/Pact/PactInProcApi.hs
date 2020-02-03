{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Concurrent.MVar.Strict
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad (when)

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import GHC.Generics

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
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Version (ChainwebVersion(..), someChainId)

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

tests :: ScheduledTest
tests = ScheduledTest label $
    withRocksResource $ \rocksIO ->
    withTemporaryDir $ \dir ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withPayloadDb $ \pdb ->
    testGroup label
    [ withPact testVersion Warn pdb bhdb testMemPoolAccess dir 100000
          (newBlockTest "new-block-0")
    , after AllSucceed "new-block-0" $
      withPact testVersion Warn pdb bhdb testEmptyMemPool dir 100000
          (newBlockTest "empty-block-tests")
    , after AllSucceed "empty-block-tests" $
      withPact testVersion Quiet pdb bhdb badlistMPA dir 100000
          badlistNewBlockTest
    ]
  where
    label = "Chainweb.Test.Pact.PactInProcApi"
    genblock = genesisBlockHeader testVersion cid
    cid = someChainId testVersion

newBlockTest :: String -> IO PactQueue -> TestTree
newBlockTest label reqIO = golden label $ do
    reqQ <- reqIO
    let genesisHeader = genesisBlockHeader testVersion cid
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    respVar <- newBlock noMiner genesisHeader (BlockCreationTime blockTime) reqQ
    goldenBytes "new-block" =<< takeMVar respVar
  where
    cid = someChainId testVersion


data GotTxBadList = GotTxBadList
  deriving (Show, Generic)
instance Exception GotTxBadList

badlistMPA :: MemPoolAccess
badlistMPA = mempty
    { mpaGetBlock = \_ _ _ _ -> getBadBlock
    , mpaBadlistTx = \_ -> throwIO GotTxBadList
    }
  where
    getBadBlock = do
        d <- adminData
        let inputs = V.fromList [ PactTransaction "(+ 1 2)" d ]
        txs0 <- goldenTestTransactions inputs
        let setGP = modifyPayloadWithText . set (pMeta . pmGasPrice)
        let setGL = modifyPayloadWithText . set (pMeta . pmGasLimit)
        -- this should exceed the account balance
        let txs = flip V.map txs0 $
                  fmap (setGP 1000000000000000 . setGL 99999)
        return txs

badlistNewBlockTest :: IO PactQueue -> TestTree
badlistNewBlockTest reqIO = testCase "badlist-new-block-test" $ do
    reqQ <- reqIO
    expectBadlistException $ do
        m <- newBlock noMiner genesisHeader blockTime reqQ
        takeMVar m >>= either throwIO (const (return ()))
  where
    cid = someChainId testVersion
    genesisHeader = genesisBlockHeader testVersion cid
    blockTime = BlockCreationTime $ Time $ secondsToTimeSpan $
                Seconds $ succ 1000000
    -- verify that the pact service attempts to badlist the bad tx at mempool
    expectBadlistException m = do
        let wrap = m >> fail "expected exception"
        let onEx (e :: PactException) =
                if "GotTxBadList" `isInfixOf` show e
                  then return ()
                  else throwIO e
        wrap `catch` onEx

_localTest :: IO PactQueue -> ScheduledTest
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
_getBlockHeaders cid n = gbh0 : take (n - 1) (testBlockHeaders $ ParentHeader gbh0)
  where
    gbh0 = genesisBlockHeader testVersion cid

-- moved here, should NEVER be in production code:
-- you can't modify a payload without recomputing hash/signatures
modifyPayloadWithText
    :: (Payload PublicMeta ParsedCode -> Payload PublicMeta ParsedCode)
    -> PayloadWithText
    -> PayloadWithText
modifyPayloadWithText f pwt = mkPayloadWithText newPayload
  where
    oldPayload = payloadObj pwt
    newPayload = f oldPayload

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = mempty
    { mpaGetBlock = \validate bh hash _header ->
        getTestBlock validate bh hash
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
            g = modifyPayloadWithText . set (pMeta . pmTTL)
        outtxs' <- goldenTestTransactions txs
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap ((g ttl) . (f (TxCreationTime $ ParsedInteger 1000000))) tx
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
testEmptyMemPool = mempty
    { mpaGetBlock = \_ _ _ _ -> goldenTestTransactions V.empty
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
