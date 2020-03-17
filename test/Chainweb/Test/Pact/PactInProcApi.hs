{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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
import Control.Monad

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
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Cut.TestBlockDb
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVersion

genesisHeader :: BlockHeader
genesisHeader = genesisBlockHeader testVersion cid

tests :: ScheduledTest
tests = ScheduledTest label $
    withResource (newMVar "1") (const $ return()) $ \noncer ->
    testGroup label
    [ withPactTestBlockDb testVersion cid Warn testMemPoolAccess conf
          (newBlockTest "new-block-0")
    , withPactTestBlockDb testVersion cid Warn testMemPoolAccess conf
          newBlockAndValidate
    , withPactTestBlockDb testVersion cid Warn (testMempoolChainData noncer) conf
          (newBlockRewindValidate noncer)
    , withPactTestBlockDb testVersion cid Warn testEmptyMemPool conf
          (newBlockTest "empty-block-tests")
    , withPactTestBlockDb testVersion cid Quiet badlistMPA  conf
          badlistNewBlockTest
    ]
  where
    label = "Chainweb.Test.Pact.PactInProcApi"
    conf = defaultPactServiceConfig


newBlockTest :: String -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockTest label reqIO = golden label $ do
    (reqQ,_) <- reqIO
    respVar <- newBlock noMiner (ParentHeader genesisHeader) reqQ
    goldenBytes "new-block" =<< takeMVar respVar

forSuccess :: String -> IO (MVar (Either PactException a)) -> IO a
forSuccess msg mvio = (`catch` handler) $ do
  mv <- mvio
  takeMVar mv >>= \r -> case r of
    Left e -> assertFailure $ msg ++ ": got failure result: " ++ show e
    Right v -> return v
  where
    handler (e :: SomeException) = assertFailure $ msg ++ ": exception thrown: " ++ show e

runBlock :: PactQueue -> TestBlockDb -> TimeSpan Micros -> String -> IO ()
runBlock q bdb timeOffset msg = do
  ph <- getParentTestBlockDb bdb cid
  let blockTime = add timeOffset $ _bct $ _blockCreationTime ph
  nb <- forSuccess (msg <> ": newblock") $
        newBlock noMiner (ParentHeader ph) q
  forM_ (chainIds testVersion) $ \c -> do
    let o | c == cid = nb
          | otherwise = emptyPayload
    addTestBlockDb bdb (Nonce 0) (\_ _ -> blockTime) c o
  nextH <- getParentTestBlockDb bdb cid
  void $ forSuccess "newBlockAndValidate: validate" $
       validateBlock nextH (payloadWithOutputsToPayloadData nb) q


newBlockAndValidate :: IO (PactQueue,TestBlockDb) -> TestTree
newBlockAndValidate reqIO = testCase "newBlockAndValidate" $ do
  (q,bdb) <- reqIO
  void $ runBlock q bdb second "newBlockAndValidate"

newBlockRewindValidate :: IO (MVar T.Text) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockRewindValidate noncer reqIO = testCase "newBlockRewindValidate" $ do
  (q,bdb) <- reqIO
  nonce <- noncer
  cut0 <- readMVar $ _bdbCut bdb -- genesis cut

  -- cut 1a
  runBlock q bdb second "newBlockRewindValidate-1a"
  cut1a <- readMVar $ _bdbCut bdb

  -- rewind, cut 1b
  void $ swapMVar (_bdbCut bdb) cut0
  void $ swapMVar nonce "1'"
  runBlock q bdb second "newBlockRewindValidate-1b"

  -- rewind to cut 1a to trigger replay with chain data bug
  void $ swapMVar (_bdbCut bdb) cut1a
  void $ swapMVar nonce "2"
  runBlock q bdb (secondsToTimeSpan 2) "newBlockRewindValidate-2"






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
                  fmap (setGP 1_000_000_000_000_000 . setGL 99999)
        return txs

badlistNewBlockTest :: IO (PactQueue,TestBlockDb) -> TestTree
badlistNewBlockTest reqIO = testCase "badlist-new-block-test" $ do
    (reqQ,_) <- reqIO
    expectBadlistException $ do
        m <- newBlock noMiner (ParentHeader genesisHeader) reqQ
        takeMVar m >>= either throwIO (const (return ()))
  where
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
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock validate bHeight bHash parentHeader = do
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
            t = toTxCreationTime $ _bct $ _blockCreationTime parentHeader
        outtxs' <- goldenTestTransactions txs
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap (g ttl . f t) tx
        oks <- validate bHeight bHash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "tx failed validation! input list: \n"
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

testMempoolChainData :: IO (MVar T.Text) -> MemPoolAccess
testMempoolChainData noncer = mempty {
  mpaGetBlock = \_ _ _ bh -> ts bh
  }
  where
    ts bh = do
      let txs = V.fromList [ PactTransaction "(chain-data)" Nothing ]
          c = P.ChainId $ sshow (chainIdInt (_blockChainId bh) :: Integer)
          txTime = toTxCreationTime $ _bct $ _blockCreationTime bh
          txTtl = 1000 -- seconds
      n <- readMVar =<< noncer
      ks <- testKeyPairs sender00KeyPair Nothing
      mkTestExecTransactions "sender00" c ks n 10_000 0.01 txTtl txTime txs


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
