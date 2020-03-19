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

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
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
import Pact.Types.Hash

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
tests = ScheduledTest testName $ withDelegateMempool $ go
  where
    testName = "Chainweb.Test.Pact.PactInProcApi"
    go dm = testGroup testName
         [ test Warn $ goldenNewBlock "new-block-0" mpRef goldenMemPool
         , test Warn $ goldenNewBlock "empty-block-tests" mpRef mempty
         , test Warn $ newBlockAndValidate mpRef
         , test Warn $ newBlockRewindValidate mpRef
         , test Quiet $ badlistNewBlockTest mpRef
         ]
      where
        mpRef = fst <$> dm
        test logLevel =
          withPactTestBlockDb testVersion cid logLevel (snd <$> dm) defaultPactServiceConfig



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


newBlockAndValidate :: IO (IORef (String,MemPoolAccess)) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (q,bdb) <- reqIO
  setMempool refIO ("newBlockAndValidate",goldenMemPool)
  void $ runBlock q bdb second "newBlockAndValidate"

newBlockRewindValidate :: IO (IORef (String,MemPoolAccess)) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockRewindValidate mpRefIO reqIO = testCase "newBlockRewindValidate" $ do
  (q,bdb) <- reqIO
  setMempool mpRefIO ("newBlockRewindValidate",chainDataMemPool)
  cut0 <- readMVar $ _bdbCut bdb -- genesis cut

  -- cut 1a
  runBlock q bdb second "newBlockRewindValidate-1a"
  cut1a <- readMVar $ _bdbCut bdb

  -- rewind, cut 1b
  void $ swapMVar (_bdbCut bdb) cut0
  runBlock q bdb second "newBlockRewindValidate-1b"

  -- rewind to cut 1a to trigger replay with chain data bug
  void $ swapMVar (_bdbCut bdb) cut1a
  runBlock q bdb (secondsToTimeSpan 2) "newBlockRewindValidate-2"

  where

    chainDataMemPool = mempty {
      mpaGetBlock = \_ _ _ bh -> do
          fmap V.singleton $ buildCwCmd
            $ set cbSigners [mkSigner' sender00 []]
            $ set cbChainId (_blockChainId bh)
            $ set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)
            $ mkCmd (sshow bh) -- nonce is block height, sufficiently unique
            $ mkExec' "(chain-data)"
      }



badlistNewBlockTest :: IO (IORef (String,MemPoolAccess)) -> IO (PactQueue,TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlist-new-block-test" $ do
  (reqQ,_) <- reqIO
  badHashRef <- newIORef $ fromUntypedHash pactInitialHash
  badTx <- buildCwCmd
    $ set cbSigners [mkSigner' sender00 []]
    -- this should exceed the account balance
    $ set cbGasLimit 99999
    $ set cbGasPrice 1_000_000_000_000_000
    $ mkCmd "badListMPA"
    $ mkExec' "(+ 1 2)"
  setMempool mpRefIO $ ("badlistNewBlockTest",badlistMPA badTx badHashRef)
  newBlock noMiner (ParentHeader genesisHeader) reqQ
    >>= readMVar
    >>= expectFailureContaining "badlistNewBlockTest:newBlock" "Insufficient funds"
  badHash <- readIORef badHashRef
  assertEqual "Badlist should have badtx hash" (_cmdHash badTx) badHash
  where
    badlistMPA badTx badHashRef = mempty
      { mpaGetBlock = \_ _ _ _ -> return $ V.singleton badTx
      , mpaBadlistTx = writeIORef badHashRef
      }


goldenNewBlock :: String -> IO (IORef (String,MemPoolAccess)) -> MemPoolAccess -> IO (PactQueue,TestBlockDb) -> TestTree
goldenNewBlock label mpRefIO mp reqIO = golden label $ do
    (reqQ,_) <- reqIO
    setMempool mpRefIO (label,mp)
    respVar <- newBlock noMiner (ParentHeader genesisHeader) reqQ
    goldenBytes =<< takeMVar respVar
  where
    goldenBytes :: Y.ToJSON a => Exception e => Either e a -> IO BL.ByteString
    goldenBytes (Left e) = assertFailure $ label ++ ": " ++ show e
    goldenBytes (Right a) = return $ BL.fromStrict $ Y.encode $ object
      [ "test-group" .= ("new-block" :: T.Text)
      , "results" .= a
      ]

goldenMemPool :: MemPoolAccess
goldenMemPool = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock validate bHeight bHash parentHeader = do
        moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
        let txs =
              [ (T.pack moduleStr)
              , "(create-table test1.accounts)"
              , "(test1.create-global-accounts)"
              , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)"
              , "(at 'prev-block-hash (chain-data))"
              , "(at 'block-time (chain-data))"
              , "(at 'block-height (chain-data))"
              , "(at 'gas-limit (chain-data))"
              , "(at 'gas-price (chain-data))"
              , "(at 'chain-id (chain-data))"
              , "(at 'sender (chain-data))"
              ]
        outtxs' <- mkTxs txs
        -- the following is done post-hash which is lame but in
        -- the goldens. TODO boldly overwrite goldens at some point of
        -- great stability
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (pMeta . pmTTL)
            t = toTxCreationTime $ _bct $ _blockCreationTime parentHeader
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
    mkTxs txs =
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          set cbSigners [mkSigner' sender00 []] $
          set cbGasPrice 0.01 $
          set cbTTL 1_000_000 $ -- match old goldens
          mkCmd ("1" <> sshow n) $
          mkExec code $
          mkKeySetData "test-admin-keyset" [sender00]
    modifyPayloadWithText f pwt = mkPayloadWithText newPayload
      where
        oldPayload = payloadObj pwt
        newPayload = f oldPayload
