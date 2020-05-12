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

import Data.Aeson (object, (.=), Value(..))
import Data.Either (isRight)
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
import Pact.Types.Persistence

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Test.Cut.TestBlockDb
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
tests = ScheduledTest testName $ go
  where
    testName = "Chainweb.Test.Pact.PactInProcApi"
    go = testGroup testName
         [ test Warn $ goldenNewBlock "new-block-0" goldenMemPool
         , test Warn $ goldenNewBlock "empty-block-tests" mempty
         , test Warn $ newBlockAndValidate
         , test Warn $ newBlockRewindValidate
         , test Quiet $ getHistory
         , test Quiet $ badlistNewBlockTest
         , test Warn $ mempoolCreationTimeTest
         , test Warn $ moduleNameFork
         ]
      where
        test logLevel f =
          withDelegateMempool $ \dm ->
          withPactTestBlockDb testVersion cid logLevel (snd <$> dm) defaultPactServiceConfig $
          f (fst <$> dm)



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


newBlockAndValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (q,bdb) <- reqIO
  setMempool refIO goldenMemPool
  void $ runBlock q bdb second "newBlockAndValidate"


getHistory :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (q,bdb) <- reqIO
  setMempool refIO goldenMemPool
  void $ runBlock q bdb second "getHistory"
  h <- getParentTestBlockDb bdb cid
  mv <- pactBlockTxHistory h (Domain' (UserTables "coin_coin-table")) q
  (BlockTxHistory hist) <- forSuccess "getHistory" (return mv)
  -- just check first one here
  assertEqual "check first entry of history"
    (10,[TxLog "coin_coin-table" "sender00"
         (object
          [
            "guard" .= object [ "pred" .= ("keys-all" :: T.Text),
                                "keys" .= ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text] ]
          , "balance" .= (Number 9.99999e7)
          ])])
    (V.head hist)
  -- and transaction txids
  assertEqual "check txids"
    (V.fromList [10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42])
    (fmap fst hist)


newBlockRewindValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockRewindValidate mpRefIO reqIO = testCase "newBlockRewindValidate" $ do
  (q,bdb) <- reqIO
  setMempool mpRefIO chainDataMemPool
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

moduleNameFork :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
moduleNameFork mpRefIO reqIO = testCase "moduleNameFork" $ do

  (q,bdb) <- reqIO

  -- install in free in block 1
  setMempool mpRefIO (moduleNameMempool "free" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- install in user in block 2
  setMempool mpRefIO (moduleNameMempool "user" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- do something else post-fork
  setMempool mpRefIO (moduleNameMempool "free" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"
  setMempool mpRefIO (moduleNameMempool "user" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- TODO this test doesn't actually validate, I turn on Debug and make sure it
  -- goes well.

moduleNameMempool :: T.Text -> T.Text -> MemPoolAccess
moduleNameMempool ns mn = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock _ _ _ bh = do
        let txs =
              [ "(namespace '" <> ns <> ") (module " <> mn <> " G (defcap G () (enforce false 'cannotupgrade)))"
              , ns <> "." <> mn <> ".G"
              ]
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          set cbSigners [mkSigner' sender00 []] $
          set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh) $
          mkCmd ("1" <> sshow n) $
          mkExec' code


mempoolCreationTimeTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
mempoolCreationTimeTest mpRefIO reqIO = testCase "mempoolCreationTimeTest" $ do

  (q,bdb) <- reqIO

  let start@(Time startSpan) :: Time Micros = Time (TimeSpan (Micros 100_000_000))
      s30 = scaleTimeSpan (30 :: Int) second
      s15 = scaleTimeSpan (15 :: Int) second
  -- b1 block time is start
  void $ runBlock q bdb startSpan "mempoolCreationTimeTest-1"


  -- do pre-insert check with transaction at start + 15s
  tx <- makeTx "tx-now" (add s15 start)
  void $ forSuccess "mempoolCreationTimeTest: pre-insert tx" $
    pactPreInsertCheck (V.singleton tx) q

  setMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30 "mempoolCreationTimeTest-2"

  where

    makeTx nonce t = buildCwCmd
        $ set cbSigners [mkSigner' sender00 []]
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime $ t)
        $ set cbTTL 300
        $ mkCmd (sshow t <> nonce)
        $ mkExec' "1"
    mp tx = mempty {
      mpaGetBlock = \valid _ _ bh -> getBlock bh tx valid
      }

    getBlock bh tx valid = do
      let txs = V.singleton tx
      oks <- valid (_blockHeight bh) (_blockHash bh) txs
      unless (V.and oks) $ throwIO $ userError "Insert failed"
      return txs


badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
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
  setMempool mpRefIO (badlistMPA badTx badHashRef)
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


goldenNewBlock :: String -> MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
goldenNewBlock label mp mpRefIO reqIO = golden label $ do
    (reqQ,_) <- reqIO
    setMempool mpRefIO mp
    resp <- forSuccess ("goldenNewBlock:" ++ label) $
      newBlock noMiner (ParentHeader genesisHeader) reqQ
    -- ensure all golden txs succeed
    forM_ (_payloadWithOutputsTransactions resp) $ \(txIn,TransactionOutput out) -> do
      cr :: CommandResult Hash <- decodeStrictOrThrow out
      assertSatisfies ("golden tx succeeds, input: " ++ show txIn) (_crResult cr) (isRight . (\(PactResult r) -> r))
    goldenBytes resp
  where
    goldenBytes :: PayloadWithOutputs -> IO BL.ByteString
    goldenBytes a = return $ BL.fromStrict $ Y.encode $ object
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
              , "(create-table free.test1.accounts)"
              , "(free.test1.create-global-accounts)"
              , "(free.test1.transfer \"Acct1\" \"Acct2\" 1.00)"
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
