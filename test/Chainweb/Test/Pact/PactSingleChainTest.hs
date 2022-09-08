{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Test.Pact.PactSingleChainTest
( tests
) where

import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch


import Data.Aeson (object, (.=), Value(..), decode)
import Data.CAS.RocksDB
import Data.Either (isRight)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit


-- internal modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Persistence
import Pact.Types.PactError
import Pact.Types.RPC

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.ChainId
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation hiding (local)
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.PactService.ExecBlock
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Utils

testVersion :: ChainwebVersion
testVersion = FastTimedCPM peterson

cid :: ChainId
cid = someChainId testVersion

genesisHeader :: BlockHeader
genesisHeader = genesisBlockHeader testVersion cid

tests :: RocksDb -> ScheduledTest
tests rdb = ScheduledTest testName go
  where
    testName = "Chainweb.Test.Pact.PactSingleChainTest"
    go = testGroup testName
         [ test Warn $ goldenNewBlock "new-block-0" goldenMemPool
         , test Warn $ goldenNewBlock "empty-block-tests" mempty
         , test Warn $ newBlockAndValidate
         , test Warn $ newBlockRewindValidate
         , test Quiet $ getHistory
         , test Quiet $ testHistLookup1
         , test Quiet $ testHistLookup2
         , test Quiet $ testHistLookup3
         , test Quiet $ badlistNewBlockTest
         , test Warn $ mempoolCreationTimeTest
         , test Warn $ moduleNameFork
         , test Warn $ mempoolRefillTest
         , test Quiet $ blockGasLimitTest
         ]
      where
        test logLevel f =
          withDelegateMempool $ \dm ->
          withPactTestBlockDb testVersion cid logLevel rdb (snd <$> dm) defaultPactServiceConfig $
          f (fst <$> dm)

        testHistLookup1 = getHistoricalLookupNoTxs "sender00"
          (assertSender00Bal 100_000_000 "check latest entry for sender00 after a no txs block")
        testHistLookup2 = getHistoricalLookupNoTxs "randomAccount"
          (assertEqual "Return Nothing if key absent after a no txs block" Nothing)
        testHistLookup3 = getHistoricalLookupWithTxs "sender00"
          (assertSender00Bal 9.999998051e7 "check latest entry for sender00 after block with txs")


forSuccess :: NFData a => String -> IO (MVar (Either PactException a)) -> IO a
forSuccess msg mvio = (`catchAllSynchronous` handler) $ do
  mv <- mvio
  takeMVar mv >>= \case
    Left e -> assertFailure $ msg ++ ": got failure result: " ++ show e
    Right v -> return v
  where
    handler e = assertFailure $ msg ++ ": exception thrown: " ++ show e

runBlock :: PactQueue -> TestBlockDb -> TimeSpan Micros -> String -> IO PayloadWithOutputs
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
  forSuccess "newBlockAndValidate: validate" $
       validateBlock nextH (payloadWithOutputsToPayloadData nb) q

newBlockAndValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockAndValidate refIO reqIO = testCase "newBlockAndValidate" $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second "newBlockAndValidate"


getHistory :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
getHistory refIO reqIO = testCase "getHistory" $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second "getHistory"
  h <- getParentTestBlockDb bdb cid
  mv <- pactBlockTxHistory h (Domain' (UserTables "coin_coin-table")) q

  (BlockTxHistory hist prevBals) <- forSuccess "getHistory" (return mv)
  -- just check first one here
  assertEqual "check first entry of history"
    (Just [TxLog "coin_coin-table" "sender00"
      (object
       [ "guard" .= object
         [ "pred" .= ("keys-all" :: T.Text)
         , "keys" .=
           ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
         ]
       , "balance" .= (Number 99999900.0)
       ])])
    (M.lookup 10 hist)
  -- and transaction txids
  assertEqual "check txids"
    [7,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42]
    (M.keys hist)
  -- and last tx log change for accounts touched in given block
  assertEqual "check previous balance"
    (M.fromList
     [(RowKey "sender00",
       (TxLog "coin_coin-table" "sender00"
        (object
         [ "guard" .= object
           [ "pred" .= ("keys-all" :: T.Text)
           , "keys" .=
             ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
           ]
         , "balance" .= (Number 100000000.0)
         ]
        )
       ))])
    prevBals

getHistoricalLookupNoTxs
    :: T.Text
    -> (Maybe (TxLog Value) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (PactQueue,TestBlockDb)
    -> TestTree
getHistoricalLookupNoTxs key assertF refIO reqIO = testCase msg $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO mempty
  void $ runBlock q bdb second msg
  h <- getParentTestBlockDb bdb cid
  histLookup q h key >>= assertF
  where msg = T.unpack $ "getHistoricalLookupNoTxs: " <> key

getHistoricalLookupWithTxs
    :: T.Text
    -> (Maybe (TxLog Value) -> IO ())
    -> IO (IORef MemPoolAccess)
    -> IO (PactQueue,TestBlockDb)
    -> TestTree
getHistoricalLookupWithTxs key assertF refIO reqIO = testCase msg $ do
  (q,bdb) <- reqIO
  setOneShotMempool refIO goldenMemPool
  void $ runBlock q bdb second msg
  h <- getParentTestBlockDb bdb cid
  histLookup q h key >>= assertF
  where msg = T.unpack $ "getHistoricalLookupWithTxs: " <> key


histLookup :: PactQueue -> BlockHeader -> T.Text -> IO (Maybe (TxLog Value))
histLookup q bh k = do
  mv <- pactHistoricalLookup bh (Domain' (UserTables "coin_coin-table")) (RowKey k) q
  forSuccess "histLookup" (return mv)

assertSender00Bal :: Rational -> String -> Maybe (TxLog Value) -> Assertion
assertSender00Bal bal msg hist =
  assertEqual msg
    (Just (TxLog "coin_coin-table" "sender00"
      (object
        [ "guard" .= object
          [ "pred" .= ("keys-all" :: T.Text)
          , "keys" .=
            ["368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca" :: T.Text]
          ]
        , "balance" .= Number (fromRational bal)
        ])))
    hist

newBlockRewindValidate :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
newBlockRewindValidate mpRefIO reqIO = testCase "newBlockRewindValidate" $ do
  (q,bdb) <- reqIO
  setOneShotMempool mpRefIO chainDataMemPool
  cut0 <- readMVar $ _bdbCut bdb -- genesis cut

  -- cut 1a
  void $ runBlock q bdb second "newBlockRewindValidate-1a"
  cut1a <- readMVar $ _bdbCut bdb

  -- rewind, cut 1b
  void $ swapMVar (_bdbCut bdb) cut0
  void $ runBlock q bdb second "newBlockRewindValidate-1b"

  -- rewind to cut 1a to trigger replay with chain data bug
  void $ swapMVar (_bdbCut bdb) cut1a
  void $ runBlock q bdb (secondsToTimeSpan 2) "newBlockRewindValidate-2"

  where

    chainDataMemPool = mempty {
      mpaGetBlock = \_ _ _ _ bh -> do
          fmap V.singleton $ buildCwCmd
              $ signSender00
              $ setFromHeader bh
              $ mkCmd (sshow bh)
              $ mkExec' "(chain-data)"
      }


signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkSigner' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (_blockChainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ _blockCreationTime bh)


pattern BlockGasLimitError :: forall b. Either PactException b
pattern BlockGasLimitError <-
  Left (PactInternalError (decode . BL.fromStrict . T.encodeUtf8 -> Just (BlockGasLimitExceeded _)))

-- this test relies on block gas errors being thrown before other Pact errors.
blockGasLimitTest :: IO (IORef MemPoolAccess) -> IO (PactQueue, TestBlockDb) -> TestTree
blockGasLimitTest _ reqIO = testCase "blockGasLimitTest" $ do
  (q,_) <- reqIO

  let
    useGas g = do
      bigTx <- buildCwCmd $ set cbGasLimit g $ signSender00 $ mkCmd "cmd" $ mkExec' "TESTING"
      let
        cr = CommandResult
          (RequestKey (Hash "0")) Nothing
          (PactResult $ Left $ PactError EvalError (Pact.Types.Info.Info $ Nothing) [] mempty)
          (fromIntegral g) Nothing Nothing Nothing []
        block = Transactions
          (V.singleton (bigTx, cr))
          (CommandResult (RequestKey (Hash "h")) Nothing
            (PactResult $ Right $ pString "output") 0 Nothing Nothing Nothing [])
        payload = toPayloadWithOutputs noMiner block
        bh = newBlockHeader
          mempty
          (_payloadWithOutputsPayloadHash payload)
          (Nonce 0)
          (BlockCreationTime $ Time $ TimeSpan 0)
          (ParentHeader $ genesisBlockHeader testVersion cid)
      validateBlock bh (payloadWithOutputsToPayloadData payload) q >>= takeMVar
  -- we consume slightly more than the maximum block gas limit and provoke an error.
  useGas 2_000_001 >>= \case
    BlockGasLimitError ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume much more than the maximum block gas limit and expect an error.
  useGas 3_000_000 >>= \case
    BlockGasLimitError ->
      return ()
    r ->
      error $ "not a BlockGasLimitExceeded error: " <> sshow r
  -- we consume exactly the maximum block gas limit and expect no such error.
  useGas 2_000_000 >>= \case
    BlockGasLimitError ->
      error "consumed exactly block gas limit but errored"
    _ ->
      return ()
  -- we consume much less than the maximum block gas limit and expect no such error.
  useGas 1_000_000 >>= \case
    BlockGasLimitError ->
      error "consumed much less than block gas limit but errored"
    _ ->
      return ()
  -- we consume zero gas and expect no such error.
  useGas 0 >>= \case
    BlockGasLimitError ->
      error "consumed no gas but errored"
    _ ->
      return ()

mempoolRefillTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
mempoolRefillTest mpRefIO reqIO = testCase "mempoolRefillTest" $ do

  (q,bdb) <- reqIO
  supply <- newMVar (0 :: Int)

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-1" >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-2" >>= checkCount 3

  mp supply [ ( 0, [badTx, goodTx] ), ( 1, [goodTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2

  mp supply [ ( 0, [badTx] ), ( 1, [goodTx, goodTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2

  mp supply [ ( 0, [goodTx, goodTx] ), ( 1, [badTx, badTx] ) ]
  runBlock q bdb second "mempoolRefillTest-3" >>= checkCount 2


  where

    checkCount n = assertEqual "tx return count" n . V.length . _payloadWithOutputsTransactions

    mp supply txRefillMap = setMempool mpRefIO $ mempty {
      mpaGetBlock = \BlockFill{..} _ _ _ bh -> case M.lookup _bfCount (M.fromList txRefillMap) of
          Nothing -> return mempty
          Just txs -> fmap V.fromList $ sequence $ map (next supply bh) txs
      }


    next supply bh f = do
      i <- modifyMVar supply $ return . (succ &&& id)
      f i bh

    goodTx i bh = buildCwCmd
        $ signSender00
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"

    badTx i bh = buildCwCmd
        $ signSender00
        $ set cbSender "bad"
        $ mkCmd' bh (sshow (i,bh))
        $ mkExec' "(+ 1 2)"


    mkCmd' :: BlockHeader -> T.Text -> PactRPC T.Text -> CmdBuilder
    mkCmd' bh nonce =
      setFromHeader bh
      . mkCmd nonce



moduleNameFork :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
moduleNameFork mpRefIO reqIO = testCase "moduleNameFork" $ do

  (q,bdb) <- reqIO

  -- install in free in block 1
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- install in user in block 2
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- do something else post-fork
  setOneShotMempool mpRefIO (moduleNameMempool "free" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"
  setOneShotMempool mpRefIO (moduleNameMempool "user" "test2")
  void $ runBlock q bdb second "moduleNameFork-1"

  -- TODO this test doesn't actually validate, I turn on Debug and make sure it
  -- goes well.

moduleNameMempool :: T.Text -> T.Text -> MemPoolAccess
moduleNameMempool ns mn = mempty
    { mpaGetBlock = getTestBlock
    }
  where
    getTestBlock _ _ _ _ bh = do
        let txs =
              [ "(namespace '" <> ns <> ") (module " <> mn <> " G (defcap G () (enforce false 'cannotupgrade)))"
              , ns <> "." <> mn <> ".G"
              ]
        fmap V.fromList $ forM (zip txs [0..]) $ \(code,n :: Int) ->
          buildCwCmd $
          signSender00 $
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

  setOneShotMempool mpRefIO $ mp tx
  -- b2 will be made at start + 30s
  void $ runBlock q bdb s30 "mempoolCreationTimeTest-2"

  where

    makeTx nonce t = buildCwCmd
        $ signSender00
        $ set cbChainId cid
        $ set cbCreationTime (toTxCreationTime $ t)
        $ set cbTTL 300
        $ mkCmd (sshow t <> nonce)
        $ mkExec' "1"
    mp tx = mempty {
      mpaGetBlock = \_ valid _ _ bh -> getBlock bh tx valid
      }

    getBlock bh tx valid = do
      let txs = V.singleton tx
      oks <- valid (_blockHeight bh) (_blockHash bh) txs
      unless (V.and oks) $ throwM $ userError "Insert failed"
      return txs


badlistNewBlockTest :: IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
badlistNewBlockTest mpRefIO reqIO = testCase "badlistNewBlockTest" $ do
  (reqQ,_) <- reqIO
  let hashToTxHashList = V.singleton . requestKeyToTransactionHash . RequestKey . toUntypedHash @'Blake2b_256
  badHashRef <- newIORef $ hashToTxHashList initialHash
  badTx <- buildCwCmd
    $ signSender00
    -- this should exceed the account balance
    $ set cbGasLimit 99999
    $ set cbGasPrice 1_000_000_000_000_000
    $ mkCmd "badListMPA"
    $ mkExec' "(+ 1 2)"
  setOneShotMempool mpRefIO (badlistMPA badTx badHashRef)
  resp <- forSuccess "badlistNewBlockTest" $ newBlock noMiner (ParentHeader genesisHeader) reqQ
  assertEqual "bad tx filtered from block" mempty (_payloadWithOutputsTransactions resp)
  badHash <- readIORef badHashRef
  assertEqual "Badlist should have badtx hash" (hashToTxHashList $ _cmdHash badTx) badHash
  where
    badlistMPA badTx badHashRef = mempty
      { mpaGetBlock = \_ _ _ _ _ -> return (V.singleton badTx)
      , mpaBadlistTx = \v -> writeIORef badHashRef v
      }


goldenNewBlock :: String -> MemPoolAccess -> IO (IORef MemPoolAccess) -> IO (PactQueue,TestBlockDb) -> TestTree
goldenNewBlock name mp mpRefIO reqIO = golden name $ do
    (reqQ,_) <- reqIO
    setOneShotMempool mpRefIO mp
    resp <- forSuccess ("goldenNewBlock:" ++ name) $
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
    getTestBlock _ validate bHeight bHash _parent = do
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
        outtxs <- mkTxs txs
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
          signSender00 $
          set cbGasPrice 0.01 $
          set cbTTL 1_000_000 $ -- match old goldens
          mkCmd ("1" <> sshow n) $
          mkExec code $
          mkKeySetData "test-admin-keyset" [sender00]
