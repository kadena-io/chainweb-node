{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where
-- import Debug.Trace
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.CAS.RocksDB
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple.Strict (T3(..))
import qualified Data.Vector as V
import Data.Word

import NeatInterpolation (text)

import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq
import Pact.Parse
import Pact.Types.ChainMeta

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils (runGet, sshow)
import Chainweb.Version

testVer :: ChainwebVersion
testVer = Development

tests :: ScheduledTest
tests =
    ScheduledTest label $
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    testGroup label
        [ withTime $ \iot -> withPact pdb bhdb (testMemPoolAccess iot) dir $ \reqQIO ->
            testCase "initial-playthrough" $
            firstPlayThrough genblock pdb bhdb reqQIO
        , after AllSucceed "initial-playthrough" $
          withTime $ \iot -> withPact pdb bhdb (testMemPoolAccess iot) dir $ \reqQIO ->
            testCase "on-restart" $ onRestart pdb bhdb reqQIO
        , after AllSucceed "on-restart" $
          withTime $ \iot -> withPact pdb bhdb (dupegenMemPoolAccess iot) dir $ \reqQIO ->
            testCase "reject-dupes" $ testDupes genblock pdb bhdb reqQIO
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVer

onRestart
    :: IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
onRestart pdb bhdb r = do
    bhdb' <- bhdb
    block <- maxEntry bhdb'
    let nonce = Nonce $ fromIntegral $ _blockHeight block
    T3 _ b _ <- mineBlock block nonce pdb bhdb r
    assertEqual "Invalid BlockHeight" 9 (_blockHeight b)

testMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
testMemPoolAccess iot  = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header  -> do
            t <- meinhack bh <$> iot
            getTestBlock t validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    meinhack :: BlockHeight -> Time Integer -> Time Integer
    meinhack b tt =
      foldl' (flip add) tt (replicate (fromIntegral b) millisecond)
    getTestBlock txOrigTime validate bHeight@(BlockHeight bh) hash = do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = T.pack . show @(Time Integer) $ txOrigTime
        outtxs <-
          mkTestExecTransactions
            "sender00" "0" kp0
            nonce 10000 0.00000000001
            3600 (toTxCreationTime txOrigTime) (tx bh)
        oks <- validate bHeight hash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show (tx bh)
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks
                           ]
        return outtxs
      where
        ksData :: Text -> Value
        ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
        tx nonce = V.singleton $ PactTransaction (code nonce) (Just $ ksData (T.pack $ show nonce))
        code nonce = defModule (T.pack $ show nonce)
        toTxCreationTime :: Time Integer -> TxCreationTime
        toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
          Seconds s -> TxCreationTime $ ParsedInteger s

dupegenMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
dupegenMemPoolAccess iot  = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header -> do
            t <- meinhack bh <$> iot
            getTestBlock t validate bh hash _header
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    meinhack :: BlockHeight -> Time Integer -> Time Integer
    meinhack b tt =
      foldl' (flip add) tt (replicate (fromIntegral b) millisecond)
    getTestBlock txOrigTime validate bHeight bHash _bHeader = do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = "0"
        outtxs <-
          mkTestExecTransactions
          "sender00" "0" kp0
          nonce 10000 0.00000000001
          3600 (toTxCreationTime txOrigTime) (tx nonce)
        oks <- validate bHeight bHash outtxs
        when (not $ V.and oks) $ do
          fail $ mconcat [ "tx failed validation! input list: \n"
                         , show (tx nonce)
                         , "\n\nouttxs: "
                         , "\n\noks: "
                         , show oks
                         ]
        return outtxs
      where
        ksData :: Text -> Value
        ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
        tx nonce = V.singleton $ PactTransaction (code nonce) (Just $ ksData nonce)
        code nonce = defModule nonce
        toTxCreationTime :: Time Integer -> TxCreationTime
        toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
            Seconds s -> TxCreationTime $ ParsedInteger s

firstPlayThrough
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
firstPlayThrough genesisBlock iopdb iobhdb rr = do
    nonceCounter <- newIORef (1 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 7
    let T3 _ startline1 _ = mainlineblocks !! 0
    let T3 _ startline2 _ = mainlineblocks !! 1
    void $ mineLine startline1 nonceCounter 4
    void $ mineLine startline2 nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              r <- ask
              pblock <- get
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

testDupes
  :: BlockHeader
  -> IO (PayloadDb HashMapCas)
  -> IO (BlockHeaderDb)
  -> IO (TQueue RequestMsg)
  -> Assertion
testDupes genesisBlock iopdb iobhdb rr = do
    (T3 _ newblock payload) <- liftIO $ mineBlock genesisBlock (Nonce 1) iopdb iobhdb rr
    expectException newblock payload $ liftIO $
        mineBlock newblock (Nonce 2) iopdb iobhdb rr
  where
    expectException newblock payload act = do
        m <- wrap `catch` h
        maybe (return ()) (\msg -> assertBool msg False) m
      where
        wrap = do
            (T3 _ newblock2 payload2) <- act
            let msg = concat [ "expected exception on dupe block. new block header:\n"
                             , sshow newblock2
                             , "\nnew payload: \n"
                             , sshow payload2
                             , "\nprev block: \n"
                             , sshow newblock
                             , "\nprev payload: \n"
                             , sshow payload
                             ]
            return $ Just msg

        h :: SomeException -> IO (Maybe String)
        h _ = return Nothing


mineBlock
    :: BlockHeader
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO (TQueue RequestMsg)
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb r = do

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral

     mv <- r >>= newBlock noMiner parentHeader (BlockCreationTime creationTime)
     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     HeaderBytes newBytes  <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash newBytes

     mv' <- r >>= validateBlock newHeader (toPayloadData payload)

     void $ assertNotLeft =<< takeMVar mv'

     pdb <- iopdb
     addNewPayload pdb payload

     bhdb <- iobhdb
     insert bhdb newHeader

     return $ T3 parentHeader newHeader payload

     where
       toPayloadData :: PayloadWithOutputs -> PayloadData
       toPayloadData d = PayloadData
                 { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions d
                 , _payloadDataMiner = _payloadWithOutputsMiner d
                 , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash d
                 , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash d
                 , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash d
                 }

withTemporaryDir :: (IO FilePath -> TestTree) -> TestTree
withTemporaryDir = withResource (fst <$> newTempDir) removeDirectoryRecursive

withPayloadDb :: (IO (PayloadDb HashMapCas) -> TestTree) -> TestTree
withPayloadDb = withResource newPayloadDb (\_ -> return ())

withBlockHeaderDb
    :: IO RocksDb
    -> BlockHeader
    -> (IO BlockHeaderDb -> TestTree)
    -> TestTree
withBlockHeaderDb iordb b = withResource start stop
  where
    start = do
        rdb <- iordb
        testBlockHeaderDb rdb b
    stop = closeBlockHeaderDb

withPact
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> MemPoolAccess
    -> IO FilePath
    -> (IO (TQueue RequestMsg) -> TestTree)
    -> TestTree
withPact iopdb iobhdb mempool iodir f =
    withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically newTQueue
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir
        a <- async $ initPactService testVer cid logger reqQ mempool mv
                                     bhdb pdb (Just dir) Nothing False
        link a
        return (a, reqQ)

    stopPact (a, reqQ) = do
        sendCloseMsg reqQ
        cancel a

    logger = genericLogger Warn T.putStrLn
    cid = someChainId testVer

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(module m$idx 'k$idx

  (defschema sch col:integer)

  (deftable tbl:{sch})

  (defun insertTbl (a i)
    (insert tbl a { 'col: i }))

  (defun updateTbl (a i)
    (update tbl a { 'col: i}))

  (defun readTbl ()
    (sort (map (at 'col)
      (select tbl (constantly true)))))

  (defpact dopact (n)
    (step { 'name: n, 'value: 1 })
    (step { 'name: n, 'value: 2 }))

)
(create-table tbl)
(readTbl)
(insertTbl "a" 1)
|]
