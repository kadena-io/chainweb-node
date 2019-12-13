{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
import qualified Data.Vector as V
import Data.Word

import NeatInterpolation (text)

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Difficulty
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils (runGet, sshow)
import Chainweb.Version

testVer :: ChainwebVersion
testVer = FastTimedCPM peterson

tests :: ScheduledTest
tests =
    ScheduledTest label $
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    testGroup label
        [ withTime $ \iot ->
            withPact testVer Warn pdb bhdb (testMemPoolAccess iot) dir 100000
                (testCase "initial-playthrough" .
                 firstPlayThrough genblock pdb bhdb)
        , after AllSucceed "initial-playthrough" $
          withTime $ \iot ->
            withPact testVer Warn pdb bhdb (testMemPoolAccess iot) dir 100000
                (testCase "on-restart" . onRestart pdb bhdb)
        , after AllSucceed "on-restart" $
          withTime $ \iot ->
            withPact testVer Quiet pdb bhdb (dupegenMemPoolAccess iot) dir 100000
            (testCase "reject-dupes" . testDupes genblock pdb bhdb)
        , after AllSucceed "reject-dupes" $
          withTime $ \iot ->
            let deepForkLimit = 4
            in withPact testVer Quiet pdb bhdb (testMemPoolAccess iot) dir deepForkLimit
            (testCase "deep-fork-limit" . testDeepForkLimit deepForkLimit pdb bhdb)
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVer

onRestart
    :: IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO PactQueue
    -> Assertion
onRestart pdb bhdb r = do
    bhdb' <- bhdb
    block <- maxEntry bhdb'
    let nonce = Nonce $ fromIntegral $ _blockHeight block
    T3 _ b _ <- mineBlock block nonce pdb bhdb r
    assertEqual "Invalid BlockHeight" 9 (_blockHeight b)

testMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
testMemPoolAccess iot = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header  -> do
            t <- f bh <$> iot
            getTestBlock t validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    f :: BlockHeight -> Time Integer -> Time Integer
    f b tt =
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

dupegenMemPoolAccess :: IO (Time Integer) -> MemPoolAccess
dupegenMemPoolAccess iot = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header -> do
            t <- f bh <$> iot
            getTestBlock t validate bh hash _header
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    f :: BlockHeight -> Time Integer -> Time Integer
    f b tt =
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

firstPlayThrough
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO PactQueue
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
  -> IO PactQueue
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

testDeepForkLimit
  :: Word64
  -> IO (PayloadDb HashMapCas)
  -> IO (BlockHeaderDb)
  -> IO PactQueue
  -> Assertion
testDeepForkLimit deepForkLimit iopdb iobhdb rr = do
    bhdb <- iobhdb
    maxblock <- maxEntry bhdb
    nonceCounterMain <- newIORef (fromIntegral $ _blockHeight maxblock)

    -- mine the main line a bit more
    void $ mineLine maxblock nonceCounterMain (deepForkLimit + 1)

    -- how far it mines doesn't really matter
    nCounter <- newIORef (fromIntegral $ _blockHeight maxblock)
    try (mineLine maxblock nCounter 1) >>= \case
        Left SomeException{} -> return ()
        Right _ -> assertBool msg False

  where
    msg = "expected exception on a deep fork longer than " <> show deepForkLimit

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


mineBlock
    :: BlockHeader
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb r = do

     -- assemble block without nonce and timestamp
     creationTime <- BlockCreationTime <$> getCurrentTimeIntegral

     mv <- r >>= newBlock noMiner parentHeader creationTime
     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              (ParentHeader parentHeader)
         hbytes = HeaderBytes . runPutS $ encodeBlockHeaderWithoutHash bh
         tbytes = TargetBytes . runPutS . encodeHashTarget $ _blockTarget bh

     T2 (HeaderBytes new) _ <- usePowHash testVer (\p -> mine p (_blockNonce bh) tbytes) hbytes
     newHeader <- runGet decodeBlockHeaderWithoutHash new

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

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

defModule :: Text -> Text
defModule idx = [text| ;;

(define-keyset 'k$idx (read-keyset 'k$idx))

(namespace 'free)

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
