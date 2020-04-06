{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.CAS.HashMap
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict (T3(..))
import qualified Data.Vector as V
import Data.Word

import NeatInterpolation (text)

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq

-- chainweb imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.BlockHeight
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Utils (sshow)
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
        [ withPact testVer Warn pdb bhdb testMemPoolAccess dir 100_000
            (testCase "initial-playthrough" . firstPlayThrough genblock pdb bhdb)
        , after AllSucceed "initial-playthrough" $
            withPact testVer Warn pdb bhdb testMemPoolAccess dir 100_000
                (testCaseSteps "on-restart" . onRestart pdb bhdb)
        , after AllSucceed "on-restart" $
            withPact testVer Quiet pdb bhdb dupegenMemPoolAccess dir 100_000
            (testCase "reject-dupes" . testDupes genblock pdb bhdb)
        , after AllSucceed "reject-dupes" $
            let deepForkLimit = 4
            in withPact testVer Quiet pdb bhdb testMemPoolAccess dir deepForkLimit
                (testCaseSteps "deep-fork-limit" . testDeepForkLimit deepForkLimit pdb bhdb)
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVer

onRestart
    :: IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> (String -> IO ())
    -> Assertion
onRestart pdb bhdb r step = do
    bhdb' <- bhdb
    block <- maxEntry bhdb'
    step $ "max block has height " <> sshow (_blockHeight block)
    let nonce = Nonce $ fromIntegral $ _blockHeight block
    step "mine block on top of max block"
    T3 _ b _ <- mineBlock (ParentHeader block) nonce pdb bhdb r
    assertEqual "Invalid BlockHeight" 9 (_blockHeight b)

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = mempty
    { mpaGetBlock = \validate bh hash parentHeader  -> do
        let (BlockCreationTime t) = _blockCreationTime parentHeader
        getTestBlock t validate bh hash
    }
  where
    getTestBlock _ _ 1 _ = mempty
    getTestBlock txOrigTime validate bHeight@(BlockHeight bh) hash = do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = T.pack . show @(Time Micros) $ txOrigTime
        outtxs <-
          mkTestExecTransactions
            "sender00" "0" kp0
            nonce 10_000 0.000_000_000_01
            3600 (toTxCreationTime txOrigTime) (tx bh)
        oks <- validate bHeight hash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "testMemPoolAccess: tx failed validation! input list: \n"
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

dupegenMemPoolAccess :: MemPoolAccess
dupegenMemPoolAccess = mempty
    { mpaGetBlock = \validate bHeight bHash _parentHeader -> do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = "0"
            tx = V.singleton $ PactTransaction (defModule nonce) (Just $ ksData nonce)
        outtxs <- mkTestExecTransactions "sender00" "0" kp0 nonce 10_000 0.000_000_000_01 3600 0 tx
        oks <- validate bHeight bHash outtxs
        unless (V.and oks) $ fail $ mconcat
            [ "dupegenMemPoolAccess: tx failed validation! input list: \n"
            , show tx
            , "\n\nouttxs: "
            , "\n\noks: "
            , show oks
            ]
        return outtxs
    }
  where
    ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]

firstPlayThrough
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> Assertion
firstPlayThrough genesisBlock iopdb iobhdb rr = do
    nonceCounter <- newIORef (1 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 7
    let T3 _ startline1 _ = head mainlineblocks
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
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

testDupes
  :: BlockHeader
  -> IO (PayloadDb HashMapCas)
  -> IO BlockHeaderDb
  -> IO PactQueue
  -> Assertion
testDupes genesisBlock iopdb iobhdb rr = do
    (T3 _ newblock payload) <- liftIO $ mineBlock (ParentHeader genesisBlock) (Nonce 1) iopdb iobhdb rr
    expectException newblock payload $ liftIO $
        mineBlock (ParentHeader newblock) (Nonce 3) iopdb iobhdb rr
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
  -> IO BlockHeaderDb
  -> IO PactQueue
  -> (String -> IO ())
  -> Assertion
testDeepForkLimit deepForkLimit iopdb iobhdb rr step = do
    bhdb <- iobhdb
    step "query max db entry"
    maxblock <- maxEntry bhdb
    step $ "max block has height " <> sshow (_blockHeight maxblock)
    nonceCounterMain <- newIORef (fromIntegral $ _blockHeight maxblock)

    -- mine the main line a bit more
    step "mine (deepForkLimit + 1) many blocks on top of max block"
    void $ mineLine maxblock nonceCounterMain (deepForkLimit + 1)

    -- how far it mines doesn't really matter
    step "try to mine a fork on top of max block"
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
              pblock <- gets ParentHeader
              n <- liftIO $ Nonce <$> readIORef ncounter
              liftIO $ step $ "mine block on top of height " <> sshow (_blockHeight $ _parentHeader pblock)
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret


mineBlock
    :: ParentHeader
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO PactQueue
    -> IO (T3 ParentHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb r = do

     -- assemble block without nonce and timestamp
     mv <- r >>= newBlock noMiner parentHeader
     payload <- assertNotLeft =<< takeMVar mv

     let bh = newBlockHeader
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              creationTime
              parentHeader

     mv' <- r >>= validateBlock bh (toPayloadData payload)
     void $ assertNotLeft =<< takeMVar mv'

     pdb <- iopdb
     addNewPayload pdb payload

     bhdb <- iobhdb
     insert bhdb bh

     return $ T3 parentHeader bh payload

   where
     creationTime = BlockCreationTime
          . add (TimeSpan 1_000_000)
          . _bct . _blockCreationTime
          $ _parentHeader parentHeader

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
