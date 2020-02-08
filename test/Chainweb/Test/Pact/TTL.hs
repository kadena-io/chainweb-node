{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.TTL (tests) where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Aeson
import Data.Bytes.Put
import Data.CAS.HashMap
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Strict
import qualified Data.Vector as V

import NeatInterpolation

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq
import Pact.Types.ChainMeta

-- chainweb imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.Difficulty
import Chainweb.Miner.Core
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
import Chainweb.Utils
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
          withPact testVer Quiet pdb bhdb
                   (testMemPoolAccess (BadTTL badttl) iot) dir 100000
                   (testCase "reject-tx-with-badttl" .
                    testTTL genblock pdb bhdb)
        , after AllSucceed "reject-tx-with-badttl" $
          withTime $ \iot ->
          withPact testVer Quiet pdb bhdb
                   (testMemPoolAccess (BadTxTime addtime) iot) dir 100000
                   (testCase "reject-tx-with-badtxtime" .
                    testTTL genblock pdb bhdb)
        , after AllSucceed "reject-tx-with-badtxtime" $
          withTime $ \iot ->
          withPact testVer Quiet pdb bhdb
                   (testMemPoolAccess (BadExpirationTime addtime 1) iot) dir
                   100000
                   (testCase "reject-tx-with-badexpirationtime" .
                    testTTL genblock pdb bhdb)
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.TTL"
    cid = someChainId testVer
    badttl = 100 * 24 * 60 * 60
    addtime = toTxCreationTime . add second

-- this tests for a bad ttl
testTTL
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO PactQueue
    -> Assertion
testTTL genesisBlock iopdb iobhdb rr = do
    (T3 _ newblock _) <- liftIO $ mineBlock genesisBlock (Nonce 1) iopdb iobhdb rr
    expectException $ mineBlock newblock (Nonce 2) iopdb iobhdb rr
  where
    expectException act = do
        m <- wrap `catch` h
        case m of
          Just msg -> assertBool msg False
          Nothing -> return ()
      where
        h :: SomeException -> IO (Maybe String)
        h _ = return Nothing
        wrap = do
          (T3 _ _ _) <- act
          return $ Just "Expected a transaction validation failure."

testMemPoolAccess :: TTLTestCase -> IO (Time Integer) -> MemPoolAccess
testMemPoolAccess _ttlcase iot = mempty
    { mpaGetBlock = \validate bh hash _header  -> do
            t <- f bh <$> iot
            getTestBlock t validate bh hash
    }
  where
    f :: BlockHeight -> Time Integer -> Time Integer
    f b tt =
      foldl' (flip add) tt (replicate (fromIntegral b) millisecond)
    getTestBlock txOrigTime validate bHeight@(BlockHeight bh) hash = do
        akp0 <- stockKey "sender00"
        kp0 <- mkKeyPairs [akp0]
        let nonce = T.pack . show @(Time Integer) $ txOrigTime
            (txOrigTime', badttl) = case _ttlcase of
              BadTTL b -> (toTxCreationTime txOrigTime, b)
              BadTxTime g -> (g txOrigTime, 24 * 60 * 60)
              BadExpirationTime g ttl -> (g txOrigTime, ttl)
        outtxs <-
          mkTestExecTransactions
            "sender00" "0" kp0
            nonce 10000 0.00000000001
            badttl txOrigTime' (tx bh)
        _oks <- validate bHeight hash outtxs
        return outtxs
      where
        ksData :: Text -> Value
        ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
        tx nonce = V.singleton $ PactTransaction (code nonce) (Just $ ksData (T.pack $ show nonce))
        code nonce = defModule (T.pack $ show nonce)

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

     _mv' <- r >>= validateBlock newHeader (toPayloadData payload)

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


data TTLTestCase =
  BadTTL TTLSeconds
  | BadTxTime (Time Integer -> TxCreationTime)
    -- this takes the blocktime as an argument to ensure that the transaction
    -- comes after if needed.
  | BadExpirationTime (Time Integer -> TxCreationTime) TTLSeconds
