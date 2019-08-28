{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chainweb.Test.Pact.ZooContractTest where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.CAS.RocksDB
import Data.FileEmbed
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import Data.Tuple.Strict (T3(..))
import Data.Word
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y


import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports

import Pact.ApiReq
import Pact.Types.Crypto

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Miner.Core (HeaderBytes(..), TargetBytes(..), mine, usePowHash)
import Chainweb.Miner.Pact
import Chainweb.NodeId
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
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils (runGet)
import Chainweb.Version

testVer :: ChainwebVersion
testVer = Development

testChainId :: ChainId
testChainId = someChainId testVer

tests :: ScheduledTest
tests =
    ScheduledTest label $
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    testGroup label
        [
          withPact pdb bhdb testMemPoolAccess dir $ \reqQIO ->
            testCase "play a round" $
                playRoundAZoo genblock testChainId pdb bhdb reqQIO

        ]
  where
    genblock = genesisBlockHeader testVer testChainId
    label = "Chainweb.Test.Pact.ZooContractTest"

playRoundAZoo
    :: BlockHeader
    -> ChainId
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
playRoundAZoo genesisBlock c iopdb iobhdb rr = do
    nonceCounter <- newIORef (1 :: Word64)
    void $ mineLine genesisBlock nonceCounter 4
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM (const go) [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go = do
              r <- ask
              pblock <- get
              n <- liftIO $ Nonce <$> readIORef ncounter
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock c n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

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

mineBlock
    :: BlockHeader
    -> ChainId
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO (TQueue RequestMsg)
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader cid nonce iopdb iobhdb r = do

     mv <- r >>= newBlock noMiner parentHeader
     payload <- assertNotLeft =<< takeMVar mv

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral
     let bh = newBlockHeader
              (ChainNodeId cid 0)
              (BlockHashRecord mempty)
              (_payloadWithOutputsPayloadHash payload)
              nonce
              maxTarget
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

assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = MemPoolAccess
    { mpaGetBlock = \_ bheight _ _ -> getZooBlock bheight
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }

getZooBlock :: BlockHeight -> IO (V.Vector ChainwebTransaction)
getZooBlock (BlockHeight bh)
  | bh == 1 = loadContractTx
  | bh == 2 = initializeGameTx
  | bh == 3 = firstBetTx
  | bh == 4 = secondBetTx
  | bh == 5 = endGameTx
  | otherwise = error
        "Cannot get transactions for this block.\n This should be impossible in this test."

loadContractTx, initializeGameTx, firstBetTx, secondBetTx, endGameTx
  :: IO (V.Vector ChainwebTransaction)

loadContractTx = do
  akp0 <- stockKey "sender00"
  kp0 <- mkKeyPairs [akp0]
  zoocode <- TIO.readFile "pact/lottery.pact"
  let zooValue =
        object
            ["zookeeper" .= mkKeyset "keys-all" [fromJust $ _akpPublic akp0]]

      nonce = "1"
      tx = V.singleton $ PactTransaction zoocode (Just zooValue)
  mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 0 tx

initializeGameTx = do
  akp0 <- stockKey "sender00"
  kp0 <- mkKeyPairs [akp0]
  let zoocode = "(zoo-game.init-game \"thursday\" 1.0)"
      zooValue = Null

      nonce = "2"
      tx = V.singleton $ PactTransaction zoocode (Just zooValue)

  mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 0 tx

firstBetTx = do
  akp1 <- stockKey "sender01"
  kp1 <- mkKeyPairs [akp1]
  let zoocode = "(zoo-game.bet-number \"12\" \"thursday\" \"sender01\" 1.0)"
      zooValue = Null

      nonce = "3"
      tx = V.singleton $ PactTransaction zoocode (Just zooValue)

  mkTestExecTransactions "sender01" "0" kp1 nonce 10000 0.00000000001 3600 0 tx

secondBetTx = do
  akp1 <- stockKey "sender01"
  kp1 <- mkKeyPairs [akp1]
  let zoocode =
        -- Deal I guess?
        "(zoo-game.bet-animal [[\"00\" \"01\" \"02\" \"03\"] [\"96\" \"97\" \"98\" \"99\"]] \"thursday\" \"sender01\" 1.0)"
      zooValue = Null

      nonce = "4"
      tx = V.singleton $ PactTransaction zoocode (Just zooValue)

  mkTestExecTransactions "sender01" "0" kp1 nonce 10000 0.00000000001 3600 0 tx

endGameTx = do
  akp0 <- stockKey "sender00"
  kp0 <- mkKeyPairs [akp0]
  let zoocode = "(zoo-game.end-game \"thursday\")"
      zooValue = Null

      nonce = "5"
      tx = V.singleton $ PactTransaction zoocode (Just zooValue)

  mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 0 tx

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/testnet/keys.yaml")

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let Right (Y.Object o) = Y.decodeEither' stockKeyFile
      Just (Y.Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
      mkKeyBS = decodeKey . encodeUtf8
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519)

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode
