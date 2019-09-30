{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chainweb.Test.Pact.ChainData where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Data.Bytes.Put (runPutS)
import Data.CAS.HashMap
import Data.CAS.RocksDB
import Data.IORef
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple.Strict (T3(..))
import qualified Data.Vector as V
import Data.Word

import System.Directory
import System.IO.Extra
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
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Mempool.Mempool (MempoolPreBlockCheck)
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
import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Utils (runGet)
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Tests

testVer :: ChainwebVersion
testVer = Development

testChainId :: ChainId
testChainId = someChainId testVer

-- | cf. <https://pact-language.readthedocs.io/en/stable/pact-functions.html#chain-data>
--
tests :: ScheduledTest
tests = testGroupSch label
    [ withTime $ chainDataTest "block-time"
    , withTime $ chainDataTest "block-height"
    , withTime $ chainDataTest "gas-limit"
    , withTime $ chainDataTest "gas-price"
    , withTime $ chainDataTest "chain-id"
    , withTime $ chainDataTest "sender"
    ]
  where
    label = "Chainweb.Test.Pact.ChainData"

chainDataTest :: T.Text -> IO (Time Integer) -> TestTree
chainDataTest t time =
    withRocksResource $ \rocksIO ->
    withPayloadDb $ \pdb ->
    withBlockHeaderDb rocksIO genblock $ \bhdb ->
    withTemporaryDir $ \dir ->
    -- tx origination times need to come before block origination times.
    withPact pdb bhdb (testMemPoolAccess t time) dir $ \reqQIO ->
        testCase ("chain-data." <> T.unpack t) $
            run genblock pdb bhdb reqQIO
  where
    genblock = genesisBlockHeader testVer testChainId

-- -------------------------------------------------------------------------- --
-- Test Blocks

getTestBlock
    :: T.Text
    -> Time Integer
    -> MempoolPreBlockCheck ChainwebTransaction
    -> BlockHeight
    -> BlockHash
    -> IO (V.Vector ChainwebTransaction)
getTestBlock t txOrigTime _validate _bh _hash = do
    akp0 <- stockKey "sender00"
    kp0 <- mkKeyPairs [akp0]
    let nonce = (<> t) . T.pack . show @(Time Integer) $ txOrigTime
    txs <- mkTestExecTransactions "sender00" "0" kp0 nonce 10000 0.00000000001 3600 (toTxCreationTime txOrigTime) tx
    oks <- _validate _bh _hash txs
    when (not $ V.and oks) $ do
        fail $ mconcat [ "tx failed validation! input list: \n"
                       , show tx
                       , "\n\nouttxs: "
                       , show txs
                       , "\n\noks: "
                       , show oks ]
    return txs
  where
    code = "(at \"" <> t <> "\" (chain-data))"
    tx = V.singleton $ PactTransaction code Nothing

-- -------------------------------------------------------------------------- --
-- Utils

run
    :: BlockHeader
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
run genesisBlock iopdb iobhdb rr = do
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
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock n iopdb iobhdb r
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
    -> Nonce
    -> IO (PayloadDb HashMapCas)
    -> IO BlockHeaderDb
    -> IO (TQueue RequestMsg)
    -> IO (T3 BlockHeader BlockHeader PayloadWithOutputs)
mineBlock parentHeader nonce iopdb iobhdb r = do

     -- assemble block without nonce and timestamp
     creationTime <- getCurrentTimeIntegral
     mv <- r >>= newBlock noMiner parentHeader (BlockCreationTime creationTime)
     payload <- takeMVar mv >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Nothing
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Nothing
            , _exMessage = "failure during newBlock"
            }

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

     payload' <- takeMVar mv' >>= \case
        Right x -> return x
        Left e -> throwM $ TestException
            { _exInnerException = toException e
            , _exNewBlockResults = Just payload
            , _exValidateBlockResults = Nothing
            , _exNewBlockHeader = Just newHeader
            , _exMessage = "failure during validateBlock"
            }

     pdb <- iopdb
     addNewPayload pdb payload

     bhdb <- iobhdb
     insert bhdb newHeader `catch` \e -> throwM $ TestException
        { _exInnerException = e
        , _exNewBlockResults = Just payload
        , _exValidateBlockResults = Just payload'
        , _exNewBlockHeader = Just newHeader
        , _exMessage = "failure during insert in block header db"
        }

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

data TestException = TestException
    { _exInnerException :: !SomeException
    , _exNewBlockResults :: !(Maybe PayloadWithOutputs)
    , _exValidateBlockResults :: !(Maybe PayloadWithOutputs)
    , _exNewBlockHeader :: !(Maybe BlockHeader)
    , _exMessage :: !T.Text
    }
    deriving (Show)

instance Exception TestException

testMemPoolAccess :: T.Text -> IO (Time Integer) -> MemPoolAccess
testMemPoolAccess t iotime = MemPoolAccess
    { mpaGetBlock = \validate bh hash _parentHeader -> do
        time <- meinhack bh <$> iotime
        getTestBlock t time validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    -- tx origination times needed to be unique to ensure that the corresponding
    -- tx hashes are also unique.
    meinhack :: BlockHeight -> Time Integer -> Time Integer
    meinhack b tt =
      foldl' (flip add) tt (replicate (fromIntegral b) millisecond)
