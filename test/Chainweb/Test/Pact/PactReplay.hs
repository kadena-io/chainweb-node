{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Hash.Algorithms (SHA512t_256)

import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.CAS.HashMap
import Data.CAS.RocksDB
-- import Data.Foldable
-- import Data.Map (Map)
-- import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple.Strict (T3(..))
-- import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Data.Yaml


import System.Directory
import System.IO.Extra
import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB hiding (withBlockHeaderDb)
-- import Chainweb.BlockHeaderDB.Types
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Logger
import Chainweb.Miner.POW
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
-- import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Service.PactQueue
import Chainweb.Test.Pact.Utils
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Test.Utils
-- import Chainweb.Test.Pact.Utils
-- import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.Time
-- import Chainweb.Transaction
import Chainweb.TreeDB
import Chainweb.Version

testVersion :: ChainwebVersion
testVersion = Testnet00

tests :: ScheduledTest
tests = ScheduledTest label $
  withRocksResource $ \rocksIO ->
  withPayloadDb $ \pdb ->
  withBlockHeaderDb rocksIO genblock $ \bhdb ->
  withTemporaryDir $ \dir ->
   testGroup label
    [withPact pdb bhdb testEmptyMemPool dir $ \reqQIO ->
                testCase "simple-replay1" $
                firstPlayThrough genblock cid pdb bhdb reqQIO
    , after AllSucceed "simple-replay1" $ withPact pdb bhdb testEmptyMemPool dir $ \reqQIO ->
        testCase "simple-replay2" $ do
            bhdb' <- bhdb
            maxblock <- maxEntry bhdb'
            secondPlayThrough maxblock cid pdb bhdb reqQIO
    ]
  where
    genblock = genesisBlockHeader testVersion cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVersion

first3 :: (a,b,c) -> a
first3 (a,_,_) = a
second3 :: (a,b,c) -> b
second3 (_,a,_) = a
third3 :: (a,b,c) -> c
third3 (_,_,a) = a


withLastBlock :: (IO (MVar BlockHeader) -> TestTree) -> TestTree
withLastBlock = withResource newEmptyMVar (const $ return ())

secondPlayThrough
            :: BlockHeader
            -> ChainId
            -> IO (PayloadDb HashMapCas)
            -> IO (BlockHeaderDb)
            -> IO (TQueue RequestMsg)
            -> Assertion
secondPlayThrough block cid pdb bhdb r =
  void $ mineBlock block cid (Nonce $ fromIntegral $ _blockHeight block) pdb bhdb r

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
withPact iopdb iobhdb mempool iodir f = withResource startPact stopPact $ f . fmap snd
  where
    startPact = do
        mv <- newEmptyMVar
        reqQ <- atomically newTQueue
        pdb <- iopdb
        bhdb <- iobhdb
        dir <- iodir

        a <- async $ initPactService testVersion cid logger reqQ mempool mv bhdb pdb (Just dir) Nothing False

        return (a, reqQ)

    stopPact (a, reqQ) = do
        sendCloseMsg reqQ
        cancel a

    logger = genericLogger Warn T.putStrLn
    cid = someChainId testVersion

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess  = MemPoolAccess
    { mpaGetBlock = getTestBlock
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    getTestBlock _bHeight _bHash _bHeader = do
        moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
        d <- adminData
        let txs = V.fromList
              [ PactTransaction (T.pack moduleStr) d
              , PactTransaction "(create-table test1.accounts)" d
              , PactTransaction "(test1.create-global-accounts)" d
              , PactTransaction "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" d
              ]
        goldenTestTransactions txs

goldenBytes :: ToJSON a => Exception e => String -> Either e a -> IO BL.ByteString
goldenBytes label (Left e) = assertFailure $ label ++ ": " ++ show e
goldenBytes label (Right a) = return $ BL.fromStrict $ Data.Yaml.encode $ object
    [ "test-group" .= label
    , "results" .= a
    ]

newBlockTest :: String -> IO (TQueue RequestMsg) -> TestTree
newBlockTest label reqIO = golden label $ do
    reqQ <- reqIO
    let genesisHeader = genesisBlockHeader testVersion cid
    respVar <- newBlock noMiner genesisHeader reqQ
    goldenBytes "new-block" =<< takeMVar respVar
  where
    cid = someChainId testVersion

firstPlayThrough ::
            BlockHeader
            -> ChainId
            -> IO (PayloadDb HashMapCas)
            -> IO (BlockHeaderDb)
            -> IO (TQueue RequestMsg)
            -> Assertion
firstPlayThrough genesisBlock c iopdb iobhdb rr = do
    mainlineblocks <- mineLine genesisBlock (Nonce . (xor minBound)) 7
    let T3 _ startline1 _ = mainlineblocks !! 0
    let T3 _ startline2 _ = mainlineblocks !! 1
    -- let T3 _ end0 _ = last mainlineblocks
    void $ mineLine startline1 (Nonce . (xor maxBound)) 4
    void $ mineLine startline2 (Nonce . (xor 10000000)) 4
    return ()

    -- assertEqual ""  [1..8] ((\(T3 _ a _) -> _blockHeight a) <$> mainlineblocks)
    -- assertEqual ""  8 (_blockHeight end0)
    -- assertEqual ""  6 (_blockHeight end1)
    -- assertEqual ""  7 (_blockHeight end2)
  where
    mineLine start withNonce len =
      evalStateT (runReaderT (mapM go [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go height = do
            r <- ask
            pblock <- get
            ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock c (withNonce height) iopdb iobhdb r
            put newblock
            return ret


assertNotLeft :: (MonadThrow m, Exception e) => Either e a -> m a
assertNotLeft (Left l) = throwM l
assertNotLeft (Right r) = return r

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
   -- nonce <- Nonce <$> MWC.uniform g
   creationTime <- getCurrentTimeIntegral
   let candidateHeader = newBlockHeader
                         (ChainNodeId cid 0)
                         (BlockHashRecord mempty)
                         (_payloadWithOutputsPayloadHash payload)
                         nonce
                         maxTarget
                         creationTime
                         parentHeader

   newHeader <- mine (Proxy @SHA512t_256) candidateHeader nonce

   _mv <- r >>= validateBlock newHeader (toPayloadData payload)

   void $ assertNotLeft =<< takeMVar _mv

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


-- TODO: uncomment this for MempoolAccess
{-
-- _testMemPoolAccess :: MemPoolAccess
-- _testMemPoolAccess = MemPoolAccess
--   _getBlock
--   _setLastHeader
--   _processFork

-- _getBlock :: BlockHeight -> BlockHash -> BlockHeader -> IO (Vector ChainwebTransaction)
-- _getBlock height hash parentHeader =
--     maybe (error msg) pure $ M.lookup (height,hash,parentHeader) _transactionMap
--   where
--     msg = "getBlock: lookup failed!"


-- _transactionMap :: Map (BlockHeight, BlockHash, BlockHeader) (Vector ChainwebTransaction)
-- _transactionMap = undefined

-- _setLastHeader :: BlockHeader -> IO ()
-- _setLastHeader = undefined

-- _processFork :: BlockHeader -> IO ()
-- _processFork = undefined
-}

testEmptyMemPool :: MemPoolAccess
testEmptyMemPool = MemPoolAccess
    { mpaGetBlock = \_ _ _ -> goldenTestTransactions V.empty
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }

-- -- testMemPoolAccess :: MemPoolAccess
-- -- testMemPoolAccess  = MemPoolAccess
-- --     { mpaGetBlock = getTestBlock
-- --     , mpaSetLastHeader = \_ -> return ()
-- --     , mpaProcessFork = \_ -> return ()
-- --     }
-- --   where
-- --     getTestBlock _bHeight _bHash _bHeader = do
-- --         moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
-- --         d <- adminData
-- --         let txs = V.fromList
-- --               [ PactTransaction (T.pack moduleStr) d
-- --               , PactTransaction "(create-table test1.accounts)" d
-- --               , PactTransaction "(test1.create-global-accounts)" d
-- --               , PactTransaction "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" d
-- --               ]
-- --         goldenTestTransactions txs
