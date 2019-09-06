{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.PactReplay where

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
        [ withPact pdb bhdb testMemPoolAccess dir $ \reqQIO ->
            testCase "initial-playthrough" $
            firstPlayThrough genblock cid pdb bhdb reqQIO
        , after AllSucceed "initial-playthrough" $
          withPact pdb bhdb testMemPoolAccess dir $ \reqQIO ->
            testCase "on-restart" $ onRestart cid pdb bhdb reqQIO
        , after AllSucceed "on-restart" $
          withPact pdb bhdb dupegenMemPoolAccess dir $ \reqQIO ->
            testCase "reject-dupes" $ testDupes genblock cid pdb bhdb reqQIO
        ]
  where
    genblock = genesisBlockHeader testVer cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVer

onRestart ::
       ChainId
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
onRestart cid pdb bhdb r = do
    bhdb' <- bhdb
    block <- maxEntry bhdb'
    let nonce = Nonce $ fromIntegral $ _blockHeight block
    T3 _ b _ <- mineBlock block cid nonce pdb bhdb r
    assertEqual "Invalid BlockHeight" 9 (_blockHeight b)

testMemPoolAccess :: MemPoolAccess
testMemPoolAccess  = MemPoolAccess
    { mpaGetBlock = getTestBlock
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    ksData :: Text -> Value
    ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
    getTestBlock validate _bHeight _bHash bHeader = do
        let Nonce nonce = _blockNonce bHeader
            moduleStr = defModule (T.pack $ show nonce)
            d = Just $ ksData (T.pack $ show nonce)
        let txs = V.fromList $ [PactTransaction moduleStr d]
        outtxs <- goldenTestTransactions txs
        oks <- validate _bHeight _bHash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show txs
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks ]
        return outtxs

dupegenMemPoolAccess :: MemPoolAccess
dupegenMemPoolAccess  = MemPoolAccess
    { mpaGetBlock = getTestBlock
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    ksData :: Text -> Value
    ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
    getTestBlock validate _bHeight _bHash _bHeader = do
        let nonce = "0"
            moduleStr = defModule (T.pack nonce)
            d = Just $ ksData (T.pack nonce)
        let txs = V.fromList $ [PactTransaction moduleStr d]
        outtxs <- goldenTestTransactions txs
        oks <- validate _bHeight _bHash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show txs
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks ]
        return outtxs


firstPlayThrough
    :: BlockHeader
    -> ChainId
    -> IO (PayloadDb HashMapCas)
    -> IO (BlockHeaderDb)
    -> IO (TQueue RequestMsg)
    -> Assertion
firstPlayThrough genesisBlock c iopdb iobhdb rr = do
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
              ret@(T3 _ newblock _) <- liftIO $ mineBlock pblock c n iopdb iobhdb r
              liftIO $ modifyIORef' ncounter succ
              put newblock
              return ret

testDupes
  :: BlockHeader
  -> ChainId
  -> IO (PayloadDb HashMapCas)
  -> IO (BlockHeaderDb)
  -> IO (TQueue RequestMsg)
  -> Assertion
testDupes genesisBlock c iopdb iobhdb rr = do
    (T3 _ newblock payload) <- liftIO $ mineBlock genesisBlock c (Nonce 1) iopdb iobhdb rr
    expectException newblock payload $ liftIO $
        mineBlock newblock c (Nonce 2) iopdb iobhdb rr
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
