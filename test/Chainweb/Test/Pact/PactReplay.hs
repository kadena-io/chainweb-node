{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}

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

import Data.Aeson
import Data.CAS.HashMap
import Data.CAS.RocksDB
-- import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple.Strict (T3(..))
import Data.Vector (Vector)
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
import Chainweb.Miner.POW
import Chainweb.NodeId
import Chainweb.Pact.Backend.Types
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
import Chainweb.Time
import Chainweb.TreeDB
import Chainweb.Transaction
import Chainweb.Version

testVersion :: ChainwebVersion
testVersion = Testnet00

tests :: ScheduledTest
tests =
    ScheduledTest label $
    withRocksResource $ \rocksIO ->
      withPayloadDb $ \pdb ->
        withBlockHeaderDb rocksIO genblock $ \bhdb ->
          withTemporaryDir $ \dir ->
            withTransactionsMap $ \mm ->
            testGroup
              label
              [ withPact pdb bhdb testMemPoolAccess' dir $ \reqQIO ->
                  testCase "initial-playthrough" $
                  firstPlayThrough genblock cid pdb bhdb mm reqQIO
              , after AllSucceed "initial-playthrough" $
                withPact pdb bhdb testMemPoolAccess' dir $ \reqQIO ->
                  testCase "on-restart" $ onRestart cid pdb bhdb reqQIO
              ]
  where
    genblock = genesisBlockHeader testVersion cid
    label = "Chainweb.Test.Pact.PactReplay"
    cid = someChainId testVersion

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


testMemPoolAccess' :: MemPoolAccess
testMemPoolAccess'  = MemPoolAccess
    { mpaGetBlock = getTestBlock
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    ksData :: Text -> Value
    ksData idx = object [("k" <> idx) .= object [ "keys" .= ([] :: [Text]), "pred" .= String ">=" ]]
    getTestBlock _bHeight _bHash bHeader = do
        -- moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
        let Nonce nonce = _blockNonce bHeader
            moduleStr = defModule (T.pack $ show nonce)
            d = Just $ ksData (T.pack $ show nonce)
        print nonce
        -- d <- adminData
        let txs = V.fromList [ PactTransaction moduleStr d]
        goldenTestTransactions txs

{-
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
-}

firstPlayThrough ::
            BlockHeader
            -> ChainId
            -> IO (PayloadDb HashMapCas)
            -> IO (BlockHeaderDb)
            -> IO (MVar TransactionsMap)
            -> IO (TQueue RequestMsg)
            -> Assertion
firstPlayThrough genesisBlock c iopdb iobhdb iotmap rr = do
    nonceCounter <- newIORef (0 :: Word64)
    mainlineblocks <- mineLine genesisBlock nonceCounter 7
    -- mapM_ (\(T3 _ b _) -> print $ _blockHash b) mainlineblocks
    let T3 _ startline1 _ = mainlineblocks !! 0
    let T3 _ startline2 _ = mainlineblocks !! 1
    void $ mineLine startline1 nonceCounter 4
    void $ mineLine startline2 nonceCounter 4
    return ()
  where
    mineLine start ncounter len =
      evalStateT (runReaderT (mapM go [startHeight :: Word64 .. (startHeight + len)]) rr) start
        where
          startHeight = fromIntegral $ _blockHeight start
          go _height = do
            r <- ask
            pblock <- get
            n <- liftIO $ Nonce <$> readIORef ncounter
            ret@(T3 pnewblock newblock _) <- liftIO $ mineBlock pblock c n iopdb iobhdb r
            liftIO $ modifyIORef' ncounter succ
            tmap <- liftIO iotmap
            liftIO $ modifyMVar_ tmap $
              return . M.insert (_blockHeight newblock, _blockHash newblock, pnewblock) undefined
            put newblock
            return ret

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

{-
withLastBlock :: (IO (MVar BlockHeader) -> TestTree) -> TestTree
withLastBlock = withResource newEmptyMVar (const $ return ())
-}

type TransactionsMap
   = Map (BlockHeight, BlockHash, BlockHeader) (Vector ChainwebTransaction)

withTransactionsMap :: (IO (MVar TransactionsMap) -> TestTree) -> TestTree
withTransactionsMap = withResource (newMVar M.empty) (const $ return ())

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
;; (insertTbl "a" 1)
|]
