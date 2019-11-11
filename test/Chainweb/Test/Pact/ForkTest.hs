{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.ForkTest
  ( tests
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (Value)
import Data.CAS.RocksDB
import Data.String.Conv (toS)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Directory
import System.IO.Extra
import System.IO.Temp
import System.LogLevel

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command

-- internal modules
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.Types
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Test.ForkGen
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (withTempDir)
import Chainweb.Version

tests :: BlockHeaderDb -> BlockHeader -> ScheduledTest
tests db h0 = testGroupSch "pact-fork-quickcheck-tests"
    [ testProperty "prop-forkValidates" (prop_forkValidates db h0) ]

testVersion :: ChainwebVersion
testVersion = Development

-- | Property: Fork requiring checkpointer rewind validates properly
prop_forkValidates
    :: BlockHeaderDb
    -> BlockHeader
    -> Property
prop_forkValidates db genBlock = do
    let pairIO@(iofp, rocksIO) = rocksCreate
    _ <- withTempDir $ \dir ->
      withBlockHeaderDb rocksIO genBlock $ \bhdb ->
      withPayloadDb $ \pdb -> monadicIO $ do
          mapRef <- liftIO $ newIORef (HM.empty :: HashMap BlockHeader (HashSet TransactionHash))
          fi <- genFork db mapRef genBlock
          let blockList = blocksFromFork fi
          liftIO $ putStrLn $ "list of blocks:\n" ++ show blockList
          -- liftIO $ putStrLn $ show fi
          withPact testVersion Warn pdb bhdb testMemPoolAccess dir $ \reqQIO ->
              newBlockTest "new-block-0" reqQIO blockList
          assert (True == True) -- TODO: how to validate this test?
    (fp, rdb) <- pairIO
    rocksFree fp rdb

-- rocksCreate rocksFree pulled from withRocksResource
rocksCreate :: IO (FilePath, RocksDb)
rocksCreate = do
    sysdir <- getCanonicalTemporaryDirectory
    dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
    rocks <- openRocksDb dir
    return (dir, rocks)

rocksFree :: (FilePath, RocksDb) -> IO ()
rocksFree (dir, rocks) = do
    closeRocksDb rocks
    destroyRocksDb dir
    removeDirectoryRecursive dir
      `catchAllSynchronous` (const $ return ())

{-
withRocksResource :: (IO RocksDb -> TestTree) -> TestTree
withRocksResource m = withResource create destroy wrap
  where
    create = do
      sysdir <- getCanonicalTemporaryDirectory
      dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
      rocks <- openRocksDb dir
      return (dir, rocks)
    destroy (dir, rocks) = do
        closeRocksDb rocks
        destroyRocksDb dir
        removeDirectoryRecursive dir
          `catchAllSynchronous` (const $ return ())
    wrap ioact = let io' = snd <$> ioact in m io'
-}

forkValidatesTT :: BlockHeaderDb -> BlockHeader -> TestTree
forkValidatesTT db genBlock =
    testProperty "someName" $ prop_forkValidates db genBlock

-- withRocksResource :: (IO RocksDb -> TestTree) -> TestTree
-- withResource :: IO a -> (a -> IO ()) -> TestTree -> TestTree
-- testProperty :: Testable a => TestName -> a -> TestTree
--    (Property is an instance of Testable)




blocksFromFork :: ForkInfo -> [BlockHeader]
blocksFromFork ForkInfo{..} =
    fiPreForkHeaders ++ fiLeftForkHeaders ++ fiRightForkHeaders

----------------------------------------------------------------------------------------------------
-- Borrowed/modified from PactInProceApi test...
----------------------------------------------------------------------------------------------------
testMemPoolAccess :: MemPoolAccess
testMemPoolAccess = MemPoolAccess
    { mpaGetBlock = \validate bh hash _header ->
        getTestBlock validate bh hash
    , mpaSetLastHeader = \_ -> return ()
    , mpaProcessFork = \_ -> return ()
    }
  where
    getTestBlock validate bHeight bHash = do
        txs <- txsFromHeight bHeight
        let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
            g = modifyPayloadWithText . set (pMeta . pmTTL)
        outtxs' <- goldenTestTransactions txs
        let outtxs = flip V.map outtxs' $ \tx ->
                let ttl = TTLSeconds $ ParsedInteger $ 24 * 60 * 60
                in fmap ((g ttl) . (f (TxCreationTime $ ParsedInteger 1000000))) tx
        oks <- validate bHeight bHash outtxs
        when (not $ V.and oks) $ do
            fail $ mconcat [ "tx failed validation! input list: \n"
                           , show txs
                           , "\n\nouttxs: "
                           , show outtxs
                           , "\n\noks: "
                           , show oks ]
        return outtxs

txsFromHeight :: Int -> IO (Vector PactTransaction)
txsFromHeight 0 = do
    d <- adminData
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let f = modifyPayloadWithText . set (pMeta . pmCreationTime)
    let g = modifyPayloadWithText . set (pMeta . pmTTL)
    return $ V.fromList
        ( [ PactTransaction { _pactCode = (T.pack moduleStr) , _pactData = d }
          , PactTransaction { _pactCode = "(create-table test1.accounts)" , _pactData = d }
          , PactTransaction { _pactCode = "(test1.create-global-accounts)" , _pactData = d }
          ] V.++ commonTxs d)
txsFromheight h = do
    d <- adminData
    let escAccountStr = "\"Acct" ++ show h ++ "\""
    return $ V.fromList
        ( [ PactTransaction { _pactCode = toS ("(test1.createAccount " ++ escAccountStr ++ ")")
                            , _pactData = d }
          , PactTransaction { _pactCode = toS ("(test1.transfer \"Acct1\"" ++ escAccountStr ++ " 1.00)")
                            , _pactData = d }
          , PactTransaction { _pactCode = "(at 'prev-block-hash (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'block-time (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'block-height (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'gas-limit (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'gas-price (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'chain-id (chain-data))" , _pactData = d }
          , PactTransaction { _pactCode = "(at 'sender (chain-data))" , _pactData = d }
          ] ++ commonTxs d )

commonTxs :: (Maybe Value) -> [PactTransaction]
commonTxs d =
    [ PactTransaction { _pactCode = "(at 'prev-block-hash (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-time (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'block-height (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-limit (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'gas-price (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'chain-id (chain-data))", _pactData = d }
    , PactTransaction { _pactCode = "(at 'sender (chain-data))", _pactData = d }
    ]

newBlockTest :: String -> IO PactQueue -> [BlockHeader] -> TestTree
newBlockTest label reqIO blocks = golden label $ do
    reqQ <- reqIO
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    newBlocksToQueue blocks
    readNFromQueue reqQ (length blocks)
  where
    cid = someChainId testVersion

newBlocksToQueue :: [BlockHeader] -> PactQueue -> IO ()
newBlocksToQueue blocks reqQ = do
    let blockTime = Time $ secondsToTimeSpan $ Seconds $ succ 1000000
    forM blocks $ \h -> do
        respVar <- newBlock noMiner h (BlockCreationTime blockTime) reqQ
        -- how to validate the response...?
        return ()

readNFromQueue :: PactQueue -> Int -> _
readNFromQueue reqQ 0 = undefined
readNFromQueue reqQ n = undefined
  -- readNFromQueue reqQ (n-1)
