{-# language
    FlexibleContexts
  , ImportQualifiedPost
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , PackageImports
  , TypeApplications
#-}

module Chainweb.Test.Pact5.Utils
    ( initCheckpointer
    , pactTxFrom4To5

        -- * Logging
    , getTestLogLevel
    , testLogFn
    , getTestLogger

        -- * Mempool
    , insertMempool
    , lookupMempool

        -- * Resources
    , withTempSQLiteResource
    , withInMemSQLiteResource
    , withTestRocksDb
    , withPactQueue
    , withMempool
    , withRunPactService
    , withBlockDbs
    , withTestBlockHeaderDb
    , testRocksDb
    )
    where

import Chainweb.Chainweb (validatingMempoolConfig)
import "pact" Pact.Types.Command qualified as Pact4
import "pact" Pact.Types.Hash qualified as Pact4
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb, initBlockHeaderDb, closeBlockHeaderDb)
import Chainweb.BlockHeaderDB qualified as BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.Consensus
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (InsertType (..), LookupResult(..), MempoolBackend (..), TransactionHash(..))
--import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.PactService.Checkpointer.Internal (initCheckpointerResources)
import Chainweb.Pact.Backend.Types (Checkpointer, IntraBlockPersistence(..), SQLiteEnv)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService
import Chainweb.Pact.PactService.Pact4.ExecBlock ()
import Chainweb.Pact.Service.PactInProcApi
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Types
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact5.Transaction qualified as Pact5
import Pact.Core.Pretty qualified as Pact5
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Storage.Table.RocksDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService
import Control.Concurrent hiding (throwTo)
import Control.Exception (AsyncException (..), finally, throwTo)
import Control.Lens hiding (elements, only)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Control.Monad.Trans.Resource qualified as Resource
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SBS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.RocksDB.Internal qualified as R
import Pact.Core.Command.Types qualified as Pact5
import Pact.Core.Hash qualified as Pact5
import Pact.JSON.Encode qualified as J
import Pact.Types.Gas qualified as Pact4
import System.Environment (lookupEnv)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.LogLevel
import System.Random (randomIO)

withBlockDbs :: ChainwebVersion -> RocksDb -> ResourceT IO (PayloadDb RocksDbTable, WebBlockHeaderDb)
withBlockDbs v rdb = do
    testRdb <- liftIO $ testRocksDb "withBlockDbs" rdb
    webBHDb <- liftIO $ initWebBlockHeaderDb testRdb v
    let payloadDb = newPayloadDb testRdb
    liftIO $ initializePayloadDb v payloadDb
    return (payloadDb, webBHDb)

testBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> IO BlockHeaderDb
testBlockHeaderDb rdb h = do
    rdb' <- testRocksDb "withTestBlockHeaderDb" rdb
    initBlockHeaderDb (BlockHeaderDB.Configuration h rdb')

withTestBlockHeaderDb
    :: RocksDb
    -> BlockHeader
    -> ResourceT IO BlockHeaderDb
withTestBlockHeaderDb rdb h =
    snd <$> allocate (testBlockHeaderDb rdb h) closeBlockHeaderDb

testRocksDb
    :: ByteString -- ^ Prefix
    -> RocksDb
    -> IO RocksDb
testRocksDb l r = do
    prefix <- (<>) l . sshow <$> (randomIO @Word64)
    return r { _rocksDbNamespace = prefix }

withTestRocksDb :: ResourceT IO RocksDb
withTestRocksDb = view _2 . snd <$> allocate create destroy
    where
        create = do
            sysdir <- getCanonicalTemporaryDirectory
            dir <- createTempDirectory sysdir "chainweb-rocksdb-tmp"
            opts@(R.Options' opts_ptr _ _) <- R.mkOpts modernDefaultOptions
            rocks <- openRocksDb dir opts_ptr
            return (dir, rocks, opts)

        destroy (dir, rocks, opts) =
            closeRocksDb rocks `finally`
                R.freeOpts opts `finally`
                destroyRocksDb dir

-- | Internal. See https://www.sqlite.org/c3ref/open.html
withSQLiteResource
    :: String
    -> ResourceT IO SQLiteEnv
withSQLiteResource file = snd <$> allocate
    (openSQLiteConnection file chainwebPragmas)
    closeSQLiteConnection

-- | Open a temporary file-backed SQLite database.
withTempSQLiteResource :: ResourceT IO SQLiteEnv
withTempSQLiteResource = withSQLiteResource ""

-- | Open a temporary in-memory SQLite database.
withInMemSQLiteResource :: ResourceT IO SQLiteEnv
withInMemSQLiteResource = withSQLiteResource ":memory:"

withPactQueue :: ResourceT IO PactQueue
withPactQueue = do
    liftIO (newPactQueue 2_000)

withMempool :: ()
    => ChainwebVersion
    -> ChainId
    -> PactQueue
    -> ResourceT IO (MempoolBackend Pact4.UnparsedTransaction)
withMempool v cid pactQueue = do
    pactExecutionServiceVar <- liftIO $ newMVar (mkPactExecutionService pactQueue)
    let mempoolCfg = validatingMempoolConfig cid v (Pact4.GasLimit 150_000) (Pact4.GasPrice 1e-8) pactExecutionServiceVar
    liftIO $ startInMemoryMempoolTest mempoolCfg

withRunPactService :: (Logger logger)
    => logger
    -> ChainwebVersion
    -> ChainId
    -> PactQueue
    -> MempoolBackend Pact4.UnparsedTransaction
    -> WebBlockHeaderDb
    -> PayloadDb RocksDbTable
    -> PactServiceConfig
    -> ResourceT IO ()
withRunPactService logger v cid pactQueue mempool webBHDb payloadDb pactServiceConfig = do
    sqlite <- withTempSQLiteResource
    blockHeaderDb <- liftIO $ getWebBlockHeaderDb webBHDb cid
    mempoolConsensus <- liftIO $ mkMempoolConsensus mempool blockHeaderDb (Just payloadDb) --bhdb (Just (_bdbPayloadDb tdb))
    let mempoolAccess = pactMemPoolAccess mempoolConsensus logger

    void $ Resource.allocate
        (forkIO $ runPactService v cid logger Nothing pactQueue mempoolAccess blockHeaderDb payloadDb sqlite pactServiceConfig) --bhdb (_bdbPayloadDb tdb) sqlite pactServiceConfig)
        (\tid -> throwTo tid ThreadKilled)

-- | Insert a 'Pact5.Transaction' into the mempool. The mempool currently operates by default on
--   'Pact4.UnparsedTransaction's, so the txs have to be converted.
insertMempool :: MempoolBackend Pact4.UnparsedTransaction -> InsertType -> [Pact5.Transaction] -> IO ()
insertMempool mp insertType txs = do
    let unparsedTxs :: [Pact4.UnparsedTransaction]
        unparsedTxs = flip map txs $ \tx ->
            case codecDecode Pact4.rawCommandCodec (codecEncode Pact5.payloadCodec tx) of
                Left err -> error err
                Right a -> a
    mempoolInsert mp insertType $ Vector.fromList unparsedTxs

-- | Looks up transactions in the mempool. Returns a set which indicates pending membership of the mempool.
lookupMempool :: MempoolBackend Pact4.UnparsedTransaction -> Vector Pact5.Hash -> IO (HashSet Pact5.Hash)
lookupMempool mp hashes = do
    results <- mempoolLookup mp $ Vector.map (TransactionHash . Pact5.unHash) hashes
    return $ HashSet.fromList $ Vector.toList $ flip Vector.mapMaybe results $ \case
        Missing -> Nothing
        Pending tx -> Just $ Pact5.Hash $ Pact4.unHash $ Pact4.toUntypedHash $ Pact4._cmdHash tx

-- | Initializes a checkpointer for a given chain.
initCheckpointer :: ChainwebVersion -> ChainId -> SQLiteEnv -> IO (Checkpointer GenericLogger)
initCheckpointer v cid sql = do
    logLevel <- getTestLogLevel
    initCheckpointerResources defaultModuleCacheLimit sql DoNotPersistIntraBlockWrites (genericLogger logLevel (testLogFn logLevel)) v cid

pactTxFrom4To5 :: Pact4.Transaction -> Pact5.Transaction
pactTxFrom4To5 tx =
  let
    e = do
      let json = J.encode (fmap (Text.decodeUtf8 . SBS.fromShort . Pact4.payloadBytes) tx)
      cmdWithPayload <- Aeson.eitherDecode @(Pact5.Command Text) json
      over _Left Pact5.renderCompactString $ Pact5.parseCommand cmdWithPayload
  in
  case e of
    Left err -> error err
    Right cmds -> cmds

getTestLogLevel :: IO LogLevel
getTestLogLevel = do
    let parseLogLevel txt = case Text.toUpper txt of
            "DEBUG" -> Debug
            "INFO" -> Info
            "WARN" -> Warn
            "ERROR" -> Error
            _ -> Error
    fromMaybe Error . fmap (parseLogLevel . Text.pack) <$> lookupEnv "CHAINWEB_TEST_LOG_LEVEL"

-- | Generally, we want tests to throw an exception on an Error log, but we don't want
--   to throw an exception on any other level of log.
testLogFn :: LogLevel -> Text -> IO ()
testLogFn ll msg = case ll of
    Error -> do
        error (Text.unpack msg)
    _ -> do
        Text.putStrLn msg

getTestLogger :: IO GenericLogger
getTestLogger = do
    logLevel <- getTestLogLevel
    return $ genericLogger logLevel (testLogFn logLevel)