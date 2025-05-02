{-# language
    FlexibleContexts
  , ImportQualifiedPost
  , LambdaCase
  , NumericUnderscores
  , OverloadedStrings
  , PackageImports
  , TypeApplications
#-}

module Chainweb.Test.Pact.Utils
    -- ( initCheckpointer
    -- , pactTxFrom4To5

        -- * Logging
    ( getTestLogLevel
    , testLogFn
    , getTestLogger

        -- * Resources
    , withMempool
    , withBlockDbs

        -- * Properties
    , event
    , successfulTx
        -- * Utilities
    , coinModuleName
    )
    where

import Chainweb.Chainweb (validatingMempoolConfig)
import Chainweb.ChainId
import Chainweb.Logger
import Chainweb.Mempool.InMem
import Chainweb.Mempool.Mempool (MempoolBackend (..))
--import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types (SQLiteEnv)
import Chainweb.Pact.Backend.Utils (openSQLiteConnection, closeSQLiteConnection, chainwebPragmas)
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Storage.Table.RocksDB
-- import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
-- import Control.Concurrent hiding (throwTo)
-- import Control.Exception (AsyncException (..), throwTo)
-- import Control.Lens hiding (elements, only)
-- import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, allocate)
-- import Control.Monad.Trans.Resource qualified as Resource
-- import Data.Aeson qualified as Aeson
-- import Data.ByteString.Short qualified as SBS
-- import Data.HashSet (HashSet)
-- import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
-- import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
-- import Data.Vector (Vector)
-- import Data.Vector qualified as Vector
import Pact.Core.Command.Types qualified as Pact
-- import Pact.Core.Hash qualified as Pact
-- import Pact.Core.Pretty qualified as Pact
import Pact.Core.Gas qualified as Pact
-- import Pact.JSON.Encode qualified as J
import System.Environment (lookupEnv)
import System.LogLevel
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.Names

withBlockDbs :: ChainwebVersion -> RocksDb -> ResourceT IO (PayloadDb RocksDbTable, WebBlockHeaderDb)
withBlockDbs v rdb = do
    webBHDb <- liftIO $ initWebBlockHeaderDb rdb v
    let payloadDb = newPayloadDb rdb
    -- liftIO $ initializePayloadDb v payloadDb
    return (payloadDb, webBHDb)

-- | Internal. See https://www.sqlite.org/c3ref/open.html
withSQLiteResource
    :: String
    -> ResourceT IO SQLiteEnv
withSQLiteResource file = snd <$> allocate
    (openSQLiteConnection file chainwebPragmas)
    closeSQLiteConnection

withMempool
    :: (Logger logger)
    => logger
    -> ServiceEnv tbl
    -> ResourceT IO (MempoolBackend Pact.Transaction)
withMempool logger pact = do
    let mempoolCfg = validatingMempoolConfig
            (_chainId pact) (_chainwebVersion pact)
            (Pact.GasLimit $ Pact.Gas 150_000) (Pact.GasPrice 1e-8)
            (execPreInsertCheckReq logger pact)
    liftIO $ startInMemoryMempoolTest mempoolCfg

-- withRunPactService :: (Logger logger)
--     => logger
--     -> ChainwebVersion
--     -> ChainId
--     -> MempoolBackend Pact.Transaction
--     -> WebBlockHeaderDb
--     -> PayloadDb RocksDbTable
--     -> PactServiceConfig
--     -> ResourceT IO ()
-- withRunPactService logger v cid mempool webBHDb payloadDb pactServiceConfig = do
--     sqlite <- withTempSQLiteResource
--     blockHeaderDb <- liftIO $ getWebBlockHeaderDb webBHDb cid
--     mempoolConsensus <- liftIO $ mkMempoolConsensus mempool blockHeaderDb (Just payloadDb)
--     let mempoolAccess = pactMemPoolAccess mempoolConsensus logger

--     void $ Resource.allocate
--         (forkIO $ runPactService v cid logger Nothing pactQueue mempoolAccess blockHeaderDb payloadDb sqlite pactServiceConfig) --bhdb (_bdbPayloadDb tdb) sqlite pactServiceConfig)
--         (\tid -> throwTo tid ThreadKilled)

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

-- usually we don't want to check the module hash
event
    :: P.Prop Text
    -> P.Prop [PactValue]
    -> P.Prop ModuleName
    -> P.Prop (PactEvent PactValue)
event n args modName = P.checkAll
    [ P.fun _peName n
    , P.fun _peArgs args
    , P.fun _peModule modName
    ]

coinModuleName :: ModuleName
coinModuleName = ModuleName "coin" Nothing

successfulTx :: P.Prop (Pact.CommandResult log err)
successfulTx = P.fun Pact._crResult ? P.match Pact._PactResultOk P.succeed
