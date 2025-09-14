{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact.Utils
(
-- * Logging
  getTestLogLevel
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
import Chainweb.Pact.Mempool.InMem
import Chainweb.Pact.Mempool.Mempool (MempoolBackend (..))
import Chainweb.Pact.PactService
import Chainweb.Pact.Payload.PayloadStore
import Chainweb.Pact.Payload.PayloadStore.RocksDB
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Types
import Chainweb.Storage.Table.RocksDB
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Pact.Core.Capabilities
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.Gas qualified as Pact
import Pact.Core.Names
import Pact.Core.PactValue
import PropertyMatchers ((?))
import PropertyMatchers qualified as P
import System.Environment (lookupEnv)
import System.LogLevel

withBlockDbs :: HasVersion => RocksDb -> ResourceT IO (PayloadDb RocksDbTable, WebBlockHeaderDb)
withBlockDbs rdb = do
    webBHDb <- liftIO $ initWebBlockHeaderDb rdb
    let payloadDb = newPayloadDb rdb
    return (payloadDb, webBHDb)

withMempool
    :: (Logger logger)
    => HasVersion
    => logger
    -> ServiceEnv tbl
    -> ResourceT IO (MempoolBackend Pact.Transaction)
withMempool logger pact = do
    let mempoolCfg = validatingMempoolConfig
            (_chainId pact)
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
