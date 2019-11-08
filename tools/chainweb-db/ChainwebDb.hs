{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: ChainwebDb
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module ChainwebDb
( main
) where

import Chainweb.WebBlockHeaderDB

import Configuration.Utils

import Control.Concurrent.Async
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Retry

import Data.CAS
import Data.CAS.RocksDB
import Data.Reflection
import qualified Data.Text as T

import GHC.Generics hiding (from)

import Network.HTTP.Client

import Servant.Client

import qualified Streaming.Prelude as S

import qualified System.Logger as Y
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis
import Chainweb.BlockHeaderDB
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.RestAPI.Client
import Chainweb.TreeDB
import Chainweb.TreeDB.RemoteDB
import Chainweb.Utils hiding (Codec)
import Chainweb.Version

import Data.LogMessage

-- -------------------------------------------------------------------------- --
--

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configNodes :: ![HostAddress]
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultNodes :: [HostAddress]
defaultNodes = []

defaultConfig :: Config
defaultConfig = Config
    { _configLogHandle = Y.StdOut
    , _configLogLevel = Y.Info
    , _configChainwebVersion = Mainnet01
    , _configNodes = defaultNodes
    }

instance ToJSON Config where
    toJSON o = object
        [ "logHandle" .= _configLogHandle o
        , "logLevel" .= _configLogLevel o
        , "chainwebVersion" .= _configChainwebVersion o
        , "nodes" .= _configNodes o
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< configLogHandle ..: "logHandle" % o
        <*< configLogLevel ..: "logLevel" % o
        <*< configChainwebVersion ..: "ChainwebVersion" % o
        <*< configNodes . from leftMonoidalUpdate %.: "nodes" % o

pConfig :: MParser Config
pConfig = id
    <$< configLogHandle .:: Y.pLoggerHandleConfig
    <*< configLogLevel .:: Y.pLogLevel
    <*< configChainwebVersion .:: option textReader
        % long "chainweb-version"
        <> help "chainweb version identifier"
    <*< configNodes %:: pLeftMonoidalUpdate . fmap pure % textOption
        % long "node"
        <> short 'n'
        <> help "node that is synced"

env :: Manager -> HostAddress -> ClientEnv
env mgr h = mkClientEnv mgr (hostAddressBaseUrl h)

hostAddressBaseUrl :: HostAddress -> BaseUrl
hostAddressBaseUrl h = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = show (_hostAddressHost h)
    , baseUrlPort = fromIntegral (_hostAddressPort h)
    , baseUrlPath = ""
    }

-- -------------------------------------------------------------------------- --
-- Remote BlockHeaderDb

devNetDb :: Config -> Manager -> LogFunction -> ChainId -> HostAddress -> RemoteDb
devNetDb c mgr l cid node = mkDb (_configChainwebVersion c) cid mgr l node

-- TreeDB

mkDb
    :: HasChainwebVersion v
    => HasChainId cid
    => v
    -> cid
    -> Manager
    -> LogFunction
    -> HostAddress
    -> RemoteDb
mkDb v c mgr logg h = RemoteDb
    (env mgr h)
    (ALogFunction logg)
    (_chainwebVersion v)
    (_chainId c)

-- -------------------------------------------------------------------------- --
-- Payloads

devNetPayload :: Config -> Manager -> HostAddress -> ChainId -> BlockPayloadHash -> IO PayloadData
devNetPayload config  mgr node cid x
    = runClientM (payloadClient ver cid x) (env mgr node) >>= \case
        Left e -> error (show e)
        Right a -> return a
  where
    ver = _configChainwebVersion config

-- -------------------------------------------------------------------------- --
-- Synchronize DB with Chain

syncChain
    :: Logger l
    => Config
    -> Manager
    -> l
    -> BlockHeaderDb
    -> PayloadDataDb
    -> ChainId
    -> HostAddress
    -> IO ()
syncChain config mgr logger lhdb pdb cid node = void $ do
        entries rhdb Nothing Nothing Nothing Nothing $ \s -> s
            & S.chain (logg @T.Text Debug . sshow)
            & S.chain
                (\x -> when (_blockHeight x `mod` 1000 == 0) $
                    logg @T.Text Info ("BlockHeight: " <> sshow (_blockHeight x))
                )
            & S.chain (insert lhdb)
            & S.mapM (\h -> retry $ devNetPayload config mgr node cid $ _blockPayloadHash h)
            & S.mapM_ (casInsert pdb)
  where
    logg :: LogFunction
    logg = logFunction logger

    retry a = recoverAll policy $ \s -> do
        unless (rsIterNumber s == 0) $ logg @T.Text Warn $ "retry: " <> sshow (rsIterNumber s)
        a

    rhdb = devNetDb config mgr logg cid node
    policy = exponentialBackoff 600000 <> limitRetries 6

-- -------------------------------------------------------------------------- --
-- Payload Data Store

-- | Initialize a PayloadDb with genesis payloads for the given chainweb
-- version.
--
initializePayloadDataDb
    :: ChainwebVersion
    -> PayloadDataDb
    -> IO ()
initializePayloadDataDb v db = mapM_ initForChain $ chainIds v
  where
    initForChain cid = casInsert db $ PayloadData
        { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions g
        , _payloadDataMiner = _payloadWithOutputsMiner g
        , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash g
        , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash g
        , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash g
        }
      where
        g = genesisBlockPayload v cid

type PayloadDataDb = RocksDbCas PayloadData

newPayloadDbCas
    :: (ToJSON v, FromJSON v, CasKeyType v ~ BlockPayloadHash)
    => RocksDb
    -> RocksDbCas v
newPayloadDbCas rdb = newCas rdb
    (Codec encodeToByteString decodeStrictOrThrow')
    (Codec (runPut . encodeBlockPayloadHash) (runGet decodeBlockPayloadHash))
    ["PayloadData"]

-- -------------------------------------------------------------------------- --
-- main

run :: Logger l => Config -> l -> IO ()
run config logger = withRocksDb "chainweb-data" $ \rdb -> do
    mgr <- unsafeManager 10000000
    wbhdb <- initWebBlockHeaderDb rdb v
    let payloadDb = newPayloadDbCas rdb
    initializePayloadDataDb v payloadDb

    forConcurrently_ (_configNodes config) $ \node -> do
        let logger' = addLabel ("node", toText node) logger

        forConcurrently_ (chainIds v) $ \cid -> do
            let logger'' = addLabel ("chain", toText cid) logger'
            hdb <- give wbhdb $ getWebBlockHeaderDb cid
            (logFunction logger'') @T.Text Info $ "start syncing"
            syncChain config mgr logger'' hdb payloadDb cid node
  where
    v = _configChainwebVersion config

mainWithConfig :: Config -> IO ()
mainWithConfig config = withLog $ \logger -> do
    let logger' = logger
            & addLabel ("version", toText $ _configChainwebVersion config)
    liftIO $ run config logger'
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ (_configLogLevel config)
        & Y.logConfigBackend . Y.handleBackendConfigHandle .~ _configLogHandle config
    withLog inner = Y.withHandleBackend_ logText (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

main :: IO ()
main = runWithConfiguration pinfo mainWithConfig
  where
    pinfo = programInfo "Chainweb Database" pConfig defaultConfig

