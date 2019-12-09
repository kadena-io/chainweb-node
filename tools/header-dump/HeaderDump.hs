{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: HeaderDump
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module HeaderDump
( main
) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Payload.PayloadStore.Types
import Chainweb.Transaction
import Chainweb.TreeDB hiding (key)
import Chainweb.Utils
import Chainweb.Version

import Configuration.Utils
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Except

import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Aeson.Lens
import Data.CAS
import Data.CAS.RocksDB
import Data.Either
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.LogMessage
import Data.Semigroup hiding (option)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Database.RocksDB.Base as R

import GHC.Generics

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.Directory
import qualified System.Logger as Y
import System.LogLevel

-- -------------------------------------------------------------------------- --
-- Configuration

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !ChainId
    , _configDatabaseDirectory :: !(Maybe FilePath)
    , _configPretty :: !Bool
    , _configStart :: !(Maybe (Min Natural))
    , _configEnd :: !(Maybe (Max Natural))

    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
    { _configLogHandle = Y.StdOut
    , _configLogLevel = Y.Info
    , _configChainwebVersion = Development
    , _configChainId = someChainId devVersion
    , _configPretty = True
    , _configDatabaseDirectory = Nothing
    , _configStart = Nothing
    , _configEnd = Nothing
    }
  where
    devVersion = Development

instance ToJSON Config where
    toJSON o = object
        [ "logHandle" .= _configLogHandle o
        , "logLevel" .= _configLogLevel o
        , "chainwebVersion" .= _configChainwebVersion o
        , "chainId" .= _configChainId o
        , "pretty" .= _configPretty o
        , "databaseDirectory" .= _configDatabaseDirectory o
        , "start" .= _configStart o
        , "end" .= _configEnd o
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< configLogHandle ..: "logHandle" % o
        <*< configLogLevel ..: "logLevel" % o
        <*< configChainwebVersion ..: "ChainwebVersion" % o
        <*< configChainId ..: "chainId" % o
        <*< configPretty ..: "pretty" % o
        <*< configDatabaseDirectory ..: "databaseDirectory" % o
        <*< configStart ..: "start" % o
        <*< configEnd ..: "end" % o

pConfig :: MParser Config
pConfig = id
    <$< configLogHandle .:: Y.pLoggerHandleConfig
    <*< configLogLevel .:: Y.pLogLevel
    <*< configChainwebVersion .:: option textReader
        % long "chainweb-version"
        <> help "chainweb version identifier"
    <*< configChainId .:: option textReader
        % long "chain-id"
        <> short 'c'
        <> help "chain id to query"
    <*< configPretty .:: boolOption_
        % long "pretty"
        <> short 'p'
        <> help "print prettyfied JSON. Uses multiple lines for one transaction"
    <*< configDatabaseDirectory .:: fmap Just % textOption
        % long "database-directory"
        <> short 'd'
        <> help "directory where the databases are persisted"
    <*< configStart .:: fmap (Just . int @Natural) % option auto
        % long "start"
        <> short 's'
        <> help "start block height"
    <*< configEnd .:: fmap (Just . int @Natural) % option auto
        % long "end"
        <> short 'e'
        <> help "end block height"

validateConfig :: ConfigValidation Config []
validateConfig o = do
    checkIfValidChain (_configChainId o)
    mapM_ (validateDirectory "databaseDirectory") (_configDatabaseDirectory o)
  where
    chains = chainIds $ _configChainwebVersion o
    checkIfValidChain cid = unless (HS.member cid chains)
        $ throwError $ "Invalid chain id provided: " <> toText cid

-- -------------------------------------------------------------------------- --
-- Main

mainWithConfig :: Config -> IO ()
mainWithConfig config = withLog $ \logger -> do
    liftIO $ run config $ logger
        & addLabel ("version", toText $ _configChainwebVersion config)
        & addLabel ("chain", toText $ _configChainId config)
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ (_configLogLevel config)
        & Y.logConfigBackend . Y.handleBackendConfigHandle .~ _configLogHandle config
    withLog inner = Y.withHandleBackend_ logText (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

main :: IO ()
main = runWithConfiguration pinfo mainWithConfig
  where
    pinfo = programInfoValidate
        "Dump all block headers of a chain as JSON array"
        pConfig
        defaultConfig
        validateConfig

run :: Logger l => Config -> l -> IO ()
run config logger = do
    rocksDbDir <- getRocksDbDir
    logg Info $ "using database at: " <> T.pack rocksDbDir
    withRocksDb_ rocksDbDir $ \rdb -> do
        void $ withBlockHeaderDb rdb v cid $ \cdb -> do
            let pdb = newPayloadDb rdb
            logg Info "start dumping block headers"
            T.putStr "[\n"
            void $ entries cdb Nothing Nothing (MinRank <$> _configStart config) (MaxRank <$> _configEnd config) $ \s -> s
                & S.mapM (fmap encodeJson . blockHeaderWithCodeJSON pdb)
                & S.intersperse ",\n"
                & S.mapM_ T.putStr
            T.putStr "\n]"
  where
    logg :: LogFunctionText
    logg = logFunction logger
    v = _configChainwebVersion config
    cid = _configChainId config

    getRocksDbDir = case _configDatabaseDirectory config of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> "0" <> "/rocksDb"
        Just d -> return d

    encodeJson
        | _configPretty config = T.decodeUtf8 . BL.toStrict . encodePretty
        | otherwise = encodeToText

-- -------------------------------------------------------------------------- --
--

withRocksDb_ :: FilePath -> (RocksDb -> IO a) -> IO a
withRocksDb_ path = bracket (openRocksDb_ path) closeRocksDb_
  where
    openRocksDb_ :: FilePath -> IO RocksDb
    openRocksDb_ p = do
        db <- RocksDb <$> R.open p opts <*> mempty
        initializeRocksDb_ db
        return db

    opts = R.defaultOptions { R.createIfMissing = False }

    initializeRocksDb_ :: RocksDb -> IO ()
    initializeRocksDb_ db = R.put
        (_rocksDbHandle db)
        R.defaultWriteOptions
        (_rocksDbNamespace db <> ".")
        ""

    closeRocksDb_ :: RocksDb -> IO ()
    closeRocksDb_ = R.close . _rocksDbHandle

blockHeaderWithCodeJSON :: PayloadDb RocksDbCas -> BlockHeader -> IO Value
blockHeaderWithCodeJSON pdb b = do

    payload <- payloadWithOutputsToPayloadData
        <$> casLookupM pdb (_blockPayloadHash b)

    minerInfo <- toJSON <$> fromMinerData (_payloadDataMiner payload)

    return $ toPayloadValue payload
      & key "payload" . _Object
      %~ HM.insert "minerInfo" minerInfo . HM.delete "minerData"

  where
    toPayloadValue payload =
      object
        [ "nonce" .= _blockNonce b
        , "creationTime" .= _blockCreationTime b
        , "parent" .= _blockParent b
        , "adjacents" .= _blockAdjacentHashes b
        , "target" .= _blockTarget b
        , "payload" .= payload
        , "chainId" .= _chainId b
        , "weight" .= _blockWeight b
        , "height" .= _blockHeight b
        , "chainwebVersion" .= _blockChainwebVersion b
        , "epochStart" .= _blockEpochStart b
        , "featureFlags" .= _blockFlags b
        , "hash" .= _blockHash b
        ]

_transactionsFromPayload :: PayloadData -> IO (Vector ChainwebTransaction)
_transactionsFromPayload plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> (T.intercalate ". " ls)
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode chainwebPayloadCodec $
                                          _transactionBytes bs))
