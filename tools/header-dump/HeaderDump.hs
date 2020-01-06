{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
, run
, mainWithConfig
, withChainDbs

-- * Configuration
, Output(..)
, Config(..)
, defaultConfig

-- * Tools
, progress
, miner
, payloads
, coinbaseOutput
, coinbaseResult
, pactResult
, failures
, transactionsWithOutputs
, commandValue
, commandWithOutputsValue
) where

import Configuration.Utils hiding (Lens)
import Configuration.Utils.Validation

import Control.Arrow
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except

import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Bitraversable
import qualified Data.ByteString.Lazy as BL
import Data.CAS
import Data.CAS.RocksDB
import qualified Data.CaseInsensitive as CI
import Data.Functor.Of
import qualified Data.HashSet as HS
import Data.LogMessage
import Data.Semigroup hiding (option)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Database.RocksDB.Base as R

import GHC.Generics hiding (to)

import Numeric.Natural

import qualified Streaming.Prelude as S

import System.Directory
import qualified System.Logger as Y
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Pact.Types.Command
import Pact.Types.PactError

-- -------------------------------------------------------------------------- --

#define REMOTE_DB 0

#if REMOTE_DB
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Servant.Client

import Chainweb.HostAddress
import Chainweb.TreeDB.RemoteDB
#endif

-- -------------------------------------------------------------------------- --
-- Output types

data Output
    = Header
    | OutputMiner
    | OutputCoinbaseOutput
    | OutputCoinebaseResult
    | CoinbaseFailure
    | OutputTransaction
    | OutputPayload
    | OutputRawPayload
    | OutputAll
    deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance HasTextRepresentation Output where
    fromText a = case CI.mk a of
        "header" -> return Header
        "miner" -> return OutputMiner
        "coinbase-output" -> return OutputCoinbaseOutput
        "coinbase-result" -> return OutputCoinebaseResult
        "coinbase-failure" -> return CoinbaseFailure
        "transaction" -> return OutputTransaction
        "payload" -> return OutputPayload
        "raw-payload" -> return OutputRawPayload
        "all" -> return OutputAll
        o -> throwM $ DecodeException $ "unknown result type: " <> sshow o

    toText Header = "header"
    toText OutputMiner = "miner"
    toText OutputCoinbaseOutput = "coinbase-output"
    toText OutputCoinebaseResult = "coinbase-result"
    toText CoinbaseFailure = "coinbase-failure"
    toText OutputTransaction = "transaction"
    toText OutputPayload = "payload"
    toText OutputRawPayload = "raw-payload"
    toText OutputAll = "all"

instance FromJSON Output where
    parseJSON = parseJsonFromText "Output"

instance ToJSON Output where
    toJSON = toJSON . toText

-- -------------------------------------------------------------------------- --
-- Configuration

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !ChainId
    , _configDatabasePath :: !(Maybe FilePath)
    , _configPretty :: !Bool
    , _configStart :: !(Maybe (Min Natural))
    , _configEnd :: !(Maybe (Max Natural))
    , _configOutput :: !Output
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
    , _configDatabasePath = Nothing
    , _configStart = Nothing
    , _configEnd = Nothing
    , _configOutput = Header
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
        , "database" .= _configDatabasePath o
        , "start" .= _configStart o
        , "end" .= _configEnd o
        , "output" .= _configOutput o
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> id
        <$< configLogHandle ..: "logHandle" % o
        <*< configLogLevel ..: "logLevel" % o
        <*< configChainwebVersion ..: "ChainwebVersion" % o
        <*< configChainId ..: "chainId" % o
        <*< configPretty ..: "pretty" % o
        <*< configDatabasePath ..: "database" % o
        <*< configStart ..: "start" % o
        <*< configEnd ..: "end" % o
        <*< configOutput ..: "output" % o

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
    <*< configDatabasePath .:: fmap Just % textOption
        % long "database"
        <> short 'd'
        <> help "location of the databases"
    <*< configStart .:: fmap (Just . int @Natural) % option auto
        % long "start"
        <> short 's'
        <> help "start block height"
    <*< configEnd .:: fmap (Just . int @Natural) % option auto
        % long "end"
        <> short 'e'
        <> help "end block height"
    <*< configOutput .:: textOption
        % long "output"
        <> short 'o'
        <> help "output type"

validateConfig :: ConfigValidation Config []
validateConfig o = do
    checkIfValidChain (_configChainId o)
    mapM_ (validateDirectory "database") (_configDatabasePath o)
  where
    chains = chainIds $ _configChainwebVersion o
    checkIfValidChain cid = unless (HS.member cid chains)
        $ throwError $ "Invalid chain id provided: " <> toText cid

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

-- -------------------------------------------------------------------------- --
-- Print Transactions

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

withChainDbs
    :: Logger l
    => l
    -> Config
    -> (forall cas . PayloadCas cas => PayloadDb cas -> BlockHeaderDb -> IO a)
    -> IO a
withChainDbs logger config inner = do
    rocksDbDir <- getRocksDbDir
    logg Info $ "using database at: " <> T.pack rocksDbDir
    withRocksDb_ rocksDbDir $ \rdb -> do
        let pdb = newPayloadDb rdb
        initializePayloadDb v pdb
        withBlockHeaderDb rdb v cid $ \cdb -> do
            inner pdb cdb
  where
    logg :: LogFunctionText
    logg = logFunction logger
    v = _configChainwebVersion config
    cid = _configChainId config

    getRocksDbDir = case _configDatabasePath config of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> "0" <> "/rocksDb"
        Just d -> return d

run :: Logger l => Config -> l -> IO ()
run config logger = withChainDbs logger config $ \pdb cdb -> do
    liftIO $ logg Info "start traversing block headers"
    entries cdb Nothing Nothing (MinRank <$> _configStart config) (MaxRank <$> _configEnd config) $ \x -> x
        & void
        & progress logg

        & \s -> case _configOutput config of
            Header -> s
                & S.map ObjectEncoded
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputRawPayload -> s
                & payloads pdb id
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputTransaction -> s
                & payloads pdb id
                & transactionsWithOutputs _2
                & S.map (encodeJson @(BlockHeight, V.Vector Value))
                & S.mapM_ T.putStrLn
            OutputMiner -> s
                & payloads pdb id
                & miner
                & S.filter (\(_, Miner mid _mkeys) -> _minerId mid /= "noMiner")
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputCoinbaseOutput -> s
                & payloads pdb id
                & coinbaseOutput _2
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputCoinebaseResult -> s
                & payloads pdb id
                & coinbaseResult _2
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            CoinbaseFailure -> s
                & payloads pdb id
                & coinbaseResult _2
                & failures _2
                & S.concat
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputPayload -> s
                & payloads pdb id
                & transactionsWithOutputs _2
                & S.map (encodeJson @(BlockHeight, V.Vector Value))
                & S.mapM_ T.putStrLn
            OutputAll -> s
                & S.map (\h -> (h,h))
                & payloads pdb _2
                & S.map (\(a,(_,c)) -> (a,c))
                & transactionsWithOutputs _2
                & S.map (encodeJson @(BlockHeader, V.Vector Value))
                & S.mapM_ T.putStrLn
  where
    logg :: LogFunctionText
    logg = logFunction logger

    encodeJson :: forall a . ToJSON a => a -> T.Text
    encodeJson
        | _configPretty config = T.decodeUtf8 . BL.toStrict . encodePretty
        | otherwise = encodeToText

-- -------------------------------------------------------------------------- --
-- Tools

progress :: LogFunctionText -> S.Stream (Of BlockHeader) IO a -> S.Stream (Of BlockHeader) IO a
progress logg s = s
    & S.chain (logg Debug . sshow)

miner
    :: MonadThrow m
    => S.Stream (Of (x, PayloadWithOutputs)) m a
    -> S.Stream (Of (x, Miner)) m a
miner = S.mapM
    --  $ traverse (decodeStrictOrThrow' . _minerData . _payloadWithOutputsMiner)
    $ traverse (decodeStrictOrThrow' . _minerData . _payloadWithOutputsMiner)

payloads
    :: MonadIO m
    => PayloadCas cas
    => PayloadDb cas
    -> Lens a b BlockHeader (BlockHeight, PayloadWithOutputs)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
payloads pdb l =  S.mapM
    $ l (traverse (liftIO . casLookupM pdb) . (_blockHeight &&& _blockPayloadHash))

coinbaseOutput
    :: Monad m
    => Lens a b PayloadWithOutputs CoinbaseOutput
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
coinbaseOutput l = S.map $ over l _payloadWithOutputsCoinbase

coinbaseResult
    :: MonadThrow m
    => Lens a b PayloadWithOutputs (CommandResult T.Text)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
coinbaseResult l = S.mapM
    $ l (decodeStrictOrThrow' . _coinbaseOutput . _payloadWithOutputsCoinbase)

pactResult
    :: Monad m
    => Lens a b (CommandResult T.Text) PactResult
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
pactResult l = S.map $ over l _crResult

failures
    :: Monad m
    => Lens a b (CommandResult T.Text) (Maybe PactError)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
failures l = S.map $ over l (go . _crResult)
  where
    go :: PactResult -> Maybe PactError
    go (PactResult x) = case x of
        Left e -> Just e
        Right _ -> Nothing

transactionsWithOutputs
    :: MonadThrow m
    => Lens a b PayloadWithOutputs (V.Vector Value)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
transactionsWithOutputs l = S.mapM $ l
    $ traverse
        ( fmap commandWithOutputsValue
        . bitraverse decodeStrictOrThrow' decodeStrictOrThrow'
        . bimap _transactionBytes _transactionOutputBytes
        )
    . _payloadWithOutputsTransactions

commandWithOutputsValue :: (Command T.Text, CommandResult T.Text) -> Value
commandWithOutputsValue (c, o) = object
    [ "sigs" .= _cmdSigs c
    , "hash" .= _cmdHash c
    , "payload" .= either
        (const $ String $ _cmdPayload c)
        (id @Value)
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
    , "output" .= o
    ]

commandValue :: Command T.Text -> Value
commandValue c = object
    [ "sigs" .= _cmdSigs c
    , "hash" .= _cmdHash c
    , "payload" .= either
        (const $ String $ _cmdPayload c)
        (id @Value)
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
    ]

#if REMOTE_DB
-- -------------------------------------------------------------------------- --
-- Remote Databases
--
-- WORK IN PROGRESS

newtype RemotePayloadDb

instance IsCas RemotePayloadDb

netPayload :: Config -> Manager -> BlockPayloadHash -> IO PayloadData
netPayload config  mgr x = runClientM (payloadClient ver cid x) (env mgr node) >>= \case
    Left e -> error (show e)
    Right a -> return a
  where
    cid = _configChainId config
    ver = _configChainwebVersion config
    node = _configNode config

netPayloadWithOutput :: Config -> Manager -> BlockPayloadHash -> IO PayloadWithOutputs
netPayloadWithOutput config mgr x
    = runClientM (outputsClient ver cid x) (env mgr node) >>= \case
        Left e -> error (show e)
        Right a -> return a
  where
    cid = _configChainId config
    ver = _configChainwebVersion config
    node = _configNode config

netDb :: Config -> Manager -> LogFunction -> IO RemoteDb
netDb c mgr l = mkDb
    (_configChainwebVersion c)
    (_configChainId c)
    mgr
    l
    (_configNode c)
  where
    mkDb
        :: HasChainwebVersion v
        => HasChainId cid
        => v
        -> cid
        -> Manager
        -> LogFunction
        -> HostAddress
        -> IO RemoteDb
    mkDb v c mgr logg h = do
        return $ RemoteDb
            (env mgr h)
            (ALogFunction logg)
            (_chainwebVersion v)
            (_chainId c)

env :: Manager -> HostAddress -> ClientEnv
env mgr h = mkClientEnv mgr (hostAddressBaseUrl h)

hostAddressBaseUrl :: HostAddress -> BaseUrl
hostAddressBaseUrl h = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = show (_hostAddressHost h)
    , baseUrlPort = fromIntegral (_hostAddressPort h)
    , baseUrlPath = ""
    }
#endif
