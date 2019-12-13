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

import GHC.Generics

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
-- Configuration

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

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !ChainId
    , _configDatabaseDirectory :: !(Maybe FilePath)
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
    , _configDatabaseDirectory = Nothing
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
        , "databaseDirectory" .= _configDatabaseDirectory o
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
        <*< configDatabaseDirectory ..: "databaseDirectory" % o
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
    <*< configOutput .:: textOption
        % long "output"
        <> short 'o'
        <> help "output type"

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

main2 :: IO ()
main2 = runWithConfiguration pinfo mainWithConfig
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
            logg Info "start dumping block headers"
            T.putStr "[\n"
            void $ entries cdb Nothing Nothing (MinRank <$> _configStart config) (MaxRank <$> _configEnd config) $ \s -> s
                & S.map (encodeJson . ObjectEncoded)
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

-- -------------------------------------------------------------------------- --
-- Print Transactions

mainWithConfig2 :: Config -> IO ()
mainWithConfig2 config = withLog $ \logger -> do
    liftIO $ run3 config $ logger
        & addLabel ("version", toText $ _configChainwebVersion config)
        & addLabel ("chain", toText $ _configChainId config)
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ (_configLogLevel config)
        & Y.logConfigBackend . Y.handleBackendConfigHandle .~ _configLogHandle config
    withLog inner = Y.withHandleBackend_ logText (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

main :: IO ()
main = runWithConfiguration pinfo mainWithConfig2
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

    getRocksDbDir = case _configDatabaseDirectory config of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> "0" <> "/rocksDb"
        Just d -> return d

run3 :: Logger l => Config -> l -> IO ()
run3 config logger = withChainDbs logger config $ \pdb cdb -> do
    liftIO $ logg Info "start traversing block headers"
    void $ entries cdb Nothing Nothing (MinRank <$> _configStart config) (MaxRank <$> _configEnd config) $ \s -> s
        & void
        & progress logg

        & \s -> case _configOutput config of
            Header -> s
                & S.map ObjectEncoded
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputRawPayload -> s
                & payloads pdb _1
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputTransaction -> s
                & payloads pdb _1
                & transactionsWithOutputs
                & S.map (encodeJson @(BlockHeight, V.Vector Value))
                & S.mapM_ T.putStrLn
            OutputMiner -> s
                & payloads pdb _1
                & miner
                & S.filter (\(_, Miner mid mkeys) -> _minerId mid /= "noMiner")
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputCoinbaseOutput -> s
                & payloads1 pdb
                & coinbaseOutput _2
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            OutputCoinebaseResult -> s
                & payloads1 pdb
                & coinbaseResult _2
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            CoinbaseFailure -> s
                & payloads1 pdb
                & coinbaseResult _2
                & failures
                & S.map encodeJson
                & S.mapM_ T.putStrLn
            -- OutputPayload -> s
            --     & S.map (\h -> (h,h))
            --     & payloads pdb
            --     & moveRight
            --     & transactionsWithOutputs
            --     & moveRight
            --     & S.map (encodeJson @((BlockHeader, BlockHeight), V.Vector Value))
            --     & S.mapM_ T.putStrLn
            OutputAll -> s
                & S.map (\h -> (h,h))
                & payloads pdb _2
                & S.map (\(h,(a,b)) -> ((h,a),b))
                & transactionsWithOutputs
                & S.map (encodeJson @((BlockHeader, BlockHeight), V.Vector Value))
                & S.mapM_ T.putStrLn

  where
    logg :: LogFunctionText
    logg = logFunction logger

    encodeJson :: forall a . ToJSON a => a -> T.Text
    encodeJson
        | _configPretty config = T.decodeUtf8 . BL.toStrict . encodePretty
        | otherwise = encodeToText

moveRight :: Monad m => S.Stream (Of (a, (b,c))) m r -> S.Stream (Of ((a,b),c)) m r
moveRight = S.map (\(a,(b,c)) -> ((a,b), c))

progress :: LogFunctionText -> S.Stream (Of BlockHeader) IO a -> S.Stream (Of BlockHeader) IO a
progress logg s = s
    & S.chain (logg Debug . sshow)
    & S.chain
        (\x -> when (_blockHeight x `mod` 100 == 0) $
            logg Info ("BlockHeight: " <> sshow (_blockHeight x))
        )

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

failures
    :: Monad m
    => S.Stream (Of (x, CommandResult T.Text)) m a
    -> S.Stream (Of (x, PactError)) m a
failures = S.concat . S.map go . S.map (fmap _crResult)
  where
    go (a, PactResult x) = case x of
        Left e -> Just (a, e)
        Right _ -> Nothing

transactionsWithOutputs
    :: MonadThrow m
    => S.Stream (Of (x, PayloadWithOutputs)) m a
    -> S.Stream (Of (x, V.Vector Value)) m a
transactionsWithOutputs s = s
    & S.map (fmap _payloadWithOutputsTransactions)
    & S.map (\(a, b) -> (a, bimap _transactionBytes _transactionOutputBytes <$> b))
    & S.mapM
        (\(a, b) -> (a,)
            <$> traverse (bitraverse decodeStrictOrThrow' decodeStrictOrThrow') b
        )
    & S.map (\(a, b) -> (a, prettyCommandWithOutputs True <$> b))

prettyCommandWithOutputs :: Bool -> (Command T.Text, CommandResult T.Text) -> Value
prettyCommandWithOutputs p (c, o) = object
    [ "sigs" .= _cmdSigs c
    , "hash" .= _cmdHash c
    , "payload" .= either
        (const $ String $ _cmdPayload c)
        (id @Value)
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
    , "output" .= o
    ]

prettyCommand :: Bool -> (BlockHeight, Command T.Text) -> T.Text
prettyCommand p (bh, c) = T.decodeUtf8
    $ BL.toStrict
    $ (if p then encodePretty else encode)
    $ object
        [ "height" .= bh
        , "sigs" .= _cmdSigs c
        , "hash" .= _cmdHash c
        , "payload" .= either
            (const $ String $ _cmdPayload c)
            (id @Value)
            (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
        ]

