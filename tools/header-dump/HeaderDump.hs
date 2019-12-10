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

import Configuration.Utils
import Configuration.Utils.Validation

import Control.Arrow
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Morph

import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Bitraversable
import qualified Data.ByteString.Lazy as BL
import Data.CAS
import Data.CAS.RocksDB
import qualified Data.CaseInsenstive as CI
import Data.Functor.Of
import qualified Data.HashSet as HS
import Data.LogMessage
import Data.Semigroup hiding (option)
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
    | Miner
    | CoinbaseOutput
    | CoinebaseResult
    | CoinbaseFailures
    deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance FromJSON Output where
    parseJSON = withText "Output" $ \t -> case CI.mk t of
        "header" -> Header
        "miner" -> Miner
        "coinbase-output" -> CoinbaseOutput
        "coinbase-result" -> CoinebaseResult
        "coinbase-failures" -> CoinbaseFailures
        o -> fail $ "unknown result type: " <> sshow o

instance ToJSON Output where
    toJSON Header = "header"
    toJSON Miner = "miner"
    toJSON CoinbaseOutput = "coinbase-output"
    toJSON CoinebaseResult = "coinbase-result"
    toJSON CoinBaseFailures = "coinbase-failures"

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !ChainId
    , _configDatabaseDirectory :: !(Maybe FilePath)
    , _configPretty :: !Bool
    , _configStart :: !(Maybe (Min Natural))
    , _configEnd :: !(Maybe (Max Natural))
    , _configTyp :: !Output
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
    <*< configOutput .:: jsonOption
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
    liftIO $ run2 config $ logger
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

run2 :: Logger l => Config -> l -> IO ()
run2 config logger = withChainDbs logger config $ \pdb cdb -> do
    txOutputsStream logger config pdb cdb $ \s -> s
        & coinbaseResult
        -- & failures
        -- & miner
        -- & S.filter (\(_, Miner mid mkeys) -> _minerId mid /= "noMiner")
        & S.mapM_ (T.putStrLn . T.decodeUtf8 . BL.toStrict . encodePretty)

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

        & case _configOutput config of
            Header -> S.map ObjectEncoded
            Miner -> miner
                & S.filter (\(_, Miner mid mkeys) -> _minerId mid /= "noMiner")
            CoinbaseOutput -> payloads
                & coinbaseOutput
            CoinebaseResult -> payloads
                & coinbaseResult
            CoinBaseFailures -> payloads
                & coinbaseResult
                & failures

        & S.map encodeJson
        & S.mapM_ T.putStrLn
  where
    encodeJson
        | _configPretty config = T.decodeUtf8 . BL.toStrict . encodePretty
        | otherwise = encodeToText


txOutputsStream
    :: Logger l
    => PayloadCas cas
    => l
    -> Config
    -> PayloadDb cas
    -> BlockHeaderDb
    -> (S.Stream (Of (BlockHeight, PayloadWithOutputs)) IO () -> IO ())
    -> IO ()
txOutputsStream logger config pdb cdb inner = do
    liftIO $ logg Info "start traversing block headers"
    void $ entries cdb Nothing Nothing (MinRank <$> _configStart config) (MaxRank <$> _configEnd config) $ \s -> s
        & void
        & progress logg
        & payloads pdb
        & inner
  where
    logg :: LogFunctionText
    logg = logFunction logger
    v = _configChainwebVersion config
    cid = _configChainId config

progress :: LogFunctionText -> S.Stream (Of BlockHeader) IO a -> S.Stream (Of BlockHeader) IO a
progress logg s = s
    & S.chain (logg Debug . sshow)
    & S.chain
        (\x -> when (_blockHeight x `mod` 100 == 0) $
            logg Info ("BlockHeight: " <> sshow (_blockHeight x))
        )

commands
    :: PayloadCas cas
    => PayloadDb cas
    -> S.Stream (Of BlockHeader) IO ()
    -> S.Stream (Of (BlockHeight, Command T.Text, CommandResult T.Text)) IO ()
commands pdb s = s
    & S.mapM
        ( traverse (casLookupM pdb)
        . (_blockHeight &&& _blockPayloadHash)
        )
    & flip S.for
        ( S.each
        . sequence
        . fmap _payloadWithOutputsTransactions
        )
    & S.map (\(a,(b,c)) -> (a,b,c))
    & S.map (bimap _transactionBytes _transactionOutputBytes)
    & S.mapM (bitraverse decodeStrictOrThrow' decodeStrictOrThrow')

miner
    :: MonadThrow m
    => S.Stream (Of (BlockHeight, PayloadWithOutputs)) m a
    -> S.Stream (Of (BlockHeight, Miner)) m a
miner = S.mapM
    --  $ traverse (decodeStrictOrThrow' . _minerData . _payloadWithOutputsMiner)
    $ traverse (decodeStrictOrThrow' . _minerData . _payloadWithOutputsMiner)

payloads
    :: MonadIO m
    => PayloadCas cas
    => PayloadDb cas
    -> S.Stream (Of BlockHeader) m a
    -> S.Stream (Of (BlockHeight, PayloadWithOutputs)) m a
payloads pdb =  S.mapM
    ( traverse (liftIO . casLookupM pdb)
    . (_blockHeight &&& _blockPayloadHash)
    )

coinbaseOutput
    :: Monad m
    => S.Stream (Of (BlockHeight, PayloadWithOutputs)) m a
    -> S.Stream (Of (BlockHeight, CoinbaseOutput)) m a
coinbaseOutput = S.map $ fmap _payloadWithOutputsCoinbase

coinbaseResult
    :: MonadThrow m
    => S.Stream (Of (BlockHeight, PayloadWithOutputs)) m a
    -> S.Stream (Of (BlockHeight, CommandResult T.Text)) m a
coinbaseResult = S.mapM
    $ traverse (decodeStrictOrThrow' . _coinbaseOutput . _payloadWithOutputsCoinbase)

failures
    :: Monad m
    => S.Stream (Of (BlockHeight, CommandResult T.Text)) m a
    -> S.Stream (Of (BlockHeight, PactError)) m a
failures = S.concat . S.map go . S.map (fmap _crResult)
  where
    go (a, PactResult x) = case x of
        Left e -> Just (a, e)
        Right _ -> Nothing

-- -------------------------------------------------------------------------- --
-- PayloadWithOutputs

-- runOutputs :: Config -> LogFunction -> IO ()
-- runOutputs config logg = do
--     txOutputsStream config logg
--         & S.chain (logg @T.Text Info . prettyCommandWithOutputs (_configPretty config))
--         & S.foldM_
--             (\c _ -> do
--                 let c' = succ @Int c
--                 when (c' `mod` 100 == 0) $
--                     liftIO $ logg @T.Text Info ("total tx count: " <> sshow c')
--                 return c'
--             )
--             (return 0)
--             (const $ return ())

prettyCommandWithOutputs :: Bool -> (BlockHeight, Command T.Text, CommandResult T.Text) -> T.Text
prettyCommandWithOutputs p (bh, c, o) = T.decodeUtf8
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
        , "output" .= o
        ]

