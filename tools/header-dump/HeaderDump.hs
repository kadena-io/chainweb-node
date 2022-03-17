{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module HeaderDump
( main
, run
, mainWithConfig

-- * Configuration
, Output(..)
, Config(..)
, defaultConfig

-- * ChainData
, ChainData(..)
, cdChainId
, cdHeight
, cdData

-- * Tools
, progress
, miner
, coinbaseOutput
, coinbaseResult
, pactResult
, failures
, transactionsWithOutputs
, commandValue
, commandWithOutputsValue
, withChainDbs
) where

import Configuration.Utils hiding (Lens)
import Configuration.Utils.Validation

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except

import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Aeson.Lens
import Data.Bitraversable
import qualified Data.ByteString.Lazy as BL
import Data.CAS
import Data.CAS.RocksDB
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.Functor.Of
import qualified Data.HashSet as HS
import Data.LogMessage
import Data.Maybe
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

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeight
import Chainweb.BlockHeaderDB
import Chainweb.ChainValue
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Time
import Chainweb.TreeDB hiding (key)
import Chainweb.Utils hiding (progress)
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
    | OutputNone
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
        "none" -> return OutputNone
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
    toText OutputNone = "none"

instance FromJSON Output where
    parseJSON = parseJsonFromText "Output"

instance ToJSON Output where
    toJSON = toJSON . toText

enumMetavar
    :: forall a
    . Enum a
    => Bounded a
    => HasTextRepresentation a
    => String
enumMetavar = T.unpack
    $ T.intercalate "|" $ toText @a <$> [minBound .. maxBound]

-- -------------------------------------------------------------------------- --
-- Configuration

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !(Maybe ChainId)
    , _configDatabasePath :: !(Maybe FilePath)
    , _configPretty :: !Bool
    , _configStart :: !(Maybe (Min Natural))
    , _configEnd :: !(Maybe (Max Natural))
    , _configOutput :: !Output
    , _configValidate :: !Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
    { _configLogHandle = Y.StdOut
    , _configLogLevel = Y.Info
    , _configChainwebVersion = Development
    , _configChainId = Nothing
    , _configPretty = True
    , _configDatabasePath = Nothing
    , _configStart = Nothing
    , _configEnd = Nothing
    , _configOutput = Header
    , _configValidate = False
    }

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
        , "validate" .= _configValidate o
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
        <*< configValidate ..: "validate" % o

pConfig :: MParser Config
pConfig = id
    <$< configLogHandle .:: Y.pLoggerHandleConfig
    <*< configLogLevel .:: Y.pLogLevel
    <*< configChainwebVersion .:: option textReader
        % long "chainweb-version"
        <> help "chainweb version identifier"
    <*< configChainId .:: fmap Just % option textReader
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
        <> metavar (enumMetavar @Output)
        <> help "which component of the payload to output"
    <*< configValidate .:: boolOption_
        % long "validate"
        <> help "Validate BlockHeaders in the Database"

validateConfig :: ConfigValidation Config []
validateConfig o = do
    checkIfValidChain (_configChainId o)
    mapM_ (validateDirectory "database") (_configDatabasePath o)
    when (_configValidate o && isJust (_configChainId o))
        $ throwError $ "validation (--validate) can only be used if no particular chain is selected"
  where
    chains = chainIds $ _configChainwebVersion o

    checkIfValidChain Nothing = return ()
    checkIfValidChain (Just cid) = unless (HS.member cid chains)
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

    opts = modernDefaultOptions { R.createIfMissing = False }

    initializeRocksDb_ :: RocksDb -> IO ()
    initializeRocksDb_ db = R.put
        (_rocksDbHandle db)
        R.defaultWriteOptions
        (_rocksDbNamespace db <> ".")
        ""

    closeRocksDb_ :: RocksDb -> IO ()
    closeRocksDb_ = R.close . _rocksDbHandle

-- -------------------------------------------------------------------------- --
-- Chain Data Wrapper

data ChainData a = ChainData
    { _cdChainId :: !ChainId
    , _cdHeight :: !BlockHeight
    , _cdData :: !a
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''ChainData

instance ToJSON a => ToJSON (ChainData a) where
    toJSON o = object
        [ "chainId" .= _cdChainId o
        , "height" .= _cdHeight o
        , "data" .= _cdData o
        ]

-- -------------------------------------------------------------------------- --
-- Print Transactions

mainWithConfig :: Config -> IO ()
mainWithConfig config = withLog $ \logger ->
    liftIO $ run config $ logger
        & addLabel ("version", toText $ _configChainwebVersion config)
        -- & addLabel ("chain", toText $ _configChainId config)
  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ _configLogLevel config
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

withBlockHeaders
    :: Logger l
    => l
    -> Config
    -> (forall cas . PayloadCasLookup cas => PayloadDb cas -> S.Stream (Of BlockHeader) IO () -> IO a)
    -> IO a
withBlockHeaders logger config inner = do
    rocksDbDir <- getRocksDbDir
    logg Info $ "using database at: " <> T.pack rocksDbDir
    withRocksDb_ rocksDbDir $ \rdb -> do
        let pdb = newPayloadDb rdb
        initializePayloadDb v pdb
        liftIO $ logg Info "start traversing block headers"
        liftIO $ logg Info $ "header validation: " <> sshow (_configValidate config)
        withChainDbs rdb v cids (_configValidate config) start end $ inner pdb . void
  where
    logg :: LogFunctionText
    logg = logFunction logger

    start = MinRank <$> _configStart config
    end = MaxRank <$> _configEnd config

    v = _configChainwebVersion config
    cids = maybe (HS.toList $ chainIds v) pure $ _configChainId config

    getRocksDbDir = case _configDatabasePath config of
        Nothing -> getXdgDirectory XdgData
            $ "chainweb-node/" <> sshow v <> "/" <> "0" <> "/rocksDb"
        Just d -> return d

run :: Logger l => Config -> l -> IO ()
run config logger = withBlockHeaders logger config $ \pdb x -> x
    & progress logg
    & \s -> case _configOutput config of
        OutputNone -> s
            & S.effects
        Header -> s
            & S.map ObjectEncoded
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputRawPayload -> s
            & payloadsCid pdb id
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputTransaction -> s
            & payloadsCid pdb id
            & transactionsWithOutputs cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputMiner -> s
            & payloadsCid pdb id
            & miner cdData
            & S.filter ((/= "noMiner") . view (cdData . minerId))
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputCoinbaseOutput -> s
            & payloadsCid pdb id
            & coinbaseOutput cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputCoinebaseResult -> s
            & payloadsCid pdb id
            & coinbaseResult cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        CoinbaseFailure -> s
            & payloadsCid pdb id
            & coinbaseResult cdData
            & failures cdData
            & S.filter (isJust . view cdData)
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputPayload -> s
            & payloadsCid pdb id
            & transactionsWithOutputs cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputAll -> s
            & S.map (\h -> (h,h))
            & payloadsCid pdb _2
            & S.map (\(a,b) -> b & cdData .~ object
                    [ "header" .= a
                    , "payload" .= view cdData b
                    ]
                )
            & transactionsWithOutputs (cdData . key "payload" . _JSON)
            & S.map encodeJson
            & S.mapM_ T.putStrLn
  where
    logg :: LogFunctionText
    logg = logFunction logger

    encodeJson :: forall a . ToJSON a => a -> T.Text
    encodeJson
        | _configPretty config = T.decodeUtf8 . BL.toStrict . encodePretty
        | otherwise = encodeToText

-- | TODO include braiding validation
validate :: S.Stream (Of BlockHeader) IO () -> S.Stream (Of BlockHeader) IO ()
validate s = do
    now <- liftIO getCurrentTimeIntegral
    s
        & S.copy
        & S.foldM_ (step now) (return initial) (\_ -> return ())
  where
    -- state: (height, parents, currents, initial)
    initial :: (BlockHeight, [BlockHeader], [BlockHeader], Bool)
    initial = (0, [], [], True)

    step
        :: MonadIO m
        => Time Micros
        -> (BlockHeight, [BlockHeader], [BlockHeader], Bool)
        -> BlockHeader
        -> m (BlockHeight, [BlockHeader], [BlockHeader], Bool)
    step now state c = liftIO $ do
        let state' = update state c
        val now state' c
        return state'

    update
        :: (BlockHeight, [BlockHeader], [BlockHeader], Bool)
        -> BlockHeader
        -> (BlockHeight, [BlockHeader], [BlockHeader], Bool)
    update (h, parents, currents, i) c
        -- initially set the block height to the current header
        | i = (_blockHeight c, parents, c : currents, i)
        | _blockHeight c == h = (h, parents, c : currents, i)
        | _blockHeight c == (h + 1) = (h + 1, currents, [c], False)
        | _blockHeight c < h = error "height invariant violation in enumeration of headers. Height of current header smaller than previous headers"
        | otherwise = error
            $ "height invariant violation in enumeration of headers."
            <> " Height of current header skips block height."
            <> "\ncurrent block: " <> sshow c
            <> "\ninitial: " <> sshow i
            <> "\nheight: " <> sshow h
            <> "\nparents: " <> sshow parents

    lookupHdr
        :: Applicative m
        => [BlockHeader]
        -> ChainValue BlockHash
        -> m (Maybe BlockHeader)
    lookupHdr hdrs h = pure $ find ((== _chainValueValue h) . _blockHash) hdrs

    val
        :: Time Micros
        -> (BlockHeight, [BlockHeader], [BlockHeader], Bool)
        -> BlockHeader
        -> IO ()
    val now (_, parents, _, isInitial) c
        | isGenesisBlockHeader c = void $ validateBlockHeaderM now (lookupHdr parents) c
        | isInitial = validateIntrinsicM now c
        | otherwise = void $ validateBlockHeaderM now (lookupHdr parents) c

-- -------------------------------------------------------------------------- --
-- Tools

progress :: LogFunctionText -> S.Stream (Of BlockHeader) IO a -> S.Stream (Of BlockHeader) IO a
progress logg s = s
    & S.chain (logg Debug . sshow)
    & S.chain
        (\x -> when (_blockHeight x `mod` 100 == 0) $
            logg Info ("BlockHeight: " <> sshow (_blockHeight x))
        )
miner
    :: MonadThrow m
    => Traversal a b PayloadWithOutputs Miner
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
miner l = S.mapM
    $ l (decodeStrictOrThrow' . _minerData . _payloadWithOutputsMiner)

payloadsCid
    :: MonadIO m
    => PayloadCasLookup cas
    => PayloadDb cas
    -> Traversal a b BlockHeader (ChainData PayloadWithOutputs)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
payloadsCid pdb l =  S.mapM
    $ l
        ( cdData (liftIO . casLookupM pdb)
        . (\x -> ChainData
            { _cdChainId = _blockChainId x
            , _cdHeight = _blockHeight x
            , _cdData = _blockPayloadHash x
            }
          )
        )

coinbaseOutput
    :: Monad m
    => Traversal a b PayloadWithOutputs CoinbaseOutput
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
coinbaseOutput l = S.map $ over l _payloadWithOutputsCoinbase

coinbaseResult
    :: MonadThrow m
    => Traversal a b PayloadWithOutputs (CommandResult T.Text)
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
coinbaseResult l = S.mapM
    $ l (decodeStrictOrThrow' . _coinbaseOutput . _payloadWithOutputsCoinbase)

pactResult
    :: Monad m
    => Traversal a b (CommandResult T.Text) PactResult
    -> S.Stream (Of a) m r
    -> S.Stream (Of b) m r
pactResult l = S.map $ over l _crResult

failures
    :: Monad m
    => Traversal a b (CommandResult T.Text) (Maybe PactError)
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
    => Traversal a b PayloadWithOutputs (V.Vector Value)
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
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload c)
    , "output" .= o
    ]

commandValue :: Command T.Text -> Value
commandValue c = object
    [ "sigs" .= _cmdSigs c
    , "hash" .= _cmdHash c
    , "payload" .= either
        (const $ String $ _cmdPayload c)
        (id @Value)
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload c)
    ]

-- -------------------------------------------------------------------------- --
-- Streaming Tools

withChainDbs
    :: RocksDb
    -> ChainwebVersion
    -> [ChainId]
    -> Bool
        -- ^ whether to validate
    -> Maybe MinRank
    -> Maybe MaxRank
    -> (S.Stream (Of BlockHeader) IO () -> IO a)
    -> IO a
withChainDbs rdb v cids doValidation start end f = go cids mempty
  where
    go [] !s = f s
    go (cid:t) !s = withBlockHeaderDb rdb v cid $ \cdb ->
        entries cdb Nothing Nothing start end $ \x ->
            go t (() <$ S.mergeOn _blockHeight s (val $ () <$ x))

    val = if doValidation then validate else id

#if REMOTE_DB
-- -------------------------------------------------------------------------- --
-- Remote Databases
--
-- WORK IN PROGRESS

newtype RemotePayloadDb

instance HasCasLookup RemotePayloadDb
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
