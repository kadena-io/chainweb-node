{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
module Main (main) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeader.Validation
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.ChainValue
import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Payload.PayloadStore.RocksDB
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.TreeDB hiding (key)
import Chainweb.Utils hiding (progress)
import Chainweb.Version
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry
import Configuration.Utils hiding (Lens)
import Configuration.Utils.Validation
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Aeson.Lens
import Data.Bitraversable
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Foldable
import Data.Functor.Of
import Data.HashSet qualified as HS
import Data.LogMessage
import Data.Maybe
import Data.Semigroup
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Database.RocksDB.Base qualified as R
import GHC.Generics hiding (to)
import Numeric.Natural
import Pact.JSON.Encode qualified as J
import Pact.Types.Command
import Pact.Types.PactError
import Streaming.Prelude qualified as S
import System.Directory
import System.LogLevel
import System.Logger qualified as Y

-- -------------------------------------------------------------------------- --

#define REMOTE_DB 0

#if REMOTE_DB
import Chainweb.BlockHeaderDB.RemoteDB
import Chainweb.HostAddress
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
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
    , _configChainwebVersion = RecapDevelopment
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
        , "chainwebVersion" .= _versionName (_configChainwebVersion o)
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
        <*< setProperty configChainwebVersion "chainwebVersion"
            (findKnownVersion <=< parseJSON) o
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
    <*< configChainwebVersion .:: option (findKnownVersion =<< textReader)
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

makeLensesFor [("_cdData", "cdData")] ''ChainData

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
        & addLabel ("version", getChainwebVersionName $ _versionName $ _configChainwebVersion config)
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
    -> (forall tbl . CanReadablePayloadCas tbl => PayloadDb tbl -> S.Stream (Of BlockHeader) IO () -> IO a)
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
            & S.mapM (payloadsCid pdb)
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputTransaction -> s
            & S.mapM (payloadsCid pdb)
            & transactionsWithOutputs cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputMiner -> s
            & S.mapM (payloadsCid pdb)
            & miner cdData
            & S.filter ((/= "noMiner") . view (cdData . minerId))
            & S.map (encodeJson . fmap J.encodeText)
            & S.mapM_ T.putStrLn
        OutputCoinbaseOutput -> s
            & S.mapM (payloadsCid pdb)
            & coinbaseOutput cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputCoinebaseResult -> s
            & S.mapM (payloadsCid pdb)
            & coinbaseResult cdData
            & S.map (fmap J.toJsonViaEncode)
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        CoinbaseFailure -> s
            & S.mapM (payloadsCid pdb)
            & coinbaseResult cdData
            & failures cdData
            & S.filter (isJust . view cdData)
            & S.map (fmap J.toJsonViaEncode)
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputPayload -> s
            & S.mapM (payloadsCid pdb)
            & transactionsWithOutputs cdData
            & S.map encodeJson
            & S.mapM_ T.putStrLn
        OutputAll -> s
            & S.map (\h -> (h,h))
            & S.mapM (_2 $ payloadsCid pdb)
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
        | i = (view blockHeight c, parents, c : currents, i)
        | view blockHeight c == h = (h, parents, c : currents, i)
        | view blockHeight c == (h + 1) = (h + 1, currents, [c], False)
        | view blockHeight c < h = error "height invariant violation in enumeration of headers. Height of current header smaller than previous headers"
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
    lookupHdr hdrs h = pure $ find ((== _chainValueValue h) . view blockHash) hdrs

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
        (\x -> when (view blockHeight x `mod` 100 == 0) $
            logg Info ("BlockHeight: " <> sshow (view blockHeight x))
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
    => CanReadablePayloadCas tbl
    => PayloadDb tbl
    -> BlockHeader
    -> m (ChainData PayloadWithOutputs)
payloadsCid pdb bh = do
    payload <- liftIO $ lookupPayloadWithHeight pdb (Just $ view blockHeight bh) (view blockPayloadHash bh) >>= \case
        Nothing -> throwM $ userError "payload not found"
        Just p -> return p
    pure $ ChainData
        { _cdChainId = view blockChainId bh
        , _cdHeight = view blockHeight bh
        , _cdData = payload
        }

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
    [ "sigs" .= fmap J.toJsonViaEncode (_cmdSigs c)
    , "hash" .= J.toJsonViaEncode (_cmdHash c)
    , "payload" .= either
        (const $ String $ _cmdPayload c)
        (id @Value)
        (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload c)
    , "output" .= J.toJsonViaEncode o
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
            go t (() <$ S.mergeOn (view blockHeight) s (val $ () <$ x))

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
