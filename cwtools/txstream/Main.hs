{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: TxStream
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Print all transactions in a chain starting with the most recent block
--
module Main (main) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RemoteDB
import Chainweb.BlockHeight
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI.Client
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Payload
import Chainweb.Payload.RestAPI.Client
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Registry
import Chainweb.Version.Utils
import Configuration.Utils
import Control.Lens hiding ((.=))
import Control.Monad ((<=<), when)
import Control.Monad.Reader
import Data.Aeson.Encode.Pretty hiding (Config)
import Data.Bitraversable
import Data.Functor.Of
import Data.LogMessage
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Pact.Types.Command
import Servant.Client
import System.LogLevel
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Pact.JSON.Encode qualified as J
import Streaming.Prelude qualified as S
import System.Logger qualified as Y

-- -------------------------------------------------------------------------- --
-- Configuration

data Config = Config
    { _configLogHandle :: !Y.LoggerHandleConfig
    , _configLogLevel :: !Y.LogLevel
    , _configChainwebVersion :: !ChainwebVersion
    , _configChainId :: !ChainId
    , _configNode :: !HostAddress
    , _configPretty :: !Bool
    , _configOutputs :: !Bool
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
    { _configLogHandle = Y.StdOut
    , _configLogLevel = Y.Info
    , _configChainwebVersion = RecapDevelopment
    , _configChainId = someChainId RecapDevelopment
    , _configNode = HostAddress (unsafeHostnameFromText "us1.tn1.chainweb.com") 443
    , _configPretty = True
    , _configOutputs = True
    }

instance ToJSON Config where
    toJSON o = object
        [ "logHandle" .= _configLogHandle o
        , "logLevel" .= _configLogLevel o
        , "chainwebVersion" .= _versionName (_configChainwebVersion o)
        , "chainId" .= _configChainId o
        , "node" .= _configNode o
        , "pretty" .= _configPretty o
        , "outputs" .= _configOutputs o
        ]

instance FromJSON (Config -> Config) where
    parseJSON = withObject "Config" $ \o -> do
        id
            <$< configLogHandle ..: "logHandle" % o
            <*< configLogLevel ..: "logLevel" % o
            <*< setProperty configChainwebVersion "chainwebVersion"
                (findKnownVersion <=< parseJSON) o
            <*< configChainId ..: "chainId" % o
            <*< configNode ..: "node" % o
            <*< configPretty ..: "pretty" % o
            <*< configOutputs ..: "outputs" % o

pConfig :: MParser Config
pConfig = id
    <$< configLogHandle .:: Y.pLoggerHandleConfig
    <*< configLogLevel .:: Y.pLogLevel
    <*< configChainwebVersion .:: option (findKnownVersion =<< textReader)
        % long "chainweb-version"
        <> help "chainweb version identifier"
    <*< configChainId .:: option textReader
        % long "chain-id"
        <> short 'c'
        <> help "chain id to query"
    <*< configNode %:: pHostAddress Nothing
    <*< configPretty .:: boolOption_
        % long "pretty"
        <> short 'p'
        <> help "print prettyfied JSON. Uses multiple lines for one transaction"
    <*< configOutputs .:: boolOption_
        % long "outputs"
        <> short 'o'
        <> help "include transaction outputs"

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
-- BlockHeaderDb

devNetDb :: Config -> Manager -> LogFunction -> IO RemoteDb
devNetDb c mgr l = mkDb
    (_configChainwebVersion c)
    (_configChainId c)
    mgr
    l
    (_configNode c)

-- TreeDB

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

-- -------------------------------------------------------------------------- --
-- Payload Data

run :: Config -> LogFunction -> IO ()
run config logg = do
    mgr <- newTlsManager
    txStream config mgr logg
        & S.chain (logg @T.Text Info . prettyCommand (_configPretty config))
        & S.copy
        & S.foldM_
            (\c _ -> do
                let c' = succ @Int c
                when (c' `mod` 100 == 0) $
                    liftIO $ logg @T.Text Info ("total tx count: " <> sshow c')
                return c'
            )
            (return 0)
            (const $ return @_ @Int 0)
        & S.foldM_
            (\m (h, tx) -> case HM.lookup (_cmdHash tx) m of
                Just x -> do
                    logg @T.Text Warn $ "duplicate at " <> sshow h <> ". Previously at " <> sshow x
                    return $! HM.insertWith (<>) (_cmdHash tx) [h] m
                Nothing -> do
                    return $! HM.insertWith (<>) (_cmdHash tx) [h] m
            )
            (return mempty)
            (const $ return ())

prettyCommand :: Bool -> (BlockHeight, Command T.Text) -> T.Text
prettyCommand p (bh, c) = T.decodeUtf8
    $ BL.toStrict
    $ (if p then encodePretty else encode)
    $ object
        [ "height" .= bh
        , "sigs" .= fmap J.toJsonViaEncode (_cmdSigs c)
        , "hash" .= J.toJsonViaEncode (_cmdHash c)
        , "payload" .= either
            (const $ String $ _cmdPayload c)
            (id @Value)
            (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
        ]

txStream
    :: Config
    -> Manager
    -> LogFunction
    -> S.Stream (Of (BlockHeight, Command T.Text)) IO ()
txStream config mgr logg = do
        hdb <- liftIO $ devNetDb config mgr logg

        c <- liftIO $ devNetCut config mgr
        let h = _bhwhHash $ _cutHashes c ^?! ix (_configChainId config)

        getBranch hdb mempty (HS.singleton (UpperBound h))
            & S.chain (logg @T.Text Debug . sshow)
            & S.chain
                (\x -> when (view blockHeight x `mod` 100 == 0) $
                    logg @T.Text Info ("BlockHeight: " <> sshow (view blockHeight x))
                )
            & S.mapM (\x -> (view blockHeight x,) <$> devNetPayload config mgr (view blockHeight x) (view blockPayloadHash x))
            & flip S.for (S.each . traverse (view payloadDataTransactions))
            & S.map (fmap _transactionBytes)
            & S.mapM (traverse decodeStrictOrThrow')

-- -------------------------------------------------------------------------- --
-- PayloadWithOutputs

runOutputs :: Config -> LogFunction -> IO ()
runOutputs config logg = do
    mgr <- newTlsManager
    txOutputsStream config mgr logg
        & S.chain (logg @T.Text Info . prettyCommandWithOutputs (_configPretty config))
        & S.foldM_
            (\c _ -> do
                let c' = succ @Int c
                when (c' `mod` 100 == 0) $
                    liftIO $ logg @T.Text Info ("total tx count: " <> sshow c')
                return c'
            )
            (return 0)
            (const $ return ())

prettyCommandWithOutputs :: Bool -> (BlockHeight, Command T.Text, CommandResult T.Text) -> T.Text
prettyCommandWithOutputs p (bh, c, o) = T.decodeUtf8
    $ BL.toStrict
    $ (if p then encodePretty else encode)
    $ object
        [ "height" .= bh
        , "sigs" .= fmap J.toJsonViaEncode (_cmdSigs c)
        , "hash" .= J.toJsonViaEncode (_cmdHash c)
        , "payload" .= either
            (const $ String $ _cmdPayload c)
            (id @Value)
            (eitherDecodeStrict' $ T.encodeUtf8 $ _cmdPayload $ c)
        , "output" .= J.toJsonViaEncode o
        ]

txOutputsStream
    :: Config
    -> Manager
    -> LogFunction
    -> S.Stream (Of (BlockHeight, Command T.Text, CommandResult T.Text)) IO ()
txOutputsStream config mgr logg = do
        hdb <- liftIO $ devNetDb config mgr logg

        cut <- liftIO $ devNetCut config mgr
        let h = _bhwhHash $ _cutHashes cut ^?! ix (_configChainId config)

        getBranch hdb mempty (HS.singleton (UpperBound h))
            & S.chain (logg @T.Text Debug . sshow)
            & S.chain
                (\x -> when (view blockHeight x `mod` 100 == 0) $
                    logg @T.Text Info ("BlockHeight: " <> sshow (view blockHeight x))
                )

            & S.mapM (\x -> (view blockHeight x,) <$> devNetPayloadWithOutput config mgr (view blockHeight x) (view blockPayloadHash x))
            & flip S.for
                ( S.each
                . traverse _payloadWithOutputsTransactions
                )
            & S.map (\(a,(b,c)) -> (a,b,c))
            & S.map (bimap _transactionBytes _transactionOutputBytes)
            & S.mapM (bitraverse decodeStrictOrThrow' decodeStrictOrThrow')

-- -------------------------------------------------------------------------- --
-- Cut

devNetCut :: Config -> Manager -> IO CutHashes
devNetCut config mgr = runClientM (cutGetClient ver) (env mgr node) >>= \case
    Left e -> error (show e)
    Right x -> return x
  where
    ver = _configChainwebVersion config
    node = _configNode config

-- -------------------------------------------------------------------------- --
-- Payloads

devNetPayload :: Config -> Manager -> BlockHeight -> BlockPayloadHash -> IO PayloadData
devNetPayload config mgr h x = runClientM (payloadClient ver cid x (Just h)) (env mgr node) >>= \case
    Left e -> error (show e)
    Right a -> return a
  where
    cid = _configChainId config
    ver = _configChainwebVersion config
    node = _configNode config

devNetPayloadWithOutput :: Config -> Manager -> BlockHeight -> BlockPayloadHash -> IO PayloadWithOutputs
devNetPayloadWithOutput config mgr h x
    = runClientM (outputsClient ver cid x (Just h)) (env mgr node) >>= \case
        Left e -> error (show e)
        Right a -> return a
  where
    cid = _configChainId config
    ver = _configChainwebVersion config
    node = _configNode config

-- -------------------------------------------------------------------------- --
-- Main

mainWithConfig :: Config -> IO ()
mainWithConfig config = withLog $ \logger -> do
    let logg :: LogFunction
        logg = logFunction $ logger
            & addLabel ("host", toText $ _configNode config)
            & addLabel ("version", toText $ _versionName $ _configChainwebVersion config)
            & addLabel ("chain", toText $ _configChainId config)
    liftIO $ do
        registerVersion (_configChainwebVersion config)
        if _configOutputs config
        then runOutputs config logg
        else run config logg

  where
    logconfig = Y.defaultLogConfig
        & Y.logConfigLogger . Y.loggerConfigThreshold .~ (_configLogLevel config)
        & Y.logConfigBackend . Y.handleBackendConfigHandle .~ _configLogHandle config
    withLog inner = Y.withHandleBackend_ logText (logconfig ^. Y.logConfigBackend)
        $ \backend -> Y.withLogger (logconfig ^. Y.logConfigLogger) backend inner

main :: IO ()
main = runWithConfiguration pinfo mainWithConfig
  where
    pinfo = programInfo
        "List all transactions in chain starting with most recent block"
        pConfig
        defaultConfig
