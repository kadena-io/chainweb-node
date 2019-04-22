{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Chainweb.Utils.Logging.Config
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Utils.Logging.Config
(
-- * Log Format
  LogFormat(..)
, logFormatToText
, logFormatFromText
, pLogFormat
, pLogFormat_

-- * Logging Backend Handle
, HandleConfig(..)
, pHandleConfig
, pHandleConfig_
, validateHandleConfig

-- * Logger Backend
, BackendConfig(..)
, backendConfigColor
, backendConfigHandle
, defaultBackendConfig
, validateBackendConfig
, pBackendConfig
, pBackendConfig_

-- * Logging Config
, ClusterId
, LogConfig(..)
, logConfigLogger
, logConfigBackend
, logConfigTelemetryBackend
, logConfigClusterId
, defaultLogConfig
, validateLogConfig
, pLogConfig
, pLogConfig_
) where

import Configuration.Utils
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Lens.TH
import Control.Monad.Catch
import Control.Monad.Writer

import qualified Data.CaseInsensitive as CI
import Data.String
import qualified Data.Text as T

import GHC.Generics

import System.Logger.Backend.ColorOption
import System.Logger.Logger

-- internal modules

import Chainweb.HostAddress
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- Log Format

data LogFormat
    = LogFormatText
    | LogFormatJson
    deriving (Show, Eq, Ord, Bounded, Enum, Generic, NFData)

logFormatToText :: IsString p => LogFormat -> p
logFormatToText LogFormatText = "text"
logFormatToText LogFormatJson = "json"

logFormatFromText :: MonadThrow m => T.Text -> m LogFormat
logFormatFromText t = case CI.mk t of
    "text" -> return LogFormatText
    "json" -> return LogFormatJson
    _ -> throwM $ TextFormatException
        $ "Unknown log format: \"" <> t <> "\". Expected \"text\" or \"json\"."

instance HasTextRepresentation LogFormat where
    toText = logFormatToText
    fromText = logFormatFromText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

instance ToJSON LogFormat where
    toJSON = toJSON @T.Text . logFormatToText
    {-# INLINE toJSON #-}

instance FromJSON LogFormat where
    parseJSON = parseJsonFromText "LogFormat"
    {-# INLINE parseJSON #-}

pLogFormat :: OptionParser LogFormat
pLogFormat = pLogFormat_ ""

pLogFormat_
    :: T.Text
        -- ^ prefix for the command line options.
    -> OptionParser LogFormat
pLogFormat_ prefix = option textReader
    % long (T.unpack prefix <> "log-format")
    <> metavar "text|json"
    <> help "format that is use for writing logs to file handles"

-- -------------------------------------------------------------------------- --
-- Handle Configuration

data HandleConfig
    = StdOut
    | StdErr
    | FileHandle FilePath
    | ElasticSearch HostAddress
    deriving (Show, Eq, Ord, Generic)

instance NFData HandleConfig

handleConfigFromText :: MonadThrow m => T.Text -> m HandleConfig
handleConfigFromText x = case CI.mk x of
    "stdout" -> return StdOut
    "stderr" -> return StdErr
    _ | CI.mk (T.take 5 x) == "file:" -> return $ FileHandle (T.unpack (T.drop 5 x))
    _ | CI.mk (T.take 3 x) == "es:" -> ElasticSearch <$> fromText (T.drop 3 x)

    e -> throwM $ DecodeException $ "unexpected logger handle value: "
        <> fromString (show e)
        <> ", expected \"stdout\", \"stderr\", \"file:<FILENAME>\", or \"es:<HOST>:<PORT>\""

handleConfigToText :: HandleConfig -> T.Text
handleConfigToText StdOut = "stdout"
handleConfigToText StdErr = "stderr"
handleConfigToText (FileHandle f) = "file:" <> T.pack f
handleConfigToText (ElasticSearch f) = "es:" <> toText f

instance HasTextRepresentation HandleConfig where
    toText = handleConfigToText
    fromText = handleConfigFromText

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

validateHandleConfig :: ConfigValidation HandleConfig l
validateHandleConfig (FileHandle filepath) = validateFileWritable "file handle" filepath
validateHandleConfig _ = return ()

instance ToJSON HandleConfig where
    toJSON = String . handleConfigToText

instance FromJSON HandleConfig where
    parseJSON = parseJsonFromText "HandleConfig"

pHandleConfig :: OptionParser HandleConfig
pHandleConfig = pHandleConfig_ ""

pHandleConfig_
    :: T.Text
        -- ^ prefix for the command line options.
    -> OptionParser HandleConfig
pHandleConfig_ prefix = option textReader
    % long (T.unpack prefix <> "log-handle")
    <> metavar "stdout|stderr|file:<FILENAME>|es:<HOST>:<PORT>"
    <> help "handle where the logs are written"

-- -------------------------------------------------------------------------- --
-- Logger Backend Configuration

-- | BackendConfig
--
data BackendConfig = BackendConfig
    { _backendConfigColor :: !ColorOption
    , _backendConfigFormat :: !LogFormat
    , _backendConfigHandle :: !HandleConfig
    }
    deriving (Show, Eq, Ord, Generic, NFData)

makeLenses ''BackendConfig

defaultBackendConfig :: BackendConfig
defaultBackendConfig = BackendConfig
    { _backendConfigColor = defaultColorOption
    , _backendConfigFormat = LogFormatText
    , _backendConfigHandle = StdOut
    }

validateBackendConfig :: ConfigValidation BackendConfig []
validateBackendConfig o = do
        validateHandleConfig $ _backendConfigHandle o
        case (_backendConfigHandle o, _backendConfigColor o) of
            (FileHandle _, ColorTrue) ->
                tell ["log messages are formatted using ANSI color escape codes but are written to a file"]
            _ -> return ()

instance ToJSON BackendConfig where
    toJSON o = object
        [ "color" .= _backendConfigColor o
        , "format" .= _backendConfigFormat o
        , "handle" .= _backendConfigHandle o
        ]

instance FromJSON (BackendConfig -> BackendConfig) where
    parseJSON = withObject "BackendConfig" $ \o -> id
        <$< backendConfigColor ..: "color" % o
        <*< backendConfigFormat ..: "format" % o
        <*< backendConfigHandle ..: "handle" % o

pBackendConfig :: MParser BackendConfig
pBackendConfig = pBackendConfig_ ""

-- | A version of 'pLoggerBackendConfig' that takes a prefix for the
-- command line option.
--
pBackendConfig_
    :: T.Text
        -- ^ prefix for this and all subordinate command line options.
    -> MParser BackendConfig
pBackendConfig_ prefix = id
    <$< backendConfigColor .:: pColorOption_ prefix
    <*< backendConfigFormat .:: pLogFormat_ prefix
    <*< backendConfigHandle .:: pHandleConfig_ prefix

-- -------------------------------------------------------------------------- --
-- Logging System Configuration

-- | An user provided label that is used to tag log messages from nodes.
--
-- It intended purpose is to allow users to defines arbitrary groups of nodes
-- and tag the respective log messages.
--
newtype ClusterId = ClusterId T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (IsString, ToJSON, FromJSON)
    deriving anyclass (NFData)

instance HasTextRepresentation ClusterId where
    toText (ClusterId t) = t
    fromText = return . ClusterId

data LogConfig = LogConfig
    { _logConfigLogger :: !LoggerConfig
    , _logConfigBackend :: !BackendConfig
    , _logConfigTelemetryBackend :: !(EnableConfig BackendConfig)
    , _logConfigClusterId :: !(Maybe ClusterId)
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logConfigLogger = defaultLoggerConfig
    , _logConfigBackend = defaultBackendConfig
    , _logConfigTelemetryBackend = defaultEnableConfig defaultBackendConfig
    , _logConfigClusterId = Nothing
    }

validateLogConfig :: ConfigValidation LogConfig []
validateLogConfig o = do
    validateLoggerConfig $ _logConfigLogger o
    validateBackendConfig $ _logConfigBackend o
    validateBackendConfig $ _enableConfigConfig $ _logConfigTelemetryBackend o

instance ToJSON LogConfig where
    toJSON o = object
        [ "logger" .= _logConfigLogger o
        , "backend" .= _logConfigBackend o
        , "telemetryBackend" .= _logConfigTelemetryBackend o
        , "clusterId" .= _logConfigClusterId o
        ]

instance FromJSON (LogConfig -> LogConfig) where
    parseJSON = withObject "LogConfig" $ \o -> id
        <$< logConfigLogger %.: "logger" % o
        <*< logConfigBackend %.: "backend" % o
        <*< logConfigTelemetryBackend %.: "telemetryBackend" % o
        <*< logConfigClusterId ..: "clusterId" % o

pLogConfig :: MParser LogConfig
pLogConfig = pLogConfig_ ""

-- | A version of 'pLogConfig' that takes a prefix for the command
-- line option.
--
pLogConfig_
    :: T.Text
        -- ^ prefix for this and all subordinate command line options.
    -> MParser LogConfig
pLogConfig_ prefix = id
    <$< logConfigLogger %:: pLoggerConfig_ prefix
    <*< logConfigBackend %:: pBackendConfig_ prefix
    <*< logConfigTelemetryBackend %::
        pEnableConfig "telemetry-logger" (pBackendConfig_ $ "telemetry-" <> prefix)
    <*< logConfigClusterId .:: fmap Just % textOption
        % long "cluster-id"
        <> help "a label that is added to all log messages from this node"
