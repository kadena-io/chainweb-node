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

-- |
-- Module: Utils.Logging.Config
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Configuration for generic logging utils
--
module Utils.Logging.Config
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

) where

import Configuration.Utils
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Lens.TH
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Writer (tell)

import qualified Data.CaseInsensitive as CI
import Data.String
import qualified Data.Text as T

import GHC.Generics

import qualified Network.HTTP.Client as HTTP

import System.Logger.Backend.ColorOption

-- internal modules

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
    | ElasticSearch T.Text
    deriving (Show, Eq, Ord, Generic)

instance NFData HandleConfig

handleConfigFromText :: MonadThrow m => T.Text -> m HandleConfig
handleConfigFromText x = case CI.mk x of
    "stdout" -> return StdOut
    "stderr" -> return StdErr
    _ | CI.mk (T.take 5 x) == "file:" -> return $ FileHandle (T.unpack (T.drop 5 x))
    _ | CI.mk (T.take 3 x) == "es:" -> return $ ElasticSearch (T.drop 3 x)
    e -> configFromTextErr e

  where configFromTextErr e =
          throwM $ DecodeException $ "unexpected logger handle value: "
          <> fromString (show e)
          <> ", expected \"stdout\", \"stderr\", \"file:<FILENAME>\", or \"es:<URL>\""

handleConfigToText :: HandleConfig -> T.Text
handleConfigToText StdOut = "stdout"
handleConfigToText StdErr = "stderr"
handleConfigToText (FileHandle f) = "file:" <> T.pack f
handleConfigToText (ElasticSearch f) = "es:" <> f

instance HasTextRepresentation HandleConfig where
    toText = handleConfigToText
    fromText = handleConfigFromText

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

validateHandleConfig :: ConfigValidation HandleConfig l
validateHandleConfig (FileHandle filepath) = validateFileWritable "file handle" filepath
validateHandleConfig (ElasticSearch urlText) = case HTTP.parseRequest (T.unpack urlText) of
    Left e -> throwError $ "failed to parse URL for ElasticSearch logging backend handle: " <> sshow e
    Right _ -> return ()

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
    <> metavar "stdout|stderr|file:<FILENAME>|es:<URL>"
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

