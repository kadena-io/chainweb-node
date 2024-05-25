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

-- |
-- Module: Chainweb.Logging.Config
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Logging Configuration
--
module Chainweb.Logging.Config
(
-- * Logging Config
  ClusterId
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

import Data.Bifunctor
import Data.String
import qualified Data.Text as T

import GHC.Generics

import System.Logger.Logger

import Text.Read

-- internal modules

import Chainweb.Utils

import Utils.Logging
import Utils.Logging.Config

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


-- | General logging config
--
data LogConfig = LogConfig
    { _logConfigLogger :: !LoggerConfig
    , _logConfigBackend :: !BackendConfig
    , _logConfigTelemetryBackend :: !(EnableConfig BackendConfig)
    , _logConfigClusterId :: !(Maybe ClusterId)
    , _logConfigFilter :: !LogFilter
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logConfigLogger = defaultLoggerConfig
    , _logConfigBackend = defaultBackendConfig
    , _logConfigTelemetryBackend = defaultDisableConfig defaultBackendConfig
    , _logConfigClusterId = Nothing
    , _logConfigFilter = mempty
    }

validateLogConfig :: ConfigValidation LogConfig []
validateLogConfig o = do
    validateLoggerConfig $ _logConfigLogger o
    validateBackendConfig $ _logConfigBackend o
    validateEnableConfig validateBackendConfig $ _logConfigTelemetryBackend o

instance ToJSON LogConfig where
    toJSON o = object
        [ "logger" .= _logConfigLogger o
        , "backend" .= _logConfigBackend o
        , "telemetryBackend" .= _logConfigTelemetryBackend o
        , "clusterId" .= _logConfigClusterId o
        , "filter" .= _logConfigFilter o
        ]

instance FromJSON (LogConfig -> LogConfig) where
    parseJSON = withObject "LogConfig" $ \o -> id
        <$< logConfigLogger %.: "logger" % o
        <*< logConfigBackend %.: "backend" % o
        <*< logConfigTelemetryBackend %.: "telemetryBackend" % o
        <*< logConfigClusterId ..: "clusterId" % o
        <*< logConfigFilter . fromLeftMonoidalUpdate %.: "filter" % o

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
        % prefixLong maybePrefix "cluster-id"
        <> help "a label that is added to all log messages from this node"
    <*< logConfigFilter %:: pFilter_ maybePrefix
  where
    maybePrefix
        | T.null prefix = Nothing
        | otherwise = Just (T.unpack prefix)

pFilter_ :: Maybe String -> MParser LogFilter
pFilter_ prefix = id
    <$< pLeftMonoidalUpdate pFilterRule
    <*< pLeftMonoidalUpdate pFilterDefault
  where
    pFilterRule = option (eitherReader readEntry)
        % prefixLong prefix "log-filter-rule"
        <> help "A log filter rule. Log messages with matching scope are discarded if they don't meet the log level threshold."
        <> metavar "KEY:VALUE:LOGLEVEL[:RATE]"

    readEntry s = case T.splitOn ":" (T.pack s) of
        [a,b,c] -> do
            first T.unpack $ validateNonEmpty "KEY" a
            l <- readLogLevel c
            return $ LogFilter [LogFilterRule (a, b) l (Probability 1)] Debug (Probability 1)
        [a,b,c,d] -> do
            first T.unpack $ validateNonEmpty "KEY" a
            l <- readLogLevel c
            r <- readEither (T.unpack d)
            if 0 <= r && r <= 1
                then return $ LogFilter [LogFilterRule (a, b) l (Probability r)] Debug (Probability 1)
                else Left "failed to read log rule rate. The value must be between zero and one"
        _ -> Left $ "expecting KEY:VALUE:LOGLEVEL[:RATE], but got " <> s

    pFilterDefault = option (eitherReader readDefault)
        % prefixLong prefix "log-filter-default"
        <> help "default log filter, which is applied to all messages that don't match any other filter rule"
        <> metavar "LOGLEVEL:RATE"

    readDefault s = case T.splitOn ":" (T.pack s) of
        [a] -> do
            l <- readLogLevel a
            return $ LogFilter [] l (Probability 1)
        [a,b] -> do
            l <- readLogLevel a
            r <- readEither (T.unpack b)
            if 0 <= r && r <= 1
                then return $ LogFilter [] l (Probability r)
                else Left "failed to read log rule rate. The value must be between zero and one"
        _ -> Left $ "expecting LOGLEVEL[:RATE], but got " <> s
