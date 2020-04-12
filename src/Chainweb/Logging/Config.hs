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
, logConfigAmberdataBackend
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

-- internal modules

import Chainweb.Logging.Amberdata
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
    , _logConfigAmberdataBackend :: !(EnableConfig AmberdataConfig)
    , _logConfigClusterId :: !(Maybe ClusterId)
    , _logConfigFilter :: !LogFilter
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogConfig

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logConfigLogger = defaultLoggerConfig
    , _logConfigBackend = defaultBackendConfig
    , _logConfigTelemetryBackend = defaultEnableConfig defaultBackendConfig
      -- Amberdata logging disabled by default
    , _logConfigAmberdataBackend = EnableConfig False defaultAmberdataConfig
    , _logConfigClusterId = Nothing
    , _logConfigFilter = mempty
    }

validateLogConfig :: ConfigValidation LogConfig []
validateLogConfig o = do
    validateLoggerConfig $ _logConfigLogger o
    validateBackendConfig $ _logConfigBackend o
    validateEnableConfig validateBackendConfig $ _logConfigTelemetryBackend o
    validateEnableConfig validateAmberdataConfig $ _logConfigAmberdataBackend o

instance ToJSON LogConfig where
    toJSON o = object
        [ "logger" .= _logConfigLogger o
        , "backend" .= _logConfigBackend o
        , "telemetryBackend" .= _logConfigTelemetryBackend o
        , "clusterId" .= _logConfigClusterId o
        -- hidden:  "amberdataBackend" .= _logConfigAmberdataBackend o
        , "filter" .= _logConfigFilter o
        ]

instance FromJSON (LogConfig -> LogConfig) where
    parseJSON = withObject "LogConfig" $ \o -> id
        <$< logConfigLogger %.: "logger" % o
        <*< logConfigBackend %.: "backend" % o
        <*< logConfigTelemetryBackend %.: "telemetryBackend" % o
        <*< logConfigClusterId ..: "clusterId" % o
        <*< logConfigAmberdataBackend %.: "amberdataBackend" % o
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
        <> metavar "KEY:VALUE:LOGLEVEL"

    readEntry s = case T.splitOn ":" (T.pack s) of
        [a,b,c] -> first T.unpack $ do
            validateNonEmpty "KEY" a
            l <- readLogLevel c
            -- return $ set logFilterRules [ ((a,b),l) ] mempty
            return $ LogFilter [((a,b),l)] Debug
        _ -> Left $ "expecting KEY:VALUE:LOGLEVEL, but got " <> s

    pFilterDefault = LogFilter [] <$> option (eitherReader $ readLogLevel . T.pack)
        % prefixLong prefix "log-filter-default"
        <> help "default log filter level, which is applied to all messages that don't match any other filter rule"
        <> metavar "LOGLEVEL"
