{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module: Utils.Logging.Handle
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Utils.Logging.Handle
( LoggerHandleConfig(..)
, pLoggerHandleConfig
, pLoggerHandleConfig_
, validateLoggerHandleConfig
) where

import Configuration.Utils
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Monad.Catch

import qualified Data.CaseInsensitive as CI
import Data.String
import qualified Data.Text as T

import GHC.Generics

-- internal modules

import Chainweb.HostAddress
import Chainweb.Utils

data LoggerHandleConfig
    = StdOut
    | StdErr
    | FileHandle FilePath
    | ElasticSearch HostAddress
    deriving (Show, Eq, Ord, Generic)

instance NFData LoggerHandleConfig

loggerHandleConfigFromText :: MonadThrow m => T.Text -> m LoggerHandleConfig
loggerHandleConfigFromText x = case CI.mk x of
    "stdout" -> return StdOut
    "stderr" -> return StdErr
    _ | CI.mk (T.take 5 x) == "file:" -> return $ FileHandle (T.unpack (T.drop 5 x))
    _ | CI.mk (T.take 3 x) == "es:" -> ElasticSearch <$> fromText (T.drop 3 x)

    e -> throwM $ DecodeException $ "unexpected logger handle value: "
        <> fromString (show e)
        <> ", expected \"stdout\", \"stderr\", \"file:<FILENAME>\", or \"es:<HOST>:<PORT>\""

loggerHandleConfigToText :: LoggerHandleConfig -> T.Text
loggerHandleConfigToText StdOut = "stdout"
loggerHandleConfigToText StdErr = "stderr"
loggerHandleConfigToText (FileHandle f) = "file:" <> T.pack f
loggerHandleConfigToText (ElasticSearch f) = "file:" <> toText f

instance HasTextRepresentation LoggerHandleConfig where
    toText = loggerHandleConfigToText
    fromText = loggerHandleConfigFromText

    {-# INLINE toText #-}
    {-# INLINE fromText #-}

validateLoggerHandleConfig :: ConfigValidation LoggerHandleConfig l
validateLoggerHandleConfig (FileHandle filepath) = validateFileWritable "file handle" filepath
validateLoggerHandleConfig _ = return ()

instance ToJSON LoggerHandleConfig where
    toJSON = String . loggerHandleConfigToText

instance FromJSON LoggerHandleConfig where
    parseJSON = parseJsonFromText "LoggerHandleConfig"

pLoggerHandleConfig :: OptionParser LoggerHandleConfig
pLoggerHandleConfig = pLoggerHandleConfig_ ""

pLoggerHandleConfig_
    :: T.Text
        -- ^ prefix for the command line options.
    -> OptionParser LoggerHandleConfig
pLoggerHandleConfig_ prefix = option textReader
    % long (T.unpack prefix <> "logger-backend-handle")
    <> metavar "stdout|stderr|file:<FILENAME>|es:<HOST>:<PORT>"
    <> help "handle where the logs are written"

