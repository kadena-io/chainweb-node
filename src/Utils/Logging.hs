{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Utils.Logging
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module provides backends for handling log messages from the
-- "Data.LogMessage". Log messages that are similar to Haskell exceptions from
-- 'Control.Exception'. Like exceptions, log messages in this module are typed
-- dynamically and classes of log messages types are extensible.
--
-- Log messages and exceptions are similar in that they can be emitted/thrown
-- anywhere in the code base (in the IO monad, for logs) and are propagated to
-- handlers that are defined upward in the call stack until they are eventually
-- picked up. The difference is that exceptions synchronously interrupt the
-- computation that throws them, while log messages are usually handled
-- asynchronously and the computation that emits them continues while the
-- message is handled.
--
-- Log messages are usually handled only at the top level by a global handler
-- (or stack of handlers), but that depends on the implementation of the logger
-- (usually a queue, but sometimes just an IO callback), which is orthorgonal to
-- the definitions in this module. The backends in this module use the type from
-- the package @yet-another-logger@, which also provides an implementation of a
-- logger queue.
--
-- Unlike exceptions, log messages must be handled. The type systems ensures
-- that there is a /base log handler/ that catches messages of any type, even if
-- it just discards all messages.
--
module Utils.Logging
(
-- * Base Logger Backend
  SomeBackend

-- * Log Handlers Backends
, Backend
, TextBackend
, JsonBackend
, SomeJsonBackend

-- * Log Message Handlers
, logHandle
, maybeLogHandle
, genericLogHandle

-- * Logging Backend Stacks
, LogHandler(..)
, logHandler
, maybeLogHandler
, logHandles

-- * Filter LogScope Backend
, LogFilter(..)
, logFilterRules
, logFilterDefault
, logFilterHandle

-- * Base Backends
, withBaseHandleBackend

-- * Specialized Backends
, withTextHandleBackend
, withJsonHandleBackend
, withJsonEventSourceBackend

-- * Of-the-shelf logger
, withFileHandleLogger

-- ** Example Logger
, withExampleLogger

-- * Configuration
, configureHandler

-- * Utils
, toBackendLogMessage
, fromBackendLogMessage
) where

import Configuration.Utils hiding (Error)

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Control

import Data.Aeson.Encoding hiding (int, bool)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as List
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time

import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import Network.Wai (Middleware)
import Network.Wai.Application.Static
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Network.Wai.UrlMap

import Numeric.Natural

import System.IO
import qualified System.Logger as L
import System.Logger.Backend.ColorOption
import qualified System.Logger.Internal as L
import System.LogLevel

-- internal modules

import Chainweb.Utils hiding (check)

import Data.LogMessage

import P2P.Node

import Utils.Logging.Config

-- -------------------------------------------------------------------------- --
-- Orphans

deriving instance Functor L.LogMessage

-- -------------------------------------------------------------------------- --
-- Base Logger Backend

-- | The type of log messages handled by backends. 'Left' values are messages
-- that are emitted by the logging system itself. 'Right' values are messages
-- from the application.
--
type BackendLogMessage a = Either (L.LogMessage T.Text) (L.LogMessage a)

-- | A fully generic backend that can be used to implement any backend including
-- base backends and backends that inspect internal log messages of the logging
-- system.
--
type GenericBackend a = BackendLogMessage SomeLogMessage -> IO a

-- | A backend that is used for partial log message handlers that may pass
-- messages on to subsequent handlers.
--
type SomeBackend = GenericBackend (Maybe (BackendLogMessage SomeLogMessage))

-- | A base backend that handles any log message that is given to it.
--
type BaseBackend = GenericBackend ()

-- -------------------------------------------------------------------------- --
-- Log Handlers

-- TODO: Rename these types to avoid confusion with the Backend types defined
-- above. Maybe @LogProcessor@, @LogMsgHandler@, or @LogHandlerFun@?

type Backend a = L.LogMessage a -> IO ()
type TextBackend = Backend TextLog
type JsonBackend a = Backend (JsonLog a)
type SomeJsonBackend = Backend SomeJsonLog

-- -------------------------------------------------------------------------- --
-- Log Handler

toBackendLogMessage
    :: LogMessage a
    => BackendLogMessage a
    -> BackendLogMessage SomeLogMessage
toBackendLogMessage (Left msg) = Left msg
toBackendLogMessage (Right msg) = Right $ msg & L.logMsg %~ toLogMessage
{-# INLINEABLE toBackendLogMessage #-}

fromBackendLogMessage
    :: LogMessage a
    => BackendLogMessage SomeLogMessage
    -> Maybe (BackendLogMessage a)
fromBackendLogMessage (Left msg) = Just $ Left msg
fromBackendLogMessage (Right msg) = case fromLogMessage (L._logMsg msg) of
    Just (x :: a) -> Just . Right $ msg & L.logMsg .~ x
    Nothing -> Nothing
{-# INLINEABLE fromBackendLogMessage #-}

-- | A log handle that handles all messages of a given type.
--
logHandle
    :: Monoid b
    => LogMessage a
    => Backend a
    -> GenericBackend b
    -> GenericBackend b
logHandle f = maybeLogHandle $ \m -> Nothing <$ f m
{-# INLINEABLE logHandle #-}

-- | A log message handle that may emit a new log message. It can for instance
-- be used to forward unhandled log messages.
--
maybeLogHandle
    :: Monoid b
    => LogMessage a
    => (L.LogMessage a -> IO (Maybe (L.LogMessage SomeLogMessage)))
    -> GenericBackend b
    -> GenericBackend b
maybeLogHandle f = genericLogHandle $ \case
    (Left !msg) -> Just . Left <$!> return msg
    (Right !msg) -> fmap Right <$!> f msg
{-# INLINEABLE maybeLogHandle #-}

-- | This is most the powerful handle function that allows to implement generic
-- base backends and to inspect internal log messages of the logging system.
--
genericLogHandle
    :: Monoid b
    => LogMessage a
    => (BackendLogMessage a -> IO (Maybe (BackendLogMessage SomeLogMessage)))
    -> GenericBackend b
    -> GenericBackend b
genericLogHandle f b msg = case fromBackendLogMessage msg of
    Nothing -> b msg
    (Just !amsg) -> f amsg >>= \case
        Nothing -> return mempty
        (Just !msg') -> b msg'
{-# INLINEABLE genericLogHandle #-}

-- -------------------------------------------------------------------------- --
-- Filter LogScope Handle

-- | A filter for log messages.
--
-- Tese are the rules for processing a log message:
--
-- * If a log label of a message matches the key and value of a rule, the
-- message is discarded if the log level of the message is larger than the log
-- level of the rule.
--
-- * If, after applying all rules, no rule matched any label of the log message,
-- the message is discarded if the log level of the message is larger than the
-- default level of the filter.
--
-- These semantics seem to be useful under certain circumstances. At least, the
-- order of the rules doesn't matter.
--
-- When a filter is specified more than once in a configuration, filters are
-- merged by concatenating the lists of rules and by taking the minimum log
-- level.
--
-- The default log filter has no rules and log level debug as default level.
--
data LogFilter = LogFilter
    { _logFilterRules :: ![(L.LogLabel, L.LogLevel)]
    , _logFilterDefault :: !L.LogLevel
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogFilter

instance Semigroup LogFilter where
    a <> b = LogFilter
        { _logFilterRules = _logFilterRules a <> _logFilterRules b
        , _logFilterDefault = min (_logFilterDefault a) (_logFilterDefault b)
        }
    {-# INLINE (<>) #-}

instance Monoid LogFilter where
    mempty = LogFilter
        { _logFilterRules = mempty
        , _logFilterDefault = maxBound
        }
    {-# INLINE mempty #-}

instance ToJSON LogFilter where
    toJSON a = object
        [ "rules" .= (f <$> _logFilterRules a)
        , "default" .=  _logFilterDefault a
        ]
      where
        f ((key, val), lev) = object
            [ "key" .= key
            , "value" .= val
            , "level" .= lev
            ]
    {-# INLINE toJSON #-}

instance FromJSON LogFilter where
    parseJSON = withObject "LogFilter" $ \o -> LogFilter
        <$> (o .: "rules" >>= traverse f)
        <*> o .: "default"
      where
        f = withObject "LogRule" $ \o -> (\x y z -> ((x,y),z))
            <$> o .: "key"
            <*> o .: "value"
            <*> o .: "level"
    {-# INLINE parseJSON #-}

-- | A handle that applies a log filter.
--
-- The filter is applied after log messages have been emitted according to the
-- log level of the respective logger.
--
logFilterHandle :: LogFilter -> LogHandler
logFilterHandle sf = maybeLogHandler $ \msg -> do
    let msgLevel = L._logMsgLevel msg
        apply l = case l `List.lookup` _logFilterRules sf of
            Just level -> Just (All $ level >= msgLevel)
            Nothing -> Nothing
    case mconcat $ apply <$> L._logMsgScope msg of
        Nothing
            | _logFilterDefault sf >= msgLevel -> return (Just msg)
            | otherwise -> return Nothing
        Just (All True) -> return (Just msg)
        Just (All False) -> return Nothing
{-# INLINEABLE logFilterHandle #-}

-- -------------------------------------------------------------------------- --
-- Log Handler Stacks

data LogHandler = forall a . LogMessage a
    => LogHandler (BackendLogMessage a -> IO (Maybe (BackendLogMessage SomeLogMessage)))

logHandler
    :: forall a . LogMessage a
    => Backend a
    -> LogHandler
logHandler f = maybeLogHandler $ \m -> Nothing <$ f m
{-# INLINEABLE logHandler #-}

maybeLogHandler
    :: forall a . LogMessage a
    => (L.LogMessage a -> IO (Maybe (L.LogMessage SomeLogMessage)))
    -> LogHandler
maybeLogHandler b = LogHandler $ \case
    (Left !msg) -> Just . Left <$!> return msg
    (Right !msg) -> fmap Right <$!> b msg
{-# INLINEABLE maybeLogHandler #-}

logHandles :: Monoid b => Foldable f => f LogHandler -> GenericBackend b -> GenericBackend b
logHandles = flip $ foldr $ \case (LogHandler h) -> genericLogHandle h
{-# INLINEABLE logHandles #-}

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Enables a logger in a log-handler stack based on its 'EnabledConfig'
-- wrapper. If the logger is disabled, messages are passed to the inner backend.
--
-- Usage Example:
--
-- @
-- withEnabledJsonHandleBackend mgr = configureHandler (withJsonHandleBackend mgr)
-- @
--
configureHandler
    :: (c -> (Backend b -> IO a) -> IO a)
    -> EnableConfig c
    -> (Backend b -> IO a)
    -> IO a
configureHandler logger config inner = case _enableConfigEnabled config of
    False -> inner (const $ return ())
    True -> logger (_enableConfigConfig config) inner

-- -------------------------------------------------------------------------- --
-- JSON Encoding for Log Messages

-- | Format a Log Message as JSON.
--
newtype JsonLogMessage a = JsonLogMessage
    { _getJsonLogMessage :: L.LogMessage a }
    deriving (Generic)

instance ToJSON a => ToJSON (JsonLogMessage a) where
    toJSON (JsonLogMessage a) = object
        [ "level" .= L._logMsgLevel a
        , "scope" .= scopeToJson (L._logMsgScope a)
        , "time" .= L.formatIso8601Milli @T.Text (L._logMsgTime a)
        , "message" .= L._logMsg a
        ]
      where
        scopeToJson = object . map (uncurry (.=)) . reverse

-- -------------------------------------------------------------------------- --
-- Base Backends

-- | A Base Backend for arbitrary log messages. All message bodies are encoded
-- using 'logText'.
--
-- If a logfile is used, the file is opend in write mode and previous content is
-- deleted.
--
-- If the Elasticsearch handle is used, the logs are send to an
-- Elasticsearchserver and the format setting is ignored.
--
-- Text Formatting:
--
-- A textual representation is produced for each log message. Log messages are
-- separated by newline charachters. No escaping is performed on newline
-- characters that are embedded in the logs.
--
-- JSON Formatting:
--
-- One JSON value is produced for each log message. JSON values are separated by
-- newline characters. Note that the output of this logger as a whole doesn't
-- represent a valid JSON document.
--
-- Message bodies are encoded as a JSON string. A possibly existing 'ToJSON'
-- instance is ignored. For formatting the message body using the 'ToJSON'
-- instances log messages can be wrapped into 'JsonLog' or instance the or the
-- function 'withJsonHandleBackend' can be used.
--
-- TODO: should we try to cast to 'SomeJsonLog a' or is that to much magic?
--
withBaseHandleBackend
    :: forall b
    . T.Text
    -> HTTP.Manager
    -> [(T.Text, T.Text)]
        -- Scope that are included only with remote backends. In chainweb-node
        -- this is used for package info data.
    -> BackendConfig
    -> (BaseBackend -> IO b)
    -> IO b
withBaseHandleBackend label mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode fdBackend
    ElasticSearch f ->
        withElasticsearchBackend mgr f (T.toLower label) pkgScopes esBackend
  where

    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ logText h colored
        LogFormatJson -> inner $ \case
            Right msg ->
                BL8.hPutStrLn h $ encode $ JsonLogMessage $ logText <$> msg
            Left msg -> do
                unless (h == stderr) $ errFallback msg
                BL8.hPutStrLn h $ encode $ JsonLogMessage $ msg

    esBackend b = inner $ \case
        Right msg -> b $ logText <$> msg
        Left msg -> errFallback msg >> b msg

    errFallback msg = do
        colored <- useColor (_backendConfigColor c) stderr
        L.handleBackend_ id stderr colored (Left $ msg)

-- -------------------------------------------------------------------------- --
-- Handle Backend For JSON Message

-- | This logger encodes the bodies of the log messages using the 'ToJSON'
-- instance of the respective type.
--
-- If a logfile is used, the file is opend in write mode and previous content is
-- deleted.
--
-- If the Elasticsearch handle is used, the logs are send to an
-- Elasticsearchserver and the format setting is ignored.
--
-- /Text Formatting:/
--
-- A textual representation is produced for each log message. Log messages are
-- separated by newline charachters. Message bodies are encoded using the
-- respective 'ToJSON' instance. The formatted log messages contain no unescaped
-- line break characters.
--
-- /JSON Formatting:/
--
-- One JSON value is produced for each log message. JSON values are separated by
-- newline characters. Note that the output of this logger as a whole doesn't
-- represent a valid JSON document.
--
-- Message bodies are encoded using the respective 'ToJSON' instance.
--
withJsonHandleBackend
    :: forall a b
    . ToJSON a
    => T.Text
    -> HTTP.Manager
    -> [(T.Text, T.Text)]
        -- Scope that are included only with remote backends. In chainweb-node
        -- this is used for package info data.
    -> BackendConfig
    -> (Backend a -> IO b)
    -> IO b
withJsonHandleBackend label mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode fdBackend
    ElasticSearch f -> withElasticsearchBackend mgr f (T.toLower label) pkgScopes inner
  where
    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ encodeToText h colored . Right
        LogFormatJson -> inner $
            BL8.hPutStrLn h . encode . JsonLogMessage
{-# INLINEABLE withJsonHandleBackend #-}

-- -------------------------------------------------------------------------- --
-- Backend For Text Message

-- | This Logger logs only 'TextLog' messages.
--
withTextHandleBackend
    :: forall a
    . T.Text
    -> HTTP.Manager
    -> [(T.Text, T.Text)]
        -- Scope that are included only with remote backends. In chainweb-node
        -- this is used for package info data.
    -> BackendConfig
    -> (TextBackend -> IO a)
    -> IO a
withTextHandleBackend label mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode $ \h -> fdBackend h
    ElasticSearch f -> withElasticsearchBackend mgr f (T.toLower label) pkgScopes $ \b ->
        inner (b . fmap logText)
  where

    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ logText h colored . Right
        LogFormatJson -> inner $
            BL8.hPutStrLn h . encode . JsonLogMessage . fmap logText
{-# INLINEABLE withTextHandleBackend #-}

-- TODO: it may be more usefull to have a logger that logs all 'Right' messages

-- -------------------------------------------------------------------------- --
-- Elasticsearch Backend

elasticSearchBatchSize :: Natural
elasticSearchBatchSize = 1000

elasticSearchBatchDelayMs :: Natural
elasticSearchBatchDelayMs = 1000

-- | A backend for JSON log messags that sends all logs to the given index of an
-- Elasticsearch server. The index is created at startup if it doesn't exist.
-- Messages are sent in a fire-and-forget fashion. If a connection fails, the
-- messages are dropped without notice.
--
-- TODO: if the backend fails to deliver a message it should produce a result
-- that allows the message (or the failure message) to be passed to another
-- handler.
--
-- TODO: Currently we allocate one pipeline for each index. Instead we may
-- use a separate function @withElasticSearchServer@ that provides the queue
-- and pass to each invocation of 'withElasticsearchBackend'.
--
-- TODO: move all of this to another module.
--
withElasticsearchBackend
    :: ToJSON a
    => HTTP.Manager
    -> T.Text
        -- ^ Server URL
    -> T.Text
        -- ^ Index Name
    -> [(T.Text, T.Text)]
        -- ^ Scope that are included only with remote backends. In chainweb-node
        -- this is used for package info data.
    -> (Backend a -> IO b)
    -> IO b
withElasticsearchBackend mgr esServer ixName pkgScopes inner = do
    req <- HTTP.parseUrlThrow (T.unpack esServer)
    i <- curIxName
    createIndex req i
    queue <- newTBQueueIO 2000
    withAsync (runForever errorLogFun "Utils.Logging.withElasticsearchBackend" (processor req queue)) $ \_ -> do
        inner $ \a -> atomically (writeTBQueue queue a)

  where
    curIxName = do
        d <- T.pack . formatTime defaultTimeLocale "%Y.%m.%d" <$> getCurrentTime
        return $! ixName <> "-" <> d

    errorLogFun Error msg = T.hPutStrLn stderr msg
    errorLogFun _ _ = return ()

    -- Collect messages. If there is at least one pending message, submit a
    -- `_bulk` request every second or when the batch size is 1000 messages,
    -- whatever happens first.
    --
    processor req queue = do
        i <- curIxName

        -- ensure that there is at least one transaction in every batch
        h <- atomically $ readTBQueue queue

        -- set timer to 1 second
        timer <- registerDelay (int elasticSearchBatchDelayMs)

        -- Fill the batch
        (remaining, batch) <- go i elasticSearchBatchSize (indexAction i h) timer

        createIndex req i
        errorLogFun Info $ "send " <> sshow (elasticSearchBatchSize - remaining) <> " messages"
        void $ HTTP.httpLbs (putBulgLog req batch) mgr
      where
        getNextAction timer = atomically $ isTimeout `orElse` fill
          where
            isTimeout = Nothing <$ (readTVar timer >>= check)
            fill = tryReadTBQueue queue >>= maybe retry (return . Just)

        go _ 0 !batch _ = return $! (0, batch)
        go i !remaining !batch !timer = getNextAction timer >>= \case
            Nothing -> return (remaining, batch)
            Just x -> go i (remaining - 1) (batch <> indexAction i x) timer

    createIndex req i =
        void $ HTTP.httpLbs (putIndex req i) { HTTP.method = "HEAD"} mgr
            `catch` \case
                (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException _ _)) -> do
                    errorLogFun Error $ "Index creation failed for index " <> i <> ". Retrying ..."
                    HTTP.httpLbs (putIndex req i) mgr
                ex -> throwM ex

    putIndex req i = req
        { HTTP.method = "PUT"
        , HTTP.path = HTTP.path req <> T.encodeUtf8 i
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
        , HTTP.requestHeaders = [("content-type", "application/json")]
        }

    _putLog req i a = req
        { HTTP.method = "POST"
        , HTTP.path = HTTP.path req <> T.encodeUtf8 i <> "/_doc"
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 3000000
        , HTTP.requestHeaders = [("content-type", "application/json")]
        , HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ JsonLogMessage a
        }

    putBulgLog req a = req
        { HTTP.method = "POST"
        , HTTP.path = HTTP.path req <> "/_bulk"
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
        , HTTP.requestHeaders = [("content-type", "application/x-ndjson")]
        , HTTP.requestBody = HTTP.RequestBodyLBS $ BB.toLazyByteString a
        }

    e = fromEncoding . toEncoding
    indexAction i a
        = fromEncoding (indexActionHeader i)
        <> BB.char7 '\n'
        <> e (JsonLogMessage $ L.logMsgScope <>~ pkgScopes $ a)
        <> BB.char7 '\n'

    indexActionHeader :: T.Text -> Encoding
    indexActionHeader i = pairs
        $ pair "index" $ pairs
            $ ("_index" .= (i :: T.Text))
            <> ("_type" .= ("_doc" :: T.Text))

-- -------------------------------------------------------------------------- --
-- Event Source Backend for JSON messages

-- | A backend for JSON log messages that publishes messages via an HTTP event
-- source on the given port.
--
withJsonEventSourceBackend
    :: ToJSON a
    => W.Port
    -> (Backend a -> IO b)
    -> IO b
withJsonEventSourceBackend port inner = do
    c <- newChan
    snd <$> concurrently (serve c) (inner $ backend c)
  where
    serve c = W.run port $ loggingCors $ eventSourceAppChan c
    backend c = writeChan c
        . ServerEvent Nothing Nothing
        . pure
        . fromEncoding
        . toEncoding
        . JsonLogMessage

-- | A backend for JSON log messages that publishes messages via an HTTP event
-- source on the given port. It also serves the static application at the given
-- directory.
--
-- This can be used to serve a client application that visualizes the log data
-- from the event source. An example can be found in @examples/P2pExample.hs@.
--
withJsonEventSourceAppBackend
    :: ToJSON a
    => W.Port
    -> FilePath
    -> (Backend a -> IO b)
    -> IO b
withJsonEventSourceAppBackend port staticDir inner = do
    c <- newChan
    snd <$> concurrently (serve c) (inner $ backend c)
  where
    serve c = W.run port (app c)
    backend c = writeChan c
        . ServerEvent Nothing Nothing
        . pure
        . fromEncoding
        . toEncoding
        . JsonLogMessage
    app c = mapUrls
        $ mount "frontendapp" (staticApp $ defaultWebAppSettings staticDir)
        <|> mount "frontend" (staticApp $ defaultFileServerSettings staticDir)
        <|> mount "events" (loggingCors $ eventSourceAppChan c)
        -- <|> mountRoot (loggingCors $ eventSourceAppChan c)

-- Simple cors with actualy simpleHeaders which includes content-type.
loggingCors :: Middleware
loggingCors = cors $ const $ Just $ simpleCorsResourcePolicy
  { corsRequestHeaders = simpleHeaders
  }
-- -------------------------------------------------------------------------- --
-- Out-Of-The-Box Logger

withFileHandleLogger
    :: (MonadIO m, MonadBaseControl IO m)
    => L.LogConfig
    -> (L.Logger SomeLogMessage -> m α)
    -> m α
withFileHandleLogger config f =
    L.withHandleBackend_ logText (L._logConfigBackend config)
        $ \backend -> L.withLogger (L._logConfigLogger config) backend f

-- -------------------------------------------------------------------------- --
-- Example Logger

withExampleLogger
    :: W.Port
    -> L.LoggerConfig
        -- ^ Logger configuration
    -> BackendConfig
        -- ^ Logger backend configurationjk
    -> FilePath
    -> (L.Logger SomeLogMessage -> IO a)
    -> IO a
withExampleLogger port loggerConfig backendConfig staticDir f = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    withBaseHandleBackend "example-logger" mgr [] backendConfig $ \baseBackend ->
        withJsonEventSourceAppBackend @(JsonLog P2pSessionInfo) port staticDir $ \sessionsBackend -> do
            let loggerBackend = logHandles
                    [ logHandler sessionsBackend ]
                    baseBackend
                        -- The type system enforces that backend is a base logger.
            L.withLogger loggerConfig loggerBackend f
