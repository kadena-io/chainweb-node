{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Utils.Logging
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module defines log messages that are similar to Haskell exceptions from
-- 'Control.Exception'.
--
-- Log messages and exceptions are similar in that they can be emitted/thrown
-- anywhere in the code base (in the IO monad, for logs) and are propagated
-- upward through the call stack, until they are eventually picked up by some
-- handler. The difference is that exceptions synchronously interrupt the
-- computation that throws them, while log messages are emitted asynchronously
-- and the computation that emits them continues while the message is handled.
--
-- Log messages are usually handled only at the top level by a global handler
-- (or stack of handlers), but that depends on the implementation of the logger
-- (usually a queue, but sometimes just an IO callback), which is orthorgonal to
-- the the definitions in this module.
--
-- Like exceptions, log messages also should be typed dynamically and classes of
-- log messages types should be extensible.
--
-- Unlike exceptions, log messages must be handled. The type systems ensures
-- that there is a /base log handler/ that catches messages of any type, even if
-- it just discards all messages.
--
module Utils.Logging
(
-- * Base Logger Backend
  SomeBackend

-- * Log Handlers
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

-- * Backends
, withFileHandleBackend
, withTextFileHandleBackend
, withJsonFileHandleBackend
, withJsonEventSourceBackend

-- * Of-the-shelf logger
, withFileHandleLogger

-- ** Example Logger
, withExampleLogger

-- * Configuration
, L.LoggerHandleConfig(..)
, JsonLoggerConfig(..)
, defaultJsonLoggerConfig
, pJsonLoggerConfig

, configureHandler

-- * Utils
, l2l
, toBackendLogMessage
, fromBackendLogMessage
) where

import Configuration.Utils hiding (Error)

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable

import GHC.Generics

import qualified Network.HTTP.Client as HTTP
import Network.Wai.Application.Static
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Network.Wai.UrlMap

import System.IO
import qualified System.Logger as L
import qualified System.Logger.Internal as L

-- internal modules

import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Utils

import Data.LogMessage

import P2P.Node

import Utils.Logging.Handle

-- -------------------------------------------------------------------------- --
-- Json Logger Configuration

data JsonLoggerConfig = JsonLoggerConfig
    { _jsonLoggerConfigHandle :: !LoggerHandleConfig
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''JsonLoggerConfig

defaultJsonLoggerConfig :: JsonLoggerConfig
defaultJsonLoggerConfig = JsonLoggerConfig StdOut

instance ToJSON JsonLoggerConfig where
    toJSON o = object
        [ "handle" .= _jsonLoggerConfigHandle o
        ]

instance FromJSON (JsonLoggerConfig -> JsonLoggerConfig) where
    parseJSON = withObject "JsonLoggerConfig" $ \o -> id
        <$< jsonLoggerConfigHandle ..: "jsonLoggerConfig" % o

pJsonLoggerConfig :: Maybe String -> MParser JsonLoggerConfig
pJsonLoggerConfig logger = id
    <$< jsonLoggerConfigHandle .::
        maybe pLoggerHandleConfig (pLoggerHandleConfig_ . T.pack) logger

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
    Left msg -> Just . Left <$> return msg
    Right msg -> fmap Right <$> f msg
{-# INLINEABLE maybeLogHandle #-}

-- | This is most the powerful handle function that allows to implement generic
-- base backends and to inspect interal log messages of the logging system.
--
genericLogHandle
    :: Monoid b
    => LogMessage a
    => (BackendLogMessage a -> IO (Maybe (BackendLogMessage SomeLogMessage)))
    -> GenericBackend b
    -> GenericBackend b
genericLogHandle f b msg = case fromBackendLogMessage msg of
    Nothing -> b msg
    Just amsg -> f amsg >>= \case
        Nothing -> return mempty
        Just msg' -> b msg'
{-# INLINEABLE genericLogHandle #-}

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
    Left msg -> Just . Left <$> return msg
    Right msg -> fmap Right <$> b msg
{-# INLINEABLE maybeLogHandler #-}

logHandles :: Monoid b => Foldable f => f LogHandler -> GenericBackend b -> GenericBackend b
logHandles = flip $ foldr $ \case (LogHandler h) -> genericLogHandle h
{-# INLINEABLE logHandles #-}

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Enables a logger in a log-handler stack based on its 'EnabledConfig'
-- wrapper. If the logger is disabled, messages are passed to the inner backend.
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
-- Generic Logger

-- | This Generic File logger can be used as the base logger in a logger
-- stack.
--
withFileHandleBackend :: L.HandleBackendConfig -> (BaseBackend -> IO a) -> IO a
withFileHandleBackend = L.withHandleBackend_ logText
{-# INLINEABLE withFileHandleBackend #-}

-- -------------------------------------------------------------------------- --
-- Text Backend

-- | This Logger logs only 'TextLog' messages.
--
-- TODO: it may be more usefull to have a logger that logs all 'Right' messages
--
withTextFileHandleBackend
    :: EnableConfig L.HandleBackendConfig
    -> (TextBackend -> IO a)
    -> IO a
withTextFileHandleBackend = configureHandler handler
  where
    handler config inner = L.withHandleBackend_ logText config
        $ \b -> inner (b . Right)
{-# INLINEABLE withTextFileHandleBackend #-}

-- -------------------------------------------------------------------------- --
-- Json Backend

newtype JsonLogMessage a = JsonLogMessage
    { _getJsonLogMessage :: L.LogMessage (JsonLog a) }
    deriving (Generic)

instance ToJSON a => ToJSON (JsonLogMessage a) where
    toJSON (JsonLogMessage a) = object
        [ "level" .= L._logMsgLevel a
        , "scope" .= scopeToJson (L._logMsgScope a)
        , "time" .= L.formatIso8601Milli @T.Text (L._logMsgTime a)
        , "message" .= case L._logMsg a of (JsonLog msg) -> msg
        ]
      where
        scopeToJson = object . map (uncurry (.=)) . reverse

-- | This logger produces one JSON value for each log message of the given type
-- @a@. If the Elasticsearch handle is used, the logs are send to an
-- Elasticsearchserver. Otherwise JSON values are separated by newline
-- characters.
--
-- If a logfile is used, the file is opend in write mode and previous content is
-- deleted.
--
-- Note that the output of this logger as a whole doesn't represent a valid JSON
-- document.
--
withJsonFileHandleBackend
    :: forall a b
    . ToJSON a
    => Typeable a
    => HTTP.Manager
    -> EnableConfig JsonLoggerConfig
    -> (JsonBackend a -> IO b)
    -> IO b
withJsonFileHandleBackend mgr = configureHandler handler
  where
    handler c inner = case _jsonLoggerConfigHandle c of
        StdOut -> inner $ backend stdout
        StdErr -> inner $ backend stderr
        FileHandle f -> withFile f WriteMode $ \h -> inner $ backend h
        ElasticSearch f -> withElasticsearchBackend mgr f (T.toLower $ sshow (typeRep (Proxy @a))) inner
            -- FIXME pass index name as argument
    backend h = BL8.hPutStrLn h . encode . JsonLogMessage

-- | A backend for JSON log messags that sends all logs to the given index of an
-- Elasticsearch server. The index is created at startup if it doesn't exist.
-- Messages are sent in a fire-and-forget fashion. If a connection fails, the
-- messages are dropped without notice.
--
-- TODO: if the backend fails to deliver a message it should produce a result
-- that allows the message (or the failure message) to be passed to another
-- handler.
--
withElasticsearchBackend
    :: ToJSON a
    => HTTP.Manager
    -> HostAddress
    -> T.Text
    -> (JsonBackend a -> IO b)
    -> IO b
withElasticsearchBackend mgr esServer ixName inner = do
    void $ HTTP.httpLbs putIndex mgr
        -- FIXME Do we need failure handling? This is fire and forget
        -- which may be fine for many applications.
    inner $ \a -> void $ HTTP.httpLbs (putLog a) mgr
        -- FIXME failure handling?
        -- FIXME implement batching

  where
    putIndex = HTTP.defaultRequest
        { HTTP.method = "PUT"
        , HTTP.host = hostnameBytes (_hostAddressHost esServer)
        , HTTP.port = int (_hostAddressPort esServer)
        , HTTP.path = T.encodeUtf8 ixName
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 1000000
        , HTTP.requestHeaders = [("content-type", "application/json")]
        }

    putLog a = HTTP.defaultRequest
        { HTTP.method = "POST"
        , HTTP.host = hostnameBytes (_hostAddressHost esServer)
        , HTTP.port = int (_hostAddressPort esServer)
        , HTTP.path = T.encodeUtf8 ixName <> "/_doc"
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 1000000
        , HTTP.requestHeaders = [("content-type", "application/json")]
        , HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ JsonLogMessage a
        }

-- | A backend for JSON log messages that publishes messages via an HTTP event
-- source on the given port.
--
withJsonEventSourceBackend
    :: ToJSON a
    => W.Port
    -> (JsonBackend a -> IO b)
    -> IO b
withJsonEventSourceBackend port inner = do
    c <- newChan
    snd <$> concurrently (serve c) (inner $ backend c)
  where
    serve c = W.run port $ simpleCors $ eventSourceAppChan c
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
    -> (JsonBackend a -> IO b)
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
        <|> mount "events" (simpleCors $ eventSourceAppChan c)
        -- <|> mountRoot (simpleCors $ eventSourceAppChan c)

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
    -> L.LogConfig
        -- ^ Base logger configuration
    -> EnableConfig JsonLoggerConfig
        -- ^ Sessions logger configuration
    -> FilePath
    -> (L.Logger SomeLogMessage -> IO α)
    -> IO α
withExampleLogger port config _sessionsConfig staticDir f = do
    withFileHandleBackend (L._logConfigBackend config)
        -- $ \baseBackend -> withJsonFileHandleBackend @P2pSessionInfo sessionsConfig
        -- $ \baseBackend -> withJsonEventSourceBackend @P2pSessionInfo 8000
        $ \baseBackend -> withJsonEventSourceAppBackend @P2pSessionInfo port staticDir
        $ \sessionsBackend -> do
            let loggerBackend = logHandles
                    [ logHandler sessionsBackend
                    ]
                    baseBackend
                        -- The type system enforces that backend is a base logger.
            L.withLogger (L._logConfigLogger config) loggerBackend f
