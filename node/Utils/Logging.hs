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
-- TODO
--
module Utils.Logging
(
  Logger
, loggerFun
, loggerFunText

-- * Base Logger Backend
, SomeBackend

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
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

import GHC.Generics

import Network.Wai.Application.Static
import Network.Wai.EventSource
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Network.Wai.UrlMap

import System.Clock
import System.IO
import qualified System.Logger as L
import System.LogLevel

-- internal modules

import Chainweb.Utils

import Data.LogMessage

import P2P.Node
import P2P.Session

import Paths_chainweb

-- -------------------------------------------------------------------------- --
-- Utils

l2l :: LogLevel -> L.LogLevel
l2l Quiet = L.Quiet
l2l Error = L.Error
l2l Warn = L.Warn
l2l Info = L.Info
l2l Debug = L.Debug
l2l (Other _) = L.Debug

type Logger = L.Logger SomeLogMessage

loggerFun :: Logger -> LogFunction
loggerFun logger level = L.loggerFunIO logger (l2l level) . toLogMessage

loggerFunText :: Logger -> LogFunctionText
loggerFunText = loggerFun

-- -------------------------------------------------------------------------- --
-- Json Logger Configuration

data JsonLoggerConfig = JsonLoggerConfig
    { _jsonLoggerConfigHandle :: !L.LoggerHandleConfig
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''JsonLoggerConfig

defaultJsonLoggerConfig :: JsonLoggerConfig
defaultJsonLoggerConfig = JsonLoggerConfig L.StdOut

instance ToJSON JsonLoggerConfig where
    toJSON o = object
        [ "fileHandle" .= _jsonLoggerConfigHandle o
        ]

instance FromJSON (JsonLoggerConfig -> JsonLoggerConfig) where
    parseJSON = withObject "JsonLoggerConfig" $ \o -> id
        <$< jsonLoggerConfigHandle ..: "jsonLoggerConfig" % o

pJsonLoggerConfig :: Maybe String -> MParser JsonLoggerConfig
pJsonLoggerConfig logger = id
    <$< jsonLoggerConfigHandle .::
        maybe L.pLoggerHandleConfig (L.pLoggerHandleConfig_ . T.pack) logger

-- -------------------------------------------------------------------------- --
-- Base Logger Backend

type BackendLogMessage a = Either (L.LogMessage T.Text) (L.LogMessage a)

-- | A fully generic backend that can be used to implement any backend including
-- base backends and backends that inspect internal log messages of the logging
-- system.
--
type GenericBackend a = BackendLogMessage SomeLogMessage -> IO a

-- | A backend that is used for partial log message handlers that may pass
-- messages on subsequent handlers.
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
        , "scope" .= L._logMsgScope a
        , "time" .= timeSpecMs (L._logMsgTime a)
        , "message" .= case L._logMsg a of (JsonLog msg) -> msg
        ]

timeSpecMs :: TimeSpec -> Double
timeSpecMs t = int (toNanoSecs t) / 1000000
{-# INLINE timeSpecMs #-}

-- | This logger produces one JSON value for each log message of type the given
-- type @a@. JSON values are separate by newline characters.
--
-- If a logfile is used, the file is opend in write mode and previous content
-- is deleted.
--
-- Note that the output of this logger as a whole doesn't represent a valid JSON
-- document.
--
withJsonFileHandleBackend
    :: ToJSON a
    => EnableConfig JsonLoggerConfig
    -> (JsonBackend a -> IO b)
    -> IO b
withJsonFileHandleBackend = configureHandler handler
  where
    handler c inner = case _jsonLoggerConfigHandle c of
        L.StdOut -> inner $ backend stdout
        L.StdErr -> inner $ backend stderr
        L.FileHandle f -> withFile f WriteMode $ \h -> inner $ backend h
    backend h = BL8.hPutStrLn h . encode . JsonLogMessage

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
    -> (Logger -> m α)
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
    -> (Logger -> IO α)
    -> IO α
withExampleLogger port config _sessionsConfig f = do
    staticDir <- (<> "/examples/static-html") <$> getDataDir
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

