{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
, passthroughLogHandler
, dropLogHandler
, logHandles

-- * Filter LogScope Backend
, Probability(..)
, propMult
, LogFilterRule(..)
, logFilterRuleLabel
, logFilterRuleLevel
, logFilterRuleRate
, LogFilter(..)
, logFilterRules
, logFilterDefaultLevel
, logFilterDefaultRate
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
import qualified Data.Aeson.Key as A
import Data.Bifunctor
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef
import Data.Proxy
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

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
import System.IO.Unsafe
import qualified System.Logger as L
import System.Logger.Backend.ColorOption
import qualified System.Logger.Internal as L
import System.LogLevel
import qualified System.Random.MWC as Prob
import qualified System.Random.MWC.Distributions as Prob

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

newtype Probability = Probability Double
    deriving (Generic)
    deriving newtype (Show, Read, Eq, Ord)

instance ToJSON Probability where
    toJSON (Probability p) = toJSON p
    toEncoding (Probability p) = toEncoding p
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Probability where
    parseJSON = withScientific "Probability" $ \n -> do
        unless (0 <= n && n <= 1) $ fail "probablility must be between 0 and 1"
        return (Probability $ realToFrac n)
    {-# INLINE parseJSON #-}

data LogFilterRule = LogFilterRule
    { _logFilterRuleLabel :: !L.LogLabel
    , _logFilterRuleLevel :: !L.LogLevel
    , _logFilterRuleRate :: !Probability
        -- ^ values that are small than zero are rounded up to 0. Values
        -- that are larger than 1 are rounded down to 1.
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogFilterRule

logFilterRuleProperties
    :: KeyValue e kv
    => LogFilterRule
    -> [kv]
logFilterRuleProperties r =
    [ "key" .= fst (_logFilterRuleLabel r)
    , "value" .= snd (_logFilterRuleLabel r)
    , "level" .= _logFilterRuleLevel r
    , "rate" .= _logFilterRuleRate r
    ]

instance ToJSON LogFilterRule where
    toJSON = object . logFilterRuleProperties
    toEncoding = pairs . mconcat . logFilterRuleProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON LogFilterRule where
    parseJSON = withObject "LogFilterRule" $ \o -> LogFilterRule
        <$> ((,) <$> o .: "key" <*> o .: "value")
        <*> o .: "level"
        <*> o .:? "rate" .!= Probability 1
    {-# INLINE parseJSON #-}

-- | A filter for log messages.
--
-- A log message passes a filter rule with probability
--
-- * 1, if the level of the message is smaller than the level of the rule,
-- * rate of the rule, the level of the message equals the level of the rule, and
-- * 0 if the level of the message is larger than the level of the rule.
--
-- If more than one rule matches, each rule is applied and the message passes
-- the filter only if it passes all applying rules.
--
-- If, after applying all rules, no rule matched any label of the log message,
-- the message is discarded if the log level of the message is larger than the
-- default level of the filter.
--
-- When a filter is specified more than once in a configuration, filters are
-- merged by concatenating the lists of rules (including default rules)
--
-- The default log filter has no rules and log level debug as default level
-- and 1 as default rate.
--
data LogFilter = LogFilter
    { _logFilterRules :: ![LogFilterRule]
    , _logFilterDefaultLevel :: !L.LogLevel
    , _logFilterDefaultRate :: !Probability
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''LogFilter

propMult :: Probability -> Probability -> Probability
propMult (Probability a) (Probability b) = Probability (a * b)

instance Semigroup LogFilter where
    a <> b = LogFilter
        { _logFilterRules = _logFilterRules a <> _logFilterRules b
        , _logFilterDefaultLevel = min (_logFilterDefaultLevel a) (_logFilterDefaultLevel b)
        , _logFilterDefaultRate =
            case compare (_logFilterDefaultLevel a) (_logFilterDefaultLevel b) of
                LT -> _logFilterDefaultRate a
                EQ -> propMult (_logFilterDefaultRate a) (_logFilterDefaultRate b)
                GT -> _logFilterDefaultRate b

        }
    {-# INLINE (<>) #-}

instance Monoid LogFilter where
    mempty = LogFilter
        { _logFilterRules = mempty
        , _logFilterDefaultLevel = maxBound
        , _logFilterDefaultRate = Probability 1
        }
    {-# INLINE mempty #-}

instance ToJSON LogFilter where
    toJSON a = object
        [ "rules" .= _logFilterRules a
        , "default" .=  _logFilterDefaultLevel a
        , "default-rate" .=  _logFilterDefaultRate a
        ]
    {-# INLINE toJSON #-}

instance FromJSON LogFilter where
    parseJSON = withObject "LogFilter" $ \o -> LogFilter
        <$> o .: "rules"
        <*> o .: "default"
        <*> o .:? "default-rate" .!= Probability 1
    {-# INLINE parseJSON #-}

-- | Global RNG for use in filter rules.
--
logRuleRng :: IORef Prob.Seed
logRuleRng = unsafePerformIO (Prob.createSystemSeed >>= newIORef)
{-# NOINLINE logRuleRng #-}
-- The NOINLINE pragma ensures that the argument to unsafePerformIO is evaluated
-- only once.

logRuleToss :: Probability -> IO Bool
logRuleToss (Probability p) = do
    -- This is isn't thread-safe in a conventional way at all. But
    -- we don't care about non-deterministic results due to races as
    -- long as the results are sufficiently random on each individual thread.
    -- Results can, but don't have to, correlate between threads.
    rng <- Prob.restore =<< readIORef logRuleRng
    r <- Prob.bernoulli p rng
    Prob.save rng >>= atomicWriteIORef logRuleRng
    return r
{-# INLINE logRuleToss #-}

applyRule :: LogFilterRule -> L.LogMessage a -> IO (Maybe All)
applyRule r m = mconcat <$> mapM applyToLabel scopes
  where
    scopes :: [L.LogLabel]
    scopes = L._logMsgScope m

    applyToLabel :: L.LogLabel -> IO (Maybe All)
    applyToLabel s
        | s /= _logFilterRuleLabel r = return Nothing
        | otherwise = Just . All <$> applyRuleToLevel
            (_logFilterRuleLevel r)
            (_logFilterRuleRate r)
            (L._logMsgLevel m)

applyRuleToLevel :: L.LogLevel -> Probability -> L.LogLevel -> IO Bool
applyRuleToLevel rl rp l = case compare l rl of
    LT -> return  True
    GT -> return  False
    -- We may instead just hash the message + timestamp + salt to make this pure
    EQ -> logRuleToss rp

-- | A handle that applies a log filter.
--
-- The filter is applied after log messages have been emitted according to the
-- log level of the respective logger.
--
-- This could be optimized for the number of rules or the number of scopes
-- in a message. Currently it is optimized for the. The current implementation
-- make one lookup per scope.
--
logFilterHandle :: LogFilter -> LogHandler
logFilterHandle sf = maybeLogHandler $ \msg -> do
    foldMap (\r -> applyRule r msg) (_logFilterRules sf) >>= \case
        Just (All True) -> return (Just msg)
        Just (All False) -> return Nothing
        Nothing -> do
            x <- applyRuleToLevel
                (_logFilterDefaultLevel sf)
                (_logFilterDefaultRate sf)
                (L._logMsgLevel msg)
            if x then return (Just msg) else return Nothing
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

passthroughLogHandler :: LogHandler
passthroughLogHandler = maybeLogHandler (return . Just)

dropLogHandler :: forall a. LogMessage a => Proxy a -> LogHandler
dropLogHandler _ = LogHandler $ h @a
    where
    h :: forall m. LogMessage m => BackendLogMessage m -> IO (Maybe (BackendLogMessage SomeLogMessage))
    h (Left msg) = Just . Left <$!> return msg
    h (Right _) = return Nothing

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
configureHandler logger config inner
    | _enableConfigEnabled config = logger (_enableConfigConfig config) inner
    | otherwise = inner (const $ return ())

-- -------------------------------------------------------------------------- --
-- JSON Encoding for Log Messages

-- | Format a Log Message as JSON.
--
newtype JsonLogMessage a = JsonLogMessage (L.LogMessage a)
    deriving (Generic)

instance ToJSON a => ToJSON (JsonLogMessage a) where
    toEncoding = pairs . mconcat . jsonLogMessageEncoding
    toJSON = object . jsonLogMessageEncoding
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

jsonLogMessageEncoding :: KeyValue e kv => ToJSON a => JsonLogMessage a -> [kv]
jsonLogMessageEncoding (JsonLogMessage a) =
    [ "level" .= L._logMsgLevel a
    , "scope" .= scopeToJson (L._logMsgScope a)
    , "time" .= L.formatIso8601Milli @T.Text (L._logMsgTime a)
    , "message" .= L._logMsg a
    ]
  where
    scopeToJson = object . map (uncurry (.=) . first A.fromText) . reverse
{-# INLINE jsonLogMessageEncoding #-}

-- | Format a Log Message for Usage in Elasticsearch DataStreams
--
newtype EsJsonLogMessage a = EsJsonLogMessage
    { _getEsJsonLogMessage :: L.LogMessage a }
    deriving (Generic)

instance ToJSON a => ToJSON (EsJsonLogMessage a) where
    toEncoding = pairs . mconcat . esJsonLogMessageEncoding
    toJSON = object . esJsonLogMessageEncoding
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

esJsonLogMessageEncoding :: KeyValue e kv => ToJSON a => EsJsonLogMessage a -> [kv]
esJsonLogMessageEncoding (EsJsonLogMessage a) =
    [ "level" .= L._logMsgLevel a
    , "scope" .= scopeToJson (L._logMsgScope a)
    , "@timestamp" .= L.formatIso8601Milli @T.Text (L._logMsgTime a)
    , "message" .= L._logMsg a
    ]
  where
    scopeToJson = object . map (uncurry (.=) . first A.fromText) . reverse
{-# INLINE esJsonLogMessageEncoding #-}

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
withBaseHandleBackend llabel mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode fdBackend
    ElasticSearch f auth ->
        withElasticsearchBackend mgr f auth (T.toLower llabel) pkgScopes esBackend
  where
    addTypeScope = L.logMsgScope %~ (:) ("type", llabel)

    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ logText h colored . bimap addTypeScope addTypeScope
        LogFormatJson -> inner $ \case
            Right msg ->
                BL8.hPutStrLn h $ encode $ JsonLogMessage $ logText <$> addTypeScope msg
            Left msg -> do
                unless (h == stderr) $ errFallback msg
                BL8.hPutStrLn h $ encode $ JsonLogMessage $ addTypeScope msg

    esBackend b = inner $ \case
        Right msg -> b $ logText <$> addTypeScope msg
        Left msg -> errFallback msg >> b (addTypeScope msg)

    errFallback msg = do
        colored <- useColor (_backendConfigColor c) stderr
        L.handleBackend_ id stderr colored (Left $ addTypeScope msg)

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
    -> (Backend (JsonLog a) -> IO b)
    -> IO b
withJsonHandleBackend llabel mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode fdBackend
    ElasticSearch f auth -> withElasticsearchBackend mgr f auth (T.toLower llabel) pkgScopes $ \b ->
        inner (b . fmap unJsonLog . addTypeScope)
  where
    addTypeScope = L.logMsgScope %~ (:) ("type", llabel)
    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ (encodeToText . unJsonLog) h colored . Right . addTypeScope
        LogFormatJson -> inner $
            BL8.hPutStrLn h . encode . JsonLogMessage . fmap unJsonLog . addTypeScope
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
withTextHandleBackend llabel mgr pkgScopes c inner = case _backendConfigHandle c of
    StdOut -> fdBackend stdout
    StdErr -> fdBackend stderr
    FileHandle f -> withFile f WriteMode $ \h -> fdBackend h
    ElasticSearch f auth -> withElasticsearchBackend mgr f auth (T.toLower llabel) pkgScopes $ \b ->
        inner (b . fmap logText . addTypeScope)
  where
    addTypeScope = L.logMsgScope %~ (:) ("type", llabel)

    fdBackend h = case _backendConfigFormat c of
        LogFormatText -> do
            colored <- useColor (_backendConfigColor c) h
            inner $ L.handleBackend_ logText h colored . Right . addTypeScope
        LogFormatJson -> inner $
            BL8.hPutStrLn h . encode . JsonLogMessage . fmap logText . addTypeScope
{-# INLINEABLE withTextHandleBackend #-}

-- TODO: it may be more useful to have a logger that logs all 'Right' messages

-- -------------------------------------------------------------------------- --
-- Elasticsearch Backend

elasticSearchBatchSize :: Natural
elasticSearchBatchSize = 1000

elasticSearchBatchDelayMs :: Natural
elasticSearchBatchDelayMs = 1000

-- | A backend for JSON log messags that sends all logs to the given index of an
-- Elasticsearch server. Messages are sent in a fire-and-forget fashion. If a
-- connection fails, the messages are dropped without notice.
--
-- This backend uses Elasticsearch Data streams. It requires Elasticsearch-7.8
-- or later. In order to use it one must create an index template for data
-- streams for the prefix @chainweb-*@.
--
-- Elasticsearch will create the datastream from a matching index template if it
-- doesn't exist.
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
    -> Maybe T.Text
        -- ^ optional API Key
    -> T.Text
        -- ^ Index Name
    -> [(T.Text, T.Text)]
        -- ^ Scope that are included only with remote backends. In chainweb-node
        -- this is used for package info data.
    -> (Backend a -> IO b)
    -> IO b
withElasticsearchBackend mgr esServer key ixName pkgScopes inner = do
    req <- HTTP.parseUrlThrow (T.unpack esServer) >>= \x -> case key of
        Nothing -> return x
        Just k -> return x
            { HTTP.requestHeaders = ("Authorization", "ApiKey " <> T.encodeUtf8 k) : HTTP.requestHeaders x
            }

    -- New versions of Elasticsearch (>= 7.8) will create the datastream from
    -- a matching index template if it doesn't exist.
    --
    -- FIXME for this to work we'll have to figure out the correct set of user priveledges
    -- for checking the existence of data streams.
    --
    -- _createDataStream req

    queue <- newTBQueueIO 2000
    withAsync (runForever errorLogFun "Utils.Logging.withElasticsearchBackend" (processor req queue)) $ \_ -> do
        inner $ \a -> atomically (writeTBQueue queue a)

  where
    streamName = "chainweb-" <> ixName
    errorLogFun Error msg = T.hPutStrLn stderr msg
    errorLogFun _ _ = return ()

    -- Collect messages. If there is at least one pending message, submit a
    -- `_bulk` request every second or when the batch size is 1000 messages,
    -- whatever happens first.
    --
    processor req queue = do
        -- ensure that there is at least one transaction in every batch
        h <- atomically $ readTBQueue queue

        -- set timer to 1 second
        timer <- registerDelay (int elasticSearchBatchDelayMs)

        -- Fill the batch
        (remaining, batch) <- go elasticSearchBatchSize (indexAction h) timer

        errorLogFun Info $ "send " <> sshow (elasticSearchBatchSize - remaining) <> " messages"
        void $ HTTP.httpLbs (putBulgLog req batch) mgr
      where
        getNextAction timer = atomically $ isTimeout `orElse` fill
          where
            isTimeout = Nothing <$ (readTVar timer >>= check)
            fill = tryReadTBQueue queue >>= maybe retry (return . Just)

        go 0 !batch _ = return (0, batch)
        go !remaining !batch !timer = getNextAction timer >>= \case
            Nothing -> return (remaining, batch)
            Just x -> go (remaining - 1) (batch <> indexAction x) timer

    -- currently unused. We keep it in case we figure out how to configure users with minimal
    -- rights to check for the existence of a data stream
    --
    _createDataStream req =
        void $ HTTP.httpLbs (putDataStream req) { HTTP.method = "HEAD"} mgr
            `catch` \case
                (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException _ _)) -> do
                    errorLogFun Error $ "Datastream doesn't exist " <> streamName <> ". Trying to create it ..."
                    HTTP.httpLbs (putDataStream req) mgr
                ex -> throwM ex

    putDataStream req = req
        { HTTP.method = "PUT"
        , HTTP.path = HTTP.path req <> "/_data_stream/" <> T.encodeUtf8 streamName
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
        , HTTP.requestHeaders = ("content-type", "application/json") : HTTP.requestHeaders req
        }

    putBulgLog req a = req
        { HTTP.method = "POST"
        , HTTP.path = HTTP.path req <> T.encodeUtf8 streamName <> "/_bulk"
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
        , HTTP.requestHeaders = ("content-type", "application/x-ndjson") : HTTP.requestHeaders req
        , HTTP.requestBody = HTTP.RequestBodyLBS $ BB.toLazyByteString a
        }

    e = fromEncoding . toEncoding
    indexAction a
        = fromEncoding indexActionHeader
        <> BB.char7 '\n'
        <> e (EsJsonLogMessage $ L.logMsgScope <>~ pkgScopes $ a)
        <> BB.char7 '\n'

    indexActionHeader :: Encoding
    indexActionHeader = pairs $ pair "create" $ pairs mempty

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
    -> (Backend (JsonLog a) -> IO b)
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
        . fmap unJsonLog
    app c = mapUrls
        $ mount "frontendapp" (staticApp $ defaultWebAppSettings staticDir)
        <|> mount "frontend" (staticApp $ defaultFileServerSettings staticDir)
        <|> mount "events" (loggingCors $ eventSourceAppChan c)
        -- <|> mountRoot (loggingCors $ eventSourceAppChan c)

-- Simple cors with actually simpleHeaders which includes content-type.
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
        withJsonEventSourceAppBackend @P2pSessionInfo port staticDir $ \sessionsBackend -> do
            let loggerBackend = logHandles
                    [ logHandler sessionsBackend ]
                    baseBackend
                        -- The type system enforces that backend is a base logger.
            L.withLogger loggerConfig loggerBackend f
