{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Utils.Logging.Trace
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Tools for tracing and logging the runtime of functions.
--
-- This should be used with care, since it adds overhead to the system. /It is
-- not meant to replace a profiling/. There is a reason why GHC uses a dedicated
-- runtime for profiling: performance.
--
-- The tools in this module are indented to measure and log the runtime of
-- long-running high-level operations in production.
--
module Utils.Logging.Trace
( trace
, trace'
, Trace
, eventStart
, eventEnd
, withCountedEvent
, withEvent
, eventStartOnChain
, eventEndOnChain
, withEventOnChain
) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.StopWatch

import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Tuple
import Debug.Trace (traceEventIO)

import GHC.Generics

import System.Clock
import System.IO.Unsafe
import System.LogLevel

-- internal modules

import Chainweb.ChainId
import Chainweb.Time

import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Logging

data Trace = Trace
    { _traceAction :: !T.Text
    , _traceParam :: !Value
    , _traceWeight :: !Int
    , _traceTime :: !Micros
    }
    deriving (Show, Eq, Generic, NFData)

traceProperties :: KeyValue e kv => Trace -> [kv]
traceProperties o =
    [ "action" .= _traceAction o
    , "param" .= _traceParam o
    , "weight" .= _traceWeight o
    , "time" .= _traceTime o
    ]
{-# INLINE traceProperties #-}

instance ToJSON Trace where
    toJSON = object . traceProperties
    toEncoding = pairs . mconcat . traceProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

eventCounters :: IORef (Map String Int)
eventCounters = unsafePerformIO $ newIORef Map.empty
{-# noinline eventCounters #-}

trace
    :: (MonadIO m)
    => ToJSON param
    => (LogLevel -> JsonLog Trace -> IO ())
    -> T.Text
    -> param
    -> Int
    -> m a
    -> m a
trace logg label param weight a =
    trace' logg label (const param) (const weight) a

trace'
    :: (MonadIO m)
    => ToJSON param
    => (LogLevel -> JsonLog Trace -> IO ())
    -> T.Text
    -> (a -> param)
    -> (a -> Int)
    -> m a
    -> m a
trace' logg label calcParam calcWeight a = do
    (!r, t) <- stopWatch a
    liftIO $ logg Info $ JsonLog $ Trace label
        (toJSON (calcParam r))
        (calcWeight r)
        (fromIntegral $ toNanoSecs t `div` 1000)
    return r

eventStart :: MonadIO m => String -> m ()
eventStart msg = liftIO $ traceEventIO ("START " ++ msg)
eventEnd :: MonadIO m => String -> m ()
eventEnd msg = liftIO $ traceEventIO ("STOP " ++ msg)

withCountedEvent :: (MonadMask m, MonadIO m) => String -> m a -> m a
withCountedEvent str act = do
    !ctr <- liftIO $ atomicModifyIORef'
        eventCounters
        (swap . Map.alterF (\(fromMaybe 0 -> r) -> (r, Just (r + 1))) str)
    withEvent (str <> " " <> show ctr) act

withEvent :: (MonadMask m, MonadIO m) => String -> m a -> m a
withEvent msg act = do
    liftIO $ eventStart msg
    act `finally`
        liftIO (eventEnd msg)

eventStartOnChain :: (HasChainId cid, MonadMask m, MonadIO m) => cid -> String -> m ()
eventStartOnChain cid msg = do
    let chainMsg = msg <> " (" <> T.unpack (chainIdToText $ _chainId cid) <> ")"
    liftIO $ eventStart chainMsg

eventEndOnChain :: (HasChainId cid, MonadMask m, MonadIO m) => cid -> String -> m ()
eventEndOnChain cid msg = do
    let chainMsg = msg <> " (" <> T.unpack (chainIdToText $ _chainId cid) <> ")"
    liftIO $ eventEnd chainMsg

withEventOnChain :: (HasChainId cid, MonadMask m, MonadIO m) => cid -> String -> m a -> m a
withEventOnChain cid msg act = do
    eventStartOnChain cid msg
    act `finally`
        liftIO (eventEndOnChain cid msg)
