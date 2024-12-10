{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.StopWatch

import Data.Aeson
import qualified Data.Text as T

import GHC.Generics

import System.Clock
import System.LogLevel

-- internal modules

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

trace
    :: MonadIO m
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
    :: MonadIO m
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
