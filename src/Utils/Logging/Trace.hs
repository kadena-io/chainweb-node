{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
-- TODO
--
module Utils.Logging.Trace
( trace
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

instance ToJSON Trace where
    toJSON o = object
        [ "action" .= _traceAction o
        , "param" .= _traceParam o
        , "weight" .= _traceWeight o
        , "time" .= _traceTime o
        ]

trace
    :: MonadIO m
    => ToJSON param
    => LogFunction
    -> T.Text
    -> param
    -> Int
    -> m a
    -> m a
trace logg label param weight a = do
    (!r, t) <- stopWatch a
    liftIO $ logg Info $ JsonLog $ Trace label
        (toJSON param)
        weight
        (fromIntegral $ toNanoSecs t `div` 1000)
    return r
