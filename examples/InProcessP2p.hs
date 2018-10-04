{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Monoid
import Data.String
import qualified Data.Text as T

import Numeric.Natural


import System.Logger hiding (logg)
import qualified System.LogLevel as L
import System.Random

-- internal modules

import P2P.Connection hiding (async)
import P2P.Node

-- | Initialize Logger and call example
--
main :: IO ()
main = withHandleBackend (_logConfigBackend config)
    $ \backend -> withLogger (_logConfigLogger config) backend
    $ p2pNetwork 4 6 20
 where
    level = Debug
    config = defaultLogConfig
        & logConfigLogger . loggerConfigThreshold .~ level

-- | Run Test P2P Network
--
p2pNetwork
    :: Natural
        -- ^ targeted session count
    -> Natural
        -- ^ max session count (must be larger than targeted session count)
    -> Natural
        -- ^ message buffer size
    -> Logger T.Text
    -> IO ()
p2pNetwork sessionCount maxSessionCount msgBufferSize logger = bracket
    (mapM runNode [0..5])
    (mapM uninterruptibleCancel)
    (void . waitAnyCancel)
  where

    -- the log function that is given to the p2pNode implementation uses log-level Debug
    --
    runNode nid = withLoggerLabel ("instance", sshow nid) logger $ \logger' -> do
        let logfun level = liftIO . loggerFunIO logger' (l2l level)
        let config = P2pConfiguration sessionCount maxSessionCount msgBufferSize logfun
        async $ p2pNode config (session logfun nid)

-- | Example Session
--
session
    :: MonadCatch m
    => MonadIO m
    => (L.LogLevel -> T.Text -> m ())
    -> Int
    -> P2pConnection m
    -> m ()
session logg (nid :: Int) c = go
    `catch` \case
        e@P2pConnectionClosed{} -> logg L.Warn (sshow e) >> p2pClose c
        e@P2pConnectionFailed{} -> logg L.Error (sshow e) >> p2pClose c
  where
    go = do
        let sendMsg = ["a", "b", "c"]
        p2pSend c $ "from " <> sshow nid <> ": " : sendMsg
        logg L.Debug $ "sent: " <> sshow (mconcat sendMsg)
        d <- liftIO $ randomRIO (0,5000000)
        liftIO $ threadDelay d
        msg <- p2pReceive c
        logg L.Debug $ "received: " <> sshow (mconcat msg)
        p2pClose c

-- -------------------------------------------------------------------------- --
-- Utils

sshow :: Show a => IsString b => a -> b
sshow = fromString . show

l2l :: L.LogLevel -> LogLevel
l2l L.Quiet = Quiet
l2l L.Error = Error
l2l L.Warn = Warn
l2l L.Info = Info
l2l L.Debug = Debug
l2l (L.Other _) = Debug

