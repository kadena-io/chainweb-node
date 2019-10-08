{-# LANGUAGE CPP #-}

module Chainweb.Utils.Watchdog (withWatchdog, notifyReady) where

#ifdef WITH_SYSTEMD
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad
import System.Environment (lookupEnv)
import qualified System.Systemd.Daemon as SD

import Chainweb.Utils (tryAllSynchronous)

withWatchdog :: IO a -> IO a
withWatchdog act = do
    usec <- maybe defaultWatchdogUsec read <$> lookupEnv "WATCHDOG_USEC"
    Async.race act (watchdogTimer $! usec `div` 2) >>=
        either return impossible
  where
    impossible _ = fail "impossible: watchdog returned first"

    watchdogTimer :: Int -> IO ()
    watchdogTimer usec0 = forever $ do
        void $ tryAllSynchronous $ SD.notifyWatchdog
        threadDelay usec
      where
        usec = if usec0 <= 0 then defaultWatchdogUsec else usec0

defaultWatchdogUsec :: Int
defaultWatchdogUsec = 10000000   -- ten seconds

notifyReady :: IO ()
notifyReady = void SD.notifyReady
#else

withWatchdog :: IO a -> IO a
withWatchdog = id

notifyReady :: IO ()
notifyReady = return ()
#endif
