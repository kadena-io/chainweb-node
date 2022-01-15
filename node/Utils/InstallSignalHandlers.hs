{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Utils.InstallSignalHandlers
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Install Signal Handlers for SIGTERM and other signals that cause an "normal"
-- termination of the service.
--
-- The implementation of 'installFatalSignalHandlers' is copied from
-- <https://ro-che.info/articles/2014-07-30-bracket>.
--
-- The windows (mingw32_HOST_OS) implementation of install Handler is an
-- adaption of <https://github.com/pmlodawski/signal>, which is copyright of
-- Copyright (c) 2015 Piotr Mlodawski.
--
--
module Utils.InstallSignalHandlers
( installFatalSignalHandlers
, installHandlerCross
, sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ 
) where

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad

import Foreign.C.Types

import GHC.Generics

import System.Mem.Weak

#if !mingw32_HOST_OS
import System.Posix.Signals
#else
import Foreign.Ptr
#endif

-- -------------------------------------------------------------------------- --
-- Windows (mingw) implementatin of 'installHandler'

#if mingw32_HOST_OS
-- The windows (mingw32_HOST_OS) implementation of install Handler is an
-- adaption of <https://github.com/pmlodawski/signal>, which is copyright of
-- Copyright (c) 2015 Piotr Mlodawski.

type Signal = CInt

sigHUP, sigTERM, sigUSR1, sigUSR2, sigXCPU, sigXFSZ :: Signal
sigHUP = 1
sigTERM = 15
sigUSR1 = 16
sigUSR2 = 17
sigXCPU = 30
sigXFSZ = 31

type Handler = Signal -> IO ()

foreign import ccall "wrapper"
    genHandler :: Handler -> IO (FunPtr Handler)

foreign import ccall safe "signal.h signal"
    install :: Signal -> FunPtr Handler -> IO Signal

installHandler :: Signal -> Handler -> IO ()
installHandler signal handler = do
    result <- install signal =<< genHandler handler
    return $ assert (result == 0) ()
#endif

-- -------------------------------------------------------------------------- --
-- Install Signal Handlers

newtype SignalException = SignalException Signal deriving (Show, Eq, Generic)
instance Exception SignalException

installHandlerCross :: Signal -> (Signal -> IO ()) -> IO ()
installHandlerCross s h = 
#ifdef mingw32_HOST_OS
    installHandler s h
#else
    void $ installHandler s (Catch (h s)) Nothing
#endif

-- | Handle SIGTERM (and other signals) that are supposed to terminate the
-- program. By default GHCs RTS only installs a handler for SIGINT (Ctrl-C).
-- This function install handlers that that raise an exception on the main
-- thread when a signal is received. This causes the execution of finalization
-- logic in brackets.
--
-- This is particularly important for the SQLite, because it resets the WAL
-- files. Otherwise the files would never be deallocated and would remain at
-- their maximum size forever. Graceful shutdown will also result in better
-- logging of issues and prevent data corruption or missing data in database
-- backends.
--
-- The implementation is copied fom
-- <https://ro-che.info/articles/2014-07-30-bracket>, which also explains
-- details.
--
-- This asssumes that threads are managed properly. Threads that are spawned by
-- just calling forkIO, won't be notified and terminate without executing
-- termination logic.
--
installFatalSignalHandlers :: [Signal] -> IO ()
installFatalSignalHandlers signals = do
  main_thread_id <- myThreadId
  weak_tid <- mkWeakThreadId main_thread_id
  forM_ signals $ \sig ->
    installHandlerCross sig (send_exception weak_tid)
  where
    send_exception weak_tid sig = do
      m <- deRefWeak weak_tid
      case m of
        Nothing  -> return ()
        Just tid -> throwTo tid (toException $ SignalException sig)

