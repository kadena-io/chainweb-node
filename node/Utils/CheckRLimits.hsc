{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language CApiFFI #-}

module Utils.CheckRLimits(checkRLimits) where

import Control.Exception
import Control.Monad
import Foreign hiding (void)
import Foreign.C.Error
import Foreign.C.Types
import GHC.Stack
import System.Exit
import System.IO

-- RLIMIT_NOFILE

#include <sys/resource.h>
#include <errno.h>
#include <sys/types.h>

-- k = #size rlimit

foreign import ccall "sys/resource.h getrlimit" 
    c_getrlimit :: CInt -> Ptr () -> IO CInt

foreign import ccall "sys/resource.h setrlimit" 
    c_setrlimit :: CInt -> Ptr () -> IO CInt

getOpenFileLimits :: HasCallStack => IO ((#type rlim_t), (#type rlim_t))
getOpenFileLimits = allocaBytes (#size struct rlimit) $ \ptr -> do
    err <- c_getrlimit (#const RLIMIT_NOFILE) ptr
    if err /= 0 then do
        Errno errno <- getErrno
        error $ "getOpenFileLimits: errno not equal to 0: " <> show errno
    else (,) <$> (#peek struct rlimit, rlim_cur) ptr <*> (#peek struct rlimit, rlim_max) ptr

setOpenFileLimits :: HasCallStack => ((#type rlim_t), (#type rlim_t)) -> IO ()
setOpenFileLimits (soft, hard) = allocaBytes (#size struct rlimit) $ \ptr -> do
    (#poke struct rlimit, rlim_cur) ptr soft
    (#poke struct rlimit, rlim_max) ptr hard
    err <- c_setrlimit (#const RLIMIT_NOFILE) ptr
    when (err /= 0) $ do
        Errno errno <- getErrno
        error $ "setOpenFileLimits: errno not equal to 0: " <> show errno

checkRLimits :: IO ()
checkRLimits = void $ try @IOException $ do
    (soft, hard) <- getOpenFileLimits
    when (hard < 32768) $ do
        hPutStrLn stderr $
            "This process is only able to open " <> show hard <> " file descriptors at once, "
            <> "which is not enough to run chainweb-node.\n" 
            <> "Set the open file limit higher than 32767 using the ulimit command or contact an administrator."
        exitFailure
    when (soft < 32768) $ 
        setOpenFileLimits (hard, hard)

