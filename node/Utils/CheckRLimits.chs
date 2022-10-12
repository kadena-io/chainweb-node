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

#include <sys/resource.h>
#include <errno.h>
#include <sys/types.h>

type RLIM_T = {#type rlim_t#}

{#enum define Resource {RLIMIT_NOFILE as NumberOpenFiles}#}

foreign import ccall "sys/resource.h getrlimit" 
    c_getrlimit :: CInt -> Ptr () -> IO CInt

foreign import ccall "sys/resource.h setrlimit" 
    c_setrlimit :: CInt -> Ptr () -> IO CInt

getOpenFileLimits :: HasCallStack => IO (RLIM_T, RLIM_T)
getOpenFileLimits = allocaBytes {#sizeof rlimit#} $ \rlimit -> do
    err <- c_getrlimit (fromIntegral $ fromEnum NumberOpenFiles) rlimit
    if err /= 0 then do
        Errno errno <- getErrno
        error $ "getOpenFileLimits: errno not equal to 0: " <> show errno
    else (,) <$> {#get rlimit->rlim_cur#} rlimit <*> {#get rlimit->rlim_max#} rlimit

setOpenFileLimits :: HasCallStack => (RLIM_T, RLIM_T) -> IO ()
setOpenFileLimits (soft, hard) = allocaBytes {#sizeof rlimit#} $ \rlimit -> do
    {#set rlimit.rlim_cur#} rlimit soft
    {#set rlimit.rlim_max#} rlimit hard
    err <- c_setrlimit (fromIntegral $ fromEnum NumberOpenFiles) rlimit
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

