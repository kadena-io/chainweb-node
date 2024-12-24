{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language CApiFFI #-}

module Utils.CheckRLimits(checkRLimits) where

import Control.Exception
import Control.Monad
import Foreign hiding (void)
import Foreign.C.Error
import Foreign.C.Types
import System.Exit
import System.IO

data UInt64Pair
foreign import ccall "rlim_utils.c get_open_file_limits"
    c_getOpenFileLimits :: Ptr UInt64Pair -> IO CInt
foreign import ccall "rlim_utils.c set_open_file_limits"
    c_setOpenFileLimits :: Ptr UInt64Pair -> IO CInt

getOpenFileLimits :: IO (Word64, Word64)
getOpenFileLimits = allocaBytes (8 * 2) $ \pairPtr -> do
    err <- c_getOpenFileLimits (castPtr pairPtr)
    if err /= 0 then do
        Errno errno <- getErrno
        error $ "getOpenFileLimits: errno not equal to 0: " <> show errno
    else (,) <$> peek pairPtr <*> peek (pairPtr `plusPtr` 8)

setOpenFileLimits :: (Word64, Word64) -> IO ()
setOpenFileLimits (soft, hard) = allocaBytes (8 * 2) $ \pairPtr -> do
    poke pairPtr soft
    poke (pairPtr `plusPtr` 8) hard
    err <- c_setOpenFileLimits (castPtr pairPtr)
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
    when (soft < 32768) $ do
        setOpenFileLimits (hard, hard)
        (soft', hard') <- getOpenFileLimits
        when ((soft', hard') /= (hard, hard)) $
            hPutStrLn stderr $
                "Failed to set open file limit. This is an internal error. Continuing."
