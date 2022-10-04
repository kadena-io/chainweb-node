{-# language ScopedTypeVariables #-}

module Utils.CheckRLimits(checkRLimits) where

import Control.Monad
import Data.Char
import Data.List(stripPrefix)
import System.Exit
import System.IO
import System.Posix.Process (getProcessID)
import Text.Read

checkRLimits :: IO ()
checkRLimits = do
    pid <- getProcessID
    limits <- readFile $ "/proc/" <> show pid <> "/limits"
    let
        -- the format of /proc/<pid>/limits looks like
        -- Something      its soft limit     its hard limit    the units of the limit
        -- for example
        -- Max open files     1024        1024       files
        -- we only care about the soft limit, which is the real limit for the process.
        openFileLimit =
            [ n
            | l <- lines limits
            , Just rest <- [stripPrefix "Max open files" l]
            , Just (n :: Int) <- [readMaybe (takeWhile isDigit $ dropWhile isSpace rest)]
            ]
    case openFileLimit of
        [n] ->
            when (n < 32768) $ do
                hPutStrLn stderr $
                    "This process is only able to open " <> show n <> " file descriptors at once, "
                    <> "which is not enough to run chainweb-node.\n" <>
                    "Set the limit higher than 32767 using the ulimit command or contact an administrator."
                exitFailure
        -- this check is conservative to avoid crashing nodes that don't deserve it
        _ ->
            putStrLn "Couldn't find out the open file descriptor limit on your system"
