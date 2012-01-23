{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.ByteString.Char8 hiding (take)
import Prelude hiding (putStrLn)
import System.FilePath

import Database.LevelDB

import Debug.Trace


main :: IO ()
main =
    withLevelDB dbdir [ CreateIfMissing, CacheSize 2048 ] $ \db -> do
        trace "put / delete:" $ do
            trace ("put") put db [] "foo" "bar"
            trace ("get") get db [ FillCache ] "foo" >>= print
            trace ("delete") delete db [] "foo"
            trace ("get") get db [ FillCache ] "foo" >>= print

        trace "write batch:" $ withSnapshot db $ \snap -> do
            write db [ Sync ] [ Put "a" "one"
                              , Put "b" "two"
                              , Put "c" "three" ]
            trace "snapshot read: " $ dumpEntries db [ UseSnapshot snap, FillCache ]
            trace "live read: "     $ dumpEntries db [ FillCache ]

        trace "inspect sizes:" $
            approximateSize db ("a", "z") >>= print

        trace "inspect properties:" $ do
            getProperty db SSTables >>= printProperty "sstables"
            getProperty db Stats    >>= printProperty "stats"
            getProperty db (NumFilesAtLevel 1) >>= printProperty "num files at level"

        trace "delete batch:" $ do
            trace ("write") write db [] [ Del "a", Del "b", Del "c" ]
            trace ("dump") dumpEntries db [ FillCache ]

    where
        dbdir = "/" </> "tmp" </> "leveltest"

        dumpEntries db opts =
            withIterator db opts $ \iter -> do
                trace ("iterFirst") iterFirst iter
                iterEntries iter print

        iterEntries iter f = do
            valid <- trace ("iterValid") iterValid iter
            when valid $ do
                key <- trace ("iterKey") iterKey iter
                val <- trace ("iterValue") iterValue iter
                _   <- f (key, val)
                _   <- trace ("iterNext") iterNext iter
                iterEntries iter f

        printProperty l p = do
            putStrLn l
            maybe (putStrLn "n/a") putStrLn $ p
