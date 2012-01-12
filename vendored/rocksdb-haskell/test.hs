{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.ByteString.Char8 hiding (take)
import Prelude hiding (putStrLn)
import System.FilePath

import Database.LevelDB

import Debug.Trace


main :: IO ()
main = do
    withLevelDB dbdir comparator opts $ \db -> do
        trace "put / delete:" $ do
            trace ("put") put db wopts "foo" "bar"
            trace ("get") get db ropts "foo" >>= print
            trace ("delete") delete db wopts "foo"
            trace ("get") get db ropts "foo" >>= print

        trace "write batch:" $ withSnapshot db $ \snap -> do
            write db [ Sync ] [ Put "a" "one"
                              , Put "b" "two"
                              , Put "c" "three" ]
            trace "snapshot read: " $ dumpEntries db [ UseSnapshot snap ]
            trace "live read: "     $ dumpEntries db ropts

        trace "inspect sizes:" $
            approximateSize db ("a", "z") >>= print

        trace "inspect properties:" $ do
            getProperty db SSTables >>= printProperty
            getProperty db Stats    >>= printProperty
            getProperty db (NumFilesAtLevel 1) >>= printProperty

        trace "delete batch:" $ do
            trace ("write") write db wopts [ Del "a", Del "b", Del "c" ]
            trace ("dump") dumpEntries db ropts

    trace "destroy database" $ destroy dbdir comparator opts

    where
        dbdir = "/" </> "tmp" </> "leveltest"

        opts  = [ CreateIfMissing, UseCache 1024 ]
        wopts = []
        ropts = [ FillCache ]

        comparator = Comparator compare

        dumpEntries db ropts =
            withIterator db ropts $ \iter -> do
                iterFirst iter
                iterEntries iter print

        iterEntries iter f = do
            valid <- iterValid iter
            when valid $ do
                key <- iterKey iter
                val <- iterValue iter
                _   <- f (key, val)
                _   <- iterNext iter
                iterEntries iter f

        printProperty = maybe (putStrLn "n/a") putStrLn
