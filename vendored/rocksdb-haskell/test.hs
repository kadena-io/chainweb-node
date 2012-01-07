{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.ByteString.Char8 hiding (take)

import Database.LevelDB

import Debug.Trace


main :: IO ()
main = do
    withLevelDB dbdir opts $ \db -> do
        trace "put / delete:" $ do
            put db wopts "foo" "bar"
            get db ropts "foo" >>= print
            delete db wopts "foo"
            get db ropts "foo" >>= print

        trace "write batch:" $ do
            write db wopts [ Put "a" "one"
                           , Put "b" "two"
                           , Put "c" "three" ]
            dumpEntries db ropts

        trace "delete batch:" $ do
            write db wopts [ Del "a", Del "b", Del "c" ]
            dumpEntries db ropts

    trace "destroy database" $ destroy dbdir opts

    where
        dbdir = "/tmp/leveltest"

        opts  = [ CreateIfMissing
                , UseComparator "lexicographic" compare ]
        wopts = defaultWriteOptions
        ropts = defaultReadOptions

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
