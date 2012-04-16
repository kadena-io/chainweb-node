{-# LANGUAGE OverloadedStrings #-}
-- |
-- Comprehensive walkthough of the functionality provided by this library.
--
module Main where

import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Data.ByteString.Char8 hiding (take)
import Prelude               hiding (putStrLn)
import System.FilePath

import Database.LevelDB


main :: IO ()
main = runLevelDB $ do
    db <- open dbdir [CreateIfMissing, CacheSize 2048]
    put db [] "foo" "bar"
    get db [FillCache] "foo" >>= liftIO . print
    delete db [] "foo"
    get db [FillCache] "foo" >>= liftIO . print

    (releaseSnap, snap) <- createSnapshot' db
    write db [Sync] [ Put "a" "one"
                    , Put "b" "two"
                    , Put "c" "three"
                    ]

    withIterator db [UseSnapshot snap, FillCache] $ \iter ->
      dumpEntries iter

    -- early release snapshot
    release releaseSnap

    -- here, we keep the iterator around for later reuse.
    -- Note that we don't explicitly release it (and thus don't keep the release
    -- key). The iterator will be released when runLevelDB terminates.
    iter <- iterOpen db [FillCache]
    dumpEntries iter

    approximateSize db ("a", "z") >>= liftIO . print

    getProperty db SSTables >>= printProperty "sstables"
    getProperty db Stats >>= printProperty "stats"
    getProperty db (NumFilesAtLevel 1) >>= printProperty "num files at level"

    write db [Sync] [ Del "a"
                    , Del "b"
                    , Del "c"
                    ]
    dumpEntries iter

    return ()

    where
        dbdir = "/" </> "tmp" </> "leveltest"

        dumpEntries iter = do
            iterFirst iter
            iterItems iter >>= liftIO . print

        printProperty l p = liftIO $ do
            putStrLn l
            maybe (putStrLn "n/a") putStrLn $ p
