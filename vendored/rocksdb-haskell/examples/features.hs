{-# LANGUAGE OverloadedStrings #-}
-- |
-- Comprehensive walkthough of the functionality provided by this library.
--
module Main where

import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (release)
import Data.ByteString.Char8 hiding (take)
import Data.Default
import Prelude               hiding (putStrLn)

import Database.LevelDB


main :: IO ()
main = runResourceT $ do
    printVersion

    db <- open "/tmp/leveltest"
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }
    put db def "foo" "bar"
    get db def "foo" >>= liftIO . print
    delete db def "foo"
    get db def "foo" >>= liftIO . print

    (releaseSnap, snap) <- createSnapshot' db
    write db def{sync = True} [ Put "a" "one"
                              , Put "b" "two"
                              , Put "c" "three"
                              ]

    withIterator db def{useSnapshot = Just snap} $ \iter ->
      dumpEntries iter

    -- early release snapshot
    release releaseSnap

    -- here, we keep the iterator around for later reuse.
    -- Note that we don't explicitly release it (and thus don't keep the release
    -- key). The iterator will be released when runResourceT terminates.
    iter <- iterOpen db def
    dumpEntries iter

    approximateSize db ("a", "z") >>= liftIO . print

    getProperty db SSTables >>= printProperty "sstables"
    getProperty db Stats >>= printProperty "stats"
    getProperty db (NumFilesAtLevel 1) >>= printProperty "num files at level"

    write db def [ Del "a"
                 , Del "b"
                 , Del "c"
                 ]
    dumpEntries iter

    return ()

    where
        dumpEntries iter = do
            iterFirst iter
            iterItems iter >>= liftIO . print

        printProperty l p = liftIO $ do
            putStrLn l
            maybe (putStrLn "n/a") putStrLn $ p

        printVersion = do
            v <- versionBS
            liftIO . putStrLn $ "LevelDB Version: " `append` v

        versionBS = do
            (major, minor) <- version
            return $ intToBs major `append` "." `append` intToBs minor

        intToBs :: Int -> ByteString
        intToBs = pack . show
