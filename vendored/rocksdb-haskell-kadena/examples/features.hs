{-# LANGUAGE OverloadedStrings #-}
-- |
-- Comprehensive walkthough of the functionality provided by this library.
--
module Main where

import Control.Monad
import Data.ByteString.Char8 hiding (take)
import Prelude hiding (putStrLn)
import System.FilePath

import Database.LevelDB


main :: IO ()
main = withLevelDB dbdir [ CreateIfMissing, CacheSize 2048 ] $ \db -> do
         put db [] "foo" "bar"
         get db [ FillCache ] "foo" >>= print
         delete db [] "foo"
         get db [ FillCache ] "foo" >>= print

         withSnapshot db $ \snap -> do
             write db [ Sync ] [ Put "a" "one"
                               , Put "b" "two"
                               , Put "c" "three" ]
             dumpEntries db [ UseSnapshot snap, FillCache ]
             dumpEntries db [ FillCache ]

         approximateSize db ("a", "z") >>= print
         getProperty db SSTables >>= printProperty "sstables"
         getProperty db Stats    >>= printProperty "stats"
         getProperty db (NumFilesAtLevel 1) >>= printProperty "num files at level"

         write db [ Sync ] [ Del "a", Del "b", Del "c" ]
         dumpEntries db [ FillCache ]

    where
         dbdir = "/" </> "tmp" </> "leveltest"

         dumpEntries db opts =
             withIterator db opts $ \iter -> do
                 iterFirst iter
                 iterItems iter >>= print

         printProperty l p = do
             putStrLn l
             maybe (putStrLn "n/a") putStrLn $ p
