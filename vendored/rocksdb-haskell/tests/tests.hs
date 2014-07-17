{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default
import           System.Process               (system)
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck

import           Database.RocksDB

initializeDB :: MonadResource m => m DB
initializeDB =
    open
        testDBPath
        defaultOptions
        {createIfMissing = True, compression = NoCompression}

main :: IO ()
main =  hspec $ do
  it "cleanup" $ cleanup >>= shouldReturn (return())

  describe "Basic DB Functionality" $ do
    it "should put items into the database and retrieve them" $  do
      runResourceT $ do
        db <- initializeDB

        put db def "zzz" "zzz"
        get db def "zzz"
      `shouldReturn` (Just "zzz")

testDBPath :: String
testDBPath = "/tmp/haskell-rocksdb-tests"

cleanup :: IO ()
cleanup = system ("rm -fr " ++ testDBPath) >> return ()
