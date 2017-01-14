{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default
import           System.Process               (system)
import           System.IO.Temp               (withSystemTempDirectory)
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck

import           Database.RocksDB

initializeDB :: MonadResource m => FilePath -> m DB
initializeDB path =
    open
        path
        defaultOptions
        {createIfMissing = True, compression = NoCompression}

main :: IO ()
main =  hspec $ do

  describe "Basic DB Functionality" $ do
    it "should put items into the database and retrieve them" $  do
      runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
        db <- initializeDB path

        put db def "zzz" "zzz"
        get db def "zzz"
      `shouldReturn` (Just "zzz")
