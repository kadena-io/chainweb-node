{-# LANGUAGE OverloadedStrings #-}

-- | Demo filter policy / bloom filter

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Default

import Database.LevelDB


main :: IO ()
main = runResourceT $ do
    bloom <- bloomFilter 10
    db <- open "/tmp/lvlbloomtest"
               defaultOptions { createIfMissing = True
                              , filterPolicy    = Just . Left $ bloom
                              }

    put db def "zzz" "zzz"
    put db def "yyy" "yyy"
    put db def "xxx" "xxx"

    get db def "yyy" >>= liftIO . print
    get db def "xxx" >>= liftIO . print

    return ()
