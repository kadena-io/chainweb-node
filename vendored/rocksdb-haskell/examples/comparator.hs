{-# LANGUAGE OverloadedStrings #-}

-- | Demo custom comparator

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8  (take)
import Data.Default

import Database.LevelDB


customComparator :: Comparator
customComparator = Comparator compare

main :: IO ()
main = runResourceT $ do
    db <- open "/tmp/lvlcmptest"
               defaultOptions{ createIfMissing = True
                             , comparator = Just customComparator
                             }

    put db def "zzz" ""
    put db def "yyy" ""
    put db def "xxx" ""

    withIterator db def $ \iter -> do
        iterFirst iter
        iterItems iter >>= liftIO . print

    return ()
