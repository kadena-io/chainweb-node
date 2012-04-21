{-# LANGUAGE OverloadedStrings #-}

-- | Demo custom comparator

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8  (take)

import Database.LevelDB


comparator :: Comparator
comparator = Comparator compare

main :: IO ()
main = runResourceT $ do
    db <- open "/tmp/lvlcmptest" [CreateIfMissing, UseComparator comparator]

    put db [] "zzz" ""
    put db [] "yyy" ""
    put db [] "xxx" ""

    withIterator db [] $ \iter -> do
        iterFirst iter
        iterItems iter >>= liftIO . print

    return ()
