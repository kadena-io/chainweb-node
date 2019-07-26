{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A few basic tests for "Data.CAS.RocksDB"
--
module Main
( main
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8

import Text.Read

-- internal modules

import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- Utils

data RocksDbTableTestException
    = CodecException String
    | RocksDbTableTestFailure String
    deriving (Show, Eq)

instance Exception RocksDbTableTestException

assertIO :: Eq a => Show a => IO a -> a -> IO ()
assertIO f r = f >>= \a -> unless (a == r) $ throwM
    $ RocksDbTableTestFailure
    $ "test failed:\n  expected: " <> show a <> "\n  actual: " <> show r

-- -------------------------------------------------------------------------- --
-- Test Table

intCodec :: Codec Int
intCodec = Codec
    (B8.pack . show)
    (either (throwM . CodecException) return . readEither @Int . B8.unpack)

intTable :: RocksDb -> B8.ByteString -> RocksDbTable Int Int
intTable db tableName = newTable db intCodec intCodec [tableName]

-- -------------------------------------------------------------------------- --
-- Tests

tableTests :: RocksDb -> B8.ByteString -> IO ()
tableTests db tableName = do

    assertIO (tableLookup t 1) Nothing
    assertIO (tableMaxKey t) Nothing
    assertIO (tableMinKey t) Nothing
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) Nothing
        assertIO (tableIterLast i >> tableIterKey i) Nothing

    tableInsert t 1 8
    assertIO (tableLookup t 1) (Just 8)
    assertIO (tableMinKey t) (Just 1)
    assertIO (tableMinValue t) (Just 8)
    assertIO (tableMaxKey t) (Just 1)
    assertIO (tableMaxValue t) (Just 8)
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) (Just 1)
        assertIO (tableIterLast i >> tableIterKey i) (Just 1)

    tableInsert t 2 9
    assertIO (tableLookup t 1) (Just 8)
    assertIO (tableLookup t 2) (Just 9)
    assertIO (tableMinKey t) (Just 1)
    assertIO (tableMinValue t) (Just 8)
    assertIO (tableMaxKey t) (Just 2)
    assertIO (tableMaxValue t) (Just 9)
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) (Just 1)
        assertIO (tableIterLast i >> tableIterKey i) (Just 2)

    tableDelete t 1
    assertIO (tableLookup t 2) (Just 9)
    assertIO (tableMinKey t) (Just 2)
    assertIO (tableMinValue t) (Just 9)
    assertIO (tableMaxKey t) (Just 2)
    assertIO (tableMaxValue t) (Just 9)
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) (Just 2)
        assertIO (tableIterLast i >> tableIterKey i) (Just 2)

    tableDelete t 2
    assertIO (tableLookup t 1) Nothing
    assertIO (tableMaxKey t) Nothing
    assertIO (tableMinKey t) Nothing
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) Nothing
        assertIO (tableIterLast i >> tableIterKey i) Nothing
  where
    t = intTable db tableName

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = withTempRocksDb "testDb" $ \db -> mapConcurrently_
    (\i -> tableTests db $ "testTable" <> B8.pack (show i))
    [0..100 :: Int]

