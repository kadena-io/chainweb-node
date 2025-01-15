{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Tests for "Chainweb.Storage.RocksDB"
module Main
  ( main
  )
where

import Chainweb.Storage.Table (IsCasValue(..), Iterator(..), IterableTable(..), Entry(..), ReadableTable(..), casInsert, casInsertBatch, casDelete, casDeleteBatch, tableInsert, tableInsertBatch, tableDelete, tableLookupBatch)
import Chainweb.Storage.Table.RocksDB (RocksDb, RocksDbTable, RocksDbUpdate(..), Codec(..), updateBatch, newTable, tableMinKey, tableMaxKey, tableMinValue, tableMaxValue, withTempRocksDb)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (Exception)
import Control.Lens (_1, _2, folded, firstOf, lastOf)
import Control.Monad (unless)
import Control.Monad.Catch (throwM)
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (forM_, traverse_)
import Data.List qualified as List
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import NoThunks.Class (NoThunks, unsafeNoThunks)
import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- Utils

data RocksDbTableTestException
    = CodecException !String
    | RocksDbTableTestFailure !String

instance Show RocksDbTableTestException where
    show (CodecException str) =
      unlines
        [ "Codec exception"
        , str
        ]
    show (RocksDbTableTestFailure str) =
        unlines
          [ "RocksDb table test failure"
          , str
          ]

instance Exception RocksDbTableTestException

assertIO :: HasCallStack => Eq a => Show a => IO a -> a -> IO ()
assertIO f r =
    f >>= \a ->
        unless (a == r) $
            throwM $
                RocksDbTableTestFailure $ unlines
                  [ "test failed:"
                  , unwords ["expected:", show r]
                  , unwords ["actual:", show a]
                  , prettyCallStack callStack
                  ]

assertNoThunks :: (HasCallStack, NoThunks a) => a -> IO ()
assertNoThunks a = case unsafeNoThunks $! a of
  Nothing -> return ()
  Just e ->
    throwM $
      RocksDbTableTestFailure $ unlines
        [ "test failed:"
        , unwords ["unexpected thunk:", show e]
        , prettyCallStack callStack
        ]
{-# noinline assertNoThunks #-}

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

assertEmptyTable :: HasCallStack => RocksDbTable Int Int -> IO ()
assertEmptyTable t = do
    assertNoThunks t
    assertIO (tableLookup t 1) Nothing
    assertEntries t []

assertEntries :: HasCallStack => RocksDbTable Int Int -> [(Int, Int)] -> IO ()
assertEntries t l_ = do
    assertNoThunks t
    forM_ l $ \(k, v) -> assertIO (tableLookup t k) (Just v)
    assertIO (tableLookupBatch t ks) (Just <$> vs)

    assertIO (tableMinKey t) (firstOf (folded . _1) l)
    assertIO (tableMinValue t) (firstOf (folded . _2) l)

    assertIO (tableMaxKey t) (lastOf (folded . _1) l)
    assertIO (tableMaxValue t) (lastOf (folded . _2) l)

    -- check forward iteration and first and last
    withTableIterator t $ \i -> do
        assertIO (iterFirst i >> iterKey i) (firstOf (folded . _1) l)
        assertIO (iterLast i >> iterKey i) (lastOf (folded . _1) l)
        iterFirst i
        assertIO (iterValid i) (not $ null l)
        forM_ l $ \(k, v) -> do
          assertIO (iterEntry i) (Just (Entry k v))
          iterNext i
        assertIO (iterValid i) False

  -- check backward iteration
    withTableIterator t $ \i -> do
        iterLast i
        assertIO (iterValid i) (not $ null l)
        forM_ (reverse l) $ \(k, v) -> do
            assertIO (iterEntry i) (Just (Entry k v))
            iterPrev i
        assertIO (iterValid i) False
  where
    l = List.sort l_
    (ks, vs) = unzip l

tableTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
tableTests db tableName = do
    assertNoThunks t
    assertEmptyTable t

    tableInsert t 1 8
    assertEntries t [(1, 8)]

    tableInsert t 2 9
    assertEntries t [(1, 8), (2, 9)]

    tableDelete t 1
    assertEntries t [(2, 9)]

    tableInsert t 2 8
    assertEntries t [(2, 8)]

    tableDelete t 2
    assertEmptyTable t
  where
    !t = intTable db tableName

tableBatchTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
tableBatchTests db tableName = do
    assertNoThunks t
    assertEmptyTable t

    updateBatch []
    assertEmptyTable t

    updateBatch [RocksDbInsert t 1 8]
    assertEntries t [(1, 8)]

    updateBatch [RocksDbInsert t 2 9]
    assertEntries t [(1, 8), (2, 9)]

    updateBatch [RocksDbDelete t 2]
    assertEntries t [(1, 8)]

    updateBatch [RocksDbInsert t 2 9, RocksDbDelete t 2]
    assertEntries t [(1, 8)]

    updateBatch [RocksDbInsert t 2 9, RocksDbDelete t 2, RocksDbInsert t 2 9]
    assertEntries t [(1, 8), (2, 9)]

    updateBatch [RocksDbInsert t 1 8, RocksDbDelete t 1]
    assertEntries t [(2, 9)]

    updateBatch [RocksDbInsert t 1 7, RocksDbInsert t 1 8, RocksDbInsert t 1 8]
    assertEntries t [(1, 8), (2, 9)]

    updateBatch [RocksDbDelete t 1, RocksDbInsert t 3 7]
    assertEntries t [(2, 9), (3, 7)]

    updateBatch [RocksDbInsert t 4 6, RocksDbInsert t 5 5]
    assertEntries t [(2, 9), (3, 7), (4, 6), (5, 5)]

    updateBatch [RocksDbDelete t 2, RocksDbDelete t 3, RocksDbDelete t 4, RocksDbDelete t 5]
    assertEmptyTable t
  where
    t = intTable db tableName

-- Orphan instance
--
instance IsCasValue Int where
    type CasKeyType Int = Int
    casKey = (+ 10)

casBatchTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
casBatchTests db tableName = do
    assertEmptyTable t

    tableInsertBatch t mempty
    assertEmptyTable t

    casInsertBatch t [1]
    assertCasEntries t [1]

    casInsertBatch t [2]
    assertCasEntries t [1, 2]

    casDeleteBatch t [2]
    assertCasEntries t [1]

    casInsertBatch t [1]
    assertCasEntries t [1]

    casInsertBatch t [2, 2, 2]
    assertCasEntries t [1, 2]

    casInsertBatch t [1, 2, 3, 4]
    assertCasEntries t [1, 2, 3, 4]

    casDeleteBatch t [5]
    assertCasEntries t [1, 2, 3, 4]

    casDeleteBatch t [1, 3, 1]
    assertCasEntries t [2, 4]

    casDeleteBatch t []
    assertCasEntries t [2, 4]

    casDeleteBatch t [2, 4]
    assertEmptyTable t
  where
    t = intTable db tableName

casTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
casTests db tableName = do
    assertEmptyTable t
    assertIO (tableMember t 1) False
    assertIO (tableLookup t 1) Nothing

    casInsertBatch t mempty
    assertEmptyTable t

    casInsert t 1
    assertCasEntries t [1]
    assertIO (tableMember t (casKey @Int 1)) True
    assertIO (tableLookup t (casKey @Int 1)) (Just 1)

    casInsert t 2
    assertCasEntries t [1, 2]
    assertIO (tableMember t (casKey @Int 1)) True
    assertIO (tableMember t (casKey @Int 2)) True
    assertIO (tableLookup t (casKey @Int 1)) (Just 1)
    assertIO (tableLookup t (casKey @Int 2)) (Just 2)
    assertIO (tableLookupBatch t [casKey @Int 1, casKey @Int 2]) [Just 1, Just 2]

    casDelete t 2
    assertCasEntries t [1]
    assertIO (tableMember t (casKey @Int 1)) True
    assertIO (tableMember t (casKey @Int 2)) False
    assertIO (tableLookup t (casKey @Int 1)) (Just 1)
    assertIO (tableLookup t (casKey @Int 2)) Nothing
    assertIO (tableLookupBatch t [casKey @Int 1, casKey @Int 2]) [Just 1, Nothing]

    casInsert t 1
    assertCasEntries t [1]

    traverse_ @[] (casInsert t) [2, 2, 2]
    assertCasEntries t [1, 2]

    traverse_ @[] (casInsert t) [1, 2, 3, 4]
    assertCasEntries t [1, 2, 3, 4]

    casDelete t (casKey @Int 5)
    assertCasEntries t [1, 2, 3, 4]

    traverse_ @[] (casDelete t) [1, 3, 1]
    assertCasEntries t [2, 4]

    traverse_ @[] (casDelete t) [2, 4]
    assertEmptyTable t
  where
    t = intTable db tableName

assertCasEntries :: HasCallStack => RocksDbTable Int Int -> [Int] -> IO ()
assertCasEntries t l = do
    assertNoThunks t
    assertEntries t [(casKey v, v) | v <- l]

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = withTempRocksDb "testDb" $ \db -> do
    tableTests db "testTable0"
    mapConcurrently_
        (\i -> tableTests db $ "testTable" <> B8.pack (show i))
        ([0 .. 100] :: [Int])
    tableBatchTests db "testTable"
    casTests db "testTable"
    casBatchTests db "testTable"