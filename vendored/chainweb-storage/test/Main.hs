{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Tests for "Data.CAS.RocksDB"
--
module Main
( main
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Data.List
import Data.Vector ({- IsList Vector #-})

import GHC.Stack

import Text.Read

-- internal modules

import Data.CAS
import Data.CAS.RocksDB

-- -------------------------------------------------------------------------- --
-- Utils

data RocksDbTableTestException
    = CodecException String
    | RocksDbTableTestFailure String
    deriving (Show)

instance Exception RocksDbTableTestException

assertIO :: HasCallStack => Eq a => Show a => IO a -> a -> IO ()
assertIO f r = f >>= \a -> unless (a == r) $ throwM
    $ RocksDbTableTestFailure
    $ "test failed:\n  expected: " <> show r <> "\n  actual: " <> show a
    <> " " <> prettyCallStack callStack

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
    assertIO (tableLookup t 1) Nothing
    assertEntries t []

assertEntries :: HasCallStack => RocksDbTable Int Int -> [(Int, Int)] -> IO ()
assertEntries t l_ = do
    forM_ l $ \(k,v) ->
        assertIO (tableLookup t k) (Just v)

    assertIO (tableMinKey t) (firstOf (folded._1) l)
    assertIO (tableMinValue t) (firstOf (folded._2) l)

    assertIO (tableMaxKey t) (lastOf (folded._1) l)
    assertIO (tableMaxValue t) (lastOf (folded._2) l)

    -- check forward iteration and first and last
    withTableIter t $ \i -> do
        assertIO (tableIterFirst i >> tableIterKey i) (firstOf (folded._1) l)
        assertIO (tableIterLast i >> tableIterKey i) (lastOf (folded._1) l)

        tableIterFirst i
        assertIO (tableIterValid i) (not $ null l)
        forM_ l $ \(k,v) -> do
            assertIO (tableIterEntry i) (Just (k,v))
            tableIterNext i
        assertIO (tableIterValid i) False

    -- check backward iteration
    withTableIter t $ \i -> do
        tableIterLast i
        assertIO (tableIterValid i) (not $ null l)
        forM_ (reverse l) $ \(k,v) -> do
            assertIO (tableIterEntry i) (Just (k,v))
            tableIterPrev i
        assertIO (tableIterValid i) False
  where
    l = sort l_

tableTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
tableTests db tableName = do
    assertEmptyTable t

    tableInsert t 1 8
    assertEntries t [(1,8)]

    tableInsert t 2 9
    assertEntries t [(1,8), (2,9)]

    tableDelete t 1
    assertEntries t [(2,9)]

    tableInsert t 2 8
    assertEntries t [(2,8)]

    tableDelete t 2
    assertEmptyTable t
  where
    t = intTable db tableName

tableBatchTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
tableBatchTests db tableName = do
    assertEmptyTable t

    updateBatch []
    assertEmptyTable t

    updateBatch [ RocksDbInsert t 1 8]
    assertEntries t [(1,8)]

    updateBatch [RocksDbInsert t 2 9]
    assertEntries t [(1,8), (2,9)]

    updateBatch [RocksDbDelete t 2]
    assertEntries t [(1,8)]

    updateBatch [RocksDbInsert t 2 9, RocksDbDelete t 2]
    assertEntries t [(1,8)]

    updateBatch [RocksDbInsert t 2 9, RocksDbDelete t 2, RocksDbInsert t 2 9]
    assertEntries t [(1,8), (2,9)]

    updateBatch [RocksDbInsert t 1 8, RocksDbDelete t 1]
    assertEntries t [(2,9)]

    updateBatch [RocksDbInsert t 1 7, RocksDbInsert t 1 8, RocksDbInsert t 1 8]
    assertEntries t [(1,8), (2,9)]

    updateBatch [RocksDbDelete t 1, RocksDbInsert t 3 7]
    assertEntries t [(2,9), (3,7)]

    updateBatch [RocksDbInsert t 4 6, RocksDbInsert t 5 5]
    assertEntries t [(2,9), (3,7), (4,6), (5,5)]

    updateBatch [RocksDbDelete t 2, RocksDbDelete t 3, RocksDbDelete t 4, RocksDbDelete t 5]
    assertEmptyTable t
  where
    t = intTable db tableName


-- Orphan instance
--
instance IsCasValue Int where
    type CasKeyType Int = Int
    casKey = (+10)
    {-# INLINE casKey #-}

casBatchTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
casBatchTests db tableName = do
    assertEmptyTable t

    casInsertBatch t mempty
    assertEmptyTable t

    casInsertBatch t [1]
    assertCasEntries t [1]

    casInsertBatch t [2]
    assertCasEntries t [1, 2]

    casDeleteBatch t [casKey @Int 2]
    assertCasEntries t [1]

    casInsertBatch t [1]
    assertCasEntries t [1]

    casInsertBatch t [2, 2, 2]
    assertCasEntries t [1, 2]

    casInsertBatch t [1,2,3,4]
    assertCasEntries t [1, 2, 3, 4]

    casDeleteBatch t [casKey @Int 5]
    assertCasEntries t [1, 2, 3, 4]

    casDeleteBatch t $ casKey @Int <$> [1, 3, 1]
    assertCasEntries t [2, 4]

    casDeleteBatch t []
    assertCasEntries t [2, 4]

    casDeleteBatch t $ casKey @Int <$> [2, 4]
    assertEmptyTable t
  where
    t = intTable db tableName

casTests :: HasCallStack => RocksDb -> B8.ByteString -> IO ()
casTests db tableName = do
    assertEmptyTable t
    assertIO (casMember t 1) False
    assertIO (casLookup t 1) Nothing

    casInsertBatch t mempty
    assertEmptyTable t

    casInsert t 1
    assertCasEntries t [1]
    assertIO (casMember t $ casKey @Int 1) True
    assertIO (casLookup t $ casKey @Int 1) (Just 1)

    casInsert t 2
    assertCasEntries t [1, 2]
    assertIO (casMember t $ casKey @Int 1) True
    assertIO (casMember t $ casKey @Int 2) True
    assertIO (casLookup t $ casKey @Int 1) (Just 1)
    assertIO (casLookup t $ casKey @Int 2) (Just 2)

    casDelete t $ casKey @Int 2
    assertCasEntries t [1]
    assertIO (casMember t $ casKey @Int 1) True
    assertIO (casMember t $ casKey @Int 2) False
    assertIO (casLookup t $ casKey @Int 1) (Just 1)
    assertIO (casLookup t $ casKey @Int 2) Nothing

    casInsert t 1
    assertCasEntries t [1]

    traverse_ @[] (casInsert t) [2, 2, 2]
    assertCasEntries t [1, 2]

    traverse_ @[] (casInsert t) [1,2,3,4]
    assertCasEntries t [1, 2, 3, 4]

    casDelete t $ casKey @Int 5
    assertCasEntries t [1, 2, 3, 4]

    traverse_ @[] (casDelete t) $ casKey @Int <$> [1, 3, 1]
    assertCasEntries t [2, 4]

    traverse_ @[] (casDelete t) $ casKey @Int <$> [2, 4]
    assertEmptyTable t
  where
    t = intTable db tableName

assertCasEntries :: HasCallStack => RocksDbTable Int Int -> [Int] -> IO ()
assertCasEntries t = assertEntries t . fmap (\x -> (casKey x, x))

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = withTempRocksDb "testDb" $ \db -> do
    mapConcurrently_
        (\i -> tableTests db $ "testTable" <> B8.pack (show i))
        ([0..100] :: [Int])
    tableBatchTests db "testTable"
    casTests db "testTable"
    casBatchTests db "testTable"

