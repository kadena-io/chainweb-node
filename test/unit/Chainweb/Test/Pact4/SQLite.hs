{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.Pact4.SQLite
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Pact4.SQLite
( tests
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.State

import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Coerce
import qualified Data.Hash.SHA3 as SHA3
import Data.Hash.SHA3 (Sha3_224(..), Sha3_256(..), Sha3_384(..), Sha3_512(..))
import qualified Data.List as L
import Data.String

import Pact.Types.SQLite

import System.IO.Unsafe
import System.Random (uniformByteString, getStdRandom)

import Test.Hash.SHA3
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules


import Chainweb.Test.Utils
import Chainweb.Pact.Backend.Types (SQLiteEnv)

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = withResourceT withInMemSQLiteResource $ \dbIO ->
    withResource' (dbIO >>= newMVar) $ \dbVarIO ->
        let run = runMsgTest dbVarIO []
            runMonte = runMonteTest dbVarIO []

            -- Split input
            runVar = runMsgTest dbVarIO [1,2,17]
            runMonteVar = runMonteTest dbVarIO [1,2,17]

        in testGroup "SQL Tests"
            [ testGroup "sha3 single argument"
                [ testGroup "ShortMsg"
                    [ testCase "-" $ run 0 sha3_256ShortMsg
                    , testCase "224" $ run 224 sha3_224ShortMsg
                    , testCase "256" $ run 256 sha3_256ShortMsg
                    , testCase "384" $ run 384 sha3_384ShortMsg
                    , testCase "512" $ run 512 sha3_512ShortMsg
                    ]
                , testGroup "LongMsg"
                    [ testCase "-" $ run 0 sha3_256LongMsg
                    , testCase "224" $ run 224 sha3_224LongMsg
                    , testCase "256" $ run 256 sha3_256LongMsg
                    , testCase "384" $ run 384 sha3_384LongMsg
                    , testCase "512" $ run 512 sha3_512LongMsg
                    ]
                , testGroup "Monte"
                    [ testCase "-" $ runMonte 0 sha3_256Monte
                    , testCase "224" $ runMonte 224 sha3_224Monte
                    , testCase "256" $ runMonte 256 sha3_256Monte
                    , testCase "384" $ runMonte 384 sha3_384Monte
                    , testCase "512" $ runMonte 512 sha3_512Monte
                    ]
                ]
            , testGroup "sha3 multiple arguments"
                [ testGroup "ShortMsg"
                    [ testCase "-" $ runVar 0 sha3_256ShortMsg
                    , testCase "224" $ runVar 224 sha3_224ShortMsg
                    , testCase "256" $ runVar 256 sha3_256ShortMsg
                    , testCase "384" $ runVar 384 sha3_384ShortMsg
                    , testCase "512" $ runVar 512 sha3_512ShortMsg
                    ]
                , testGroup "LongMsg"
                    [ testCase "-" $ runVar 0 sha3_256LongMsg
                    , testCase "224" $ runVar 224 sha3_224LongMsg
                    , testCase "256" $ runVar 256 sha3_256LongMsg
                    , testCase "384" $ runVar 384 sha3_384LongMsg
                    , testCase "512" $ runVar 512 sha3_512LongMsg
                    ]
                , testGroup "Monte"
                    [ testCase "-" $ runMonteVar 0 sha3_256Monte
                    , testCase "224" $ runMonteVar 224 sha3_224Monte
                    , testCase "256" $ runMonteVar 256 sha3_256Monte
                    , testCase "384" $ runMonteVar 384 sha3_384Monte
                    , testCase "512" $ runMonteVar 512 sha3_512Monte
                    ]
                ]
            , withAggTable dbVarIO 512 128 $ \tbl -> testGroup "sha3 aggregation"
                [ testCase "-" $ testAgg 0 dbVarIO tbl
                , testCase "224" $ testAgg 224 dbVarIO tbl
                , testCase "256" $ testAgg 256 dbVarIO tbl
                , testCase "384" $ testAgg 384 dbVarIO tbl
                , testCase "512" $ testAgg 512 dbVarIO tbl
                ]
            , testGroup "sha3 msgTable"
                [ testCase "-" $ msgTableTest dbVarIO 0 sha3_256ShortMsg
                , testCase "224" $ msgTableTest dbVarIO 224 sha3_224ShortMsg
                , testCase "256" $ msgTableTest dbVarIO 256 sha3_256ShortMsg
                , testCase "384" $ msgTableTest dbVarIO 384 sha3_384ShortMsg
                , testCase "512" $ msgTableTest dbVarIO 512 sha3_512ShortMsg
                ]
            , testGroup "sha3 monteTable"
                [ testCase "-" $ monteTableTest dbVarIO 0 sha3_256Monte
                , testCase "sha224" $ monteTableTest dbVarIO 224 sha3_224Monte
                , testCase "sha256" $ monteTableTest dbVarIO 256 sha3_256Monte
                , testCase "sha384" $ monteTableTest dbVarIO 384 sha3_384Monte
                , testCase "sha512" $ monteTableTest dbVarIO 512 sha3_512Monte
                ]
            ]

-- -------------------------------------------------------------------------- --
--

sha :: IsString a => Monoid a => Int -> a
sha 0 = "sha3"
sha i = "sha3_" <> fromString (show i)

shaa :: IsString a => Monoid a => Int -> a
shaa 0 = "sha3a"
shaa i = "sha3a_" <> fromString (show i)

-- -------------------------------------------------------------------------- --
--

runMsgTest :: IO (MVar SQLiteEnv) -> [Int] -> Int -> MsgFile -> IO ()
runMsgTest dbVarIO splitArg n f = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        msgAssert (\_ a b -> a @?= b) (sqliteSha3 db n splitArg) f

runMonteTest :: IO (MVar SQLiteEnv) -> [Int] -> Int -> MonteFile -> IO ()
runMonteTest dbVarIO splitArg n f = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        monteAssert (\_ a b -> a @?= b) (sqliteSha3 db n splitArg) f

-- -------------------------------------------------------------------------- --
-- Repeated use in a query

msgTableTest :: IO (MVar SQLiteEnv) -> Int -> MsgFile -> IO ()
msgTableTest dbVarIO n msgFile = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        msgTable db name msgFile
        rows <- qry_ db query [RInt]
        h <- case rows of
            [[SInt r]] -> return r
            [[x]] -> error $ "unexpected return value: " <> show x
            [a] -> error $ "unexpected number of result fields: " <> show (length a)
            a -> error $ "unexpected number of result rows: " <> show (length a)
        h @?= 0
        exec_ db ("DROP TABLE " <> fromString name)
  where
    query = "SELECT sum(" <> sha n <> "(substr(msg,1,len)) != md) FROM " <> fromString name
    name = "msgTable_" <> show n

msgTable :: SQLiteEnv -> String -> MsgFile -> IO ()
msgTable db name msgFile = do
    exec_ db ("CREATE TABLE " <> tbl <> " (len INT, msg BLOB, md BLOB)")
    forM_ (_msgVectors msgFile) $ \i -> do
        let l = fromIntegral $ _msgLen i
        exec'
            db
            ("INSERT INTO " <> tbl <> " VALUES (?, ?, ?)")
            [SInt l, SBlob (_msgMsg i), SBlob (_msgMd i)]
  where
    tbl = fromString name

-- -------------------------------------------------------------------------- --
-- Repeated use in query for MonteFile

monteTableTest :: IO (MVar SQLiteEnv) -> Int -> MonteFile -> IO ()
monteTableTest dbVarIO n monteFile = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db ->
        monteTableTest_ db n monteFile

monteTableTest_ :: SQLiteEnv -> Int -> MonteFile -> IO ()
monteTableTest_ db n monteFile = do
        monteTable db monteTableName monteFile
        let query = fromString $ unwords
                [ "WITH RECURSIVE"
                , "  tmp(c, m) AS ("
                , "    SELECT 0, ? UNION ALL SELECT c + 1, " <> sha n <> "(m) FROM tmp"
                , "    WHERE c <= 100000"
                , "  ),"
                , "  tmp2(count, md) AS ("
                , "    SELECT c / 1000 - 1 AS count, m AS md FROM tmp"
                , "    WHERE c % 1000 == 0 AND count >= 0"
                , "  )"
                , "SELECT"
                , "  sum(tmp2.md != " <> monteTableName <> ".md)"
                , "FROM tmp2"
                , "LEFT JOIN " <> monteTableName
                , "ON tmp2.count = " <> monteTableName <> ".count"
                ]
        rows <- qry db query [SBlob $ _monteSeed monteFile] [RInt]
        case rows of
            [[SInt r]] -> r @?= 0
            [[x]] -> error $ "unexpected return value: " <> show x
            [a] -> error $ "unexpected number of result fields: " <> show (length a)
            a -> error $ "unexpected number of result rows: " <> show (length a)
  where
    monteTableName = "monteTable_" <> show n

monteTable :: SQLiteEnv -> String -> MonteFile -> IO ()
monteTable db name monteFile = do
    exec_ db ("CREATE TABLE " <> tbl <> " (count INT, md BLOB)")
    forM_ (_monteVectors monteFile) $ \i -> do
        exec'
            db
            ("INSERT INTO " <> tbl <> " VALUES (?, ?)")
            [SInt (fromIntegral $ _monteCount i), SBlob (_monteMd i)]
  where
    tbl = fromString name

-- -------------------------------------------------------------------------- --
-- Aggregate functions
--
-- split a large input accross table rows

withAggTable
    :: IO (MVar SQLiteEnv)
    -> Int
    -> Int
    -> (IO (String, [B.ByteString]) -> TestTree)
    -> TestTree
withAggTable dbVarIO rowCount chunkSize =
    withResource' createAggTable
  where
    tbl = "bytesTbl"
    createAggTable = do
        dbVar <- dbVarIO
        withMVar dbVar $ \db -> do
            input <- getStdRandom $ runState $
                replicateM rowCount $ state (uniformByteString chunkSize)
            exec_ db ("CREATE TABLE " <> fromString tbl <> " (bytes BLOB)")
            forM_ input $ \i ->
                exec' db ("INSERT INTO " <> fromString tbl <> " VALUES(?)") [SBlob i]
            return (tbl, input)

testAgg :: Int -> IO (MVar SQLiteEnv) -> IO (String, [B.ByteString]) -> IO ()
testAgg n dbVarIO tblIO = do
    dbVar <- dbVarIO
    (tbl, input) <- first fromString <$> tblIO
    withMVar dbVar $ \db -> do
        rows <- qry_ db ("SELECT " <> shaa n <> "(bytes) FROM " <> tbl) [RBlob]
        h <- case rows of
            [[SBlob r]] -> return r
            [[x]] -> error $ "unexpected return value: " <> show x
            [a] -> error $ "unexpected number of result fields: " <> show (length a)
            a -> error $ "unexpected number of result rows: " <> show (length a)

        hBytes <- hash n (mconcat input)
        h @?= hBytes
  where
    hash :: Int -> B.ByteString -> IO B.ByteString
    hash d b = case d of
        0 -> hashToByteString <$> SHA3.hashByteString @SHA3.Sha3_256 b
        224 -> hashToByteString <$> SHA3.hashByteString @SHA3.Sha3_224 b
        256 -> hashToByteString <$> SHA3.hashByteString @SHA3.Sha3_256 b
        384 -> hashToByteString <$> SHA3.hashByteString @SHA3.Sha3_384 b
        512 -> hashToByteString <$> SHA3.hashByteString @SHA3.Sha3_512 b
        _ -> error $ "unsupported SHA3 digest size: " <> show d

hashToByteString :: (SHA3.Hash a, Coercible a BS.ShortByteString) => a -> B.ByteString
hashToByteString = BS.fromShort . coerce

-- -------------------------------------------------------------------------- --
-- SHA3 Implementation

sqliteSha3 :: SQLiteEnv -> Int -> [Int] -> B.ByteString -> B.ByteString
sqliteSha3 db n argSplit arg = unsafePerformIO $ do
    rows <- qry db queryStr params [RBlob]
    case rows of
        [[SBlob r]] -> return r
        [[x]] -> error $ "unexpected return value: " <> show x
        [a] -> error $ "unexpected number of result fields: " <> show (length a)
        a -> error $ "unexpected number of result rows: " <> show (length a)
  where
    argN = length argSplit
    argStr = fromString $ L.intercalate "," $ replicate (argN + 1) "?"

    queryStr = "select " <> sha n <> "(" <> argStr <> ")"

    params = go argSplit arg

    go [] l = [SBlob l]
    go (h:t) bs = let (a,b) = B.splitAt h bs in SBlob a : go t b
