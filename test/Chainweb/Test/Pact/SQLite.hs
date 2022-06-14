{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Test.Pact.SQLite
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Pact.SQLite
( tests
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Coerce
import qualified Data.Hash.SHA3 as SHA3
import qualified Data.List as L
import Data.String

import Pact.Types.SQLite

import System.IO.Unsafe
import System.Random (genByteString, getStdRandom)

import Test.Hash.SHA3
import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Chainweb.Pact.Backend.Types
import Chainweb.Test.Utils

-- TODO: we should consider submitting all queries for a file in a single sql statememt?
-- We could turn the file contents into a table and implement the checks in SQL
--
tests :: TestTree
tests = withInMemSQLiteResource $ \dbIO ->
    withResource (dbIO >>= newMVar) mempty $ \dbVarIO ->
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
            , testGroup "sha3 aggregation"
                [ testCase "256" $ testAgg dbVarIO
                ]
            ]

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
-- Incremental use in a query:
--
-- TODO: import a monte file into a sqlite table

-- -------------------------------------------------------------------------- --
-- Aggregate functions
--
-- split large input accross table rows

testAgg :: IO (MVar SQLiteEnv) -> IO ()
testAgg dbVarIO = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        -- create tests data
        input <- getStdRandom $ runState $ replicateM 512 $ state (genByteString 128)

        -- create table
        putStrLn "create table"
        exec_ (_sConn db) "CREATE TABLE bytesTbl (bytes BLOB);"
        putStrLn "insert data"
        forM_ input $ \i ->
            exec' (_sConn db) "INSERT INTO bytesTbl VALUES(?)" [SBlob i]

        -- compute aggregated hash
        putStrLn "compute hash"
        rows <- qry_ (_sConn db) "SELECT sha3a(bytes) FROM bytesTbl;" [RBlob]
        h <- case rows of
            [[SBlob r]] -> return r
            [[x]] -> error $ "unexpected return value: " <> show x
            [a] -> error $ "unexpected number of result fields: " <> show (length a)
            a -> error $ "unexpected number of result rows: " <> show (length a)

        h @?= hashToByteString (SHA3.hashByteString @SHA3.Sha3_256 (mconcat input))

hashToByteString :: SHA3.Hash a => Coercible a BS.ShortByteString => a -> B.ByteString
hashToByteString = BS.fromShort . coerce

-- -------------------------------------------------------------------------- --
-- SHA3 Implementation

sqliteSha3 :: SQLiteEnv -> Int -> [Int] -> B.ByteString -> B.ByteString
sqliteSha3 db n argSplit arg = unsafePerformIO $ do
    rows <- qry (_sConn db) queryStr params [RBlob]
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

    sha :: IsString a => Monoid a => Int -> a
    sha 0 = "sha3"
    sha i = "sha3_" <> fromString (show i)

