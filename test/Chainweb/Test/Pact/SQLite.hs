{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.ByteString as B

import Pact.Types.SQLite

import System.IO.Unsafe

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
        testGroup "SQLite Tests"
            [ testGroup "ShortMsg"
                [ testCase "224" $ runMsgTest dbVarIO 224 sha3_224ShortMsg
                , testCase "256" $ runMsgTest dbVarIO 256 sha3_256ShortMsg
                , testCase "384" $ runMsgTest dbVarIO 384 sha3_384ShortMsg
                , testCase "512" $ runMsgTest dbVarIO 512 sha3_512ShortMsg
                ]
            , testGroup "LongMsg"
                [ testCase "224" $ runMsgTest dbVarIO 224 sha3_224LongMsg
                , testCase "256" $ runMsgTest dbVarIO 256 sha3_256LongMsg
                , testCase "384" $ runMsgTest dbVarIO 384 sha3_384LongMsg
                , testCase "512" $ runMsgTest dbVarIO 512 sha3_512LongMsg
                ]
            , testGroup "Monte"
                [ testCase "224" $ runMonteTest dbVarIO 224 sha3_224Monte
                , testCase "256" $ runMonteTest dbVarIO 256 sha3_256Monte
                , testCase "384" $ runMonteTest dbVarIO 384 sha3_384Monte
                , testCase "512" $ runMonteTest dbVarIO 512 sha3_512Monte
                ]
            ]

runMsgTest :: IO (MVar SQLiteEnv) -> Int -> MsgFile -> IO ()
runMsgTest dbVarIO n f = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        msgAssert (\_ a b -> a @?= b) (sqliteSha3 db n) f

runMonteTest :: IO (MVar SQLiteEnv) -> Int -> MonteFile -> IO ()
runMonteTest dbVarIO n f = do
    dbVar <- dbVarIO
    withMVar dbVar $ \db -> do
        monteAssert (\_ a b -> a @?= b) (sqliteSha3 db n) f

sqliteSha3 :: SQLiteEnv -> Int -> B.ByteString -> B.ByteString
sqliteSha3 db n arg = unsafePerformIO $ do
    rows <- qry (_sConn db) "select sha3(?,?)" [SBlob arg, SInt (fromIntegral n)] [RBlob]
    case head rows of
        [SBlob r] -> return r
        [x] -> error $ "unexpected return value: " <> show x
        a -> error $ "unexpected number of results: " <> show (length a)

