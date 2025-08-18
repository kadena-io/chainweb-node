{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Main
( main
) where

import Chainweb.Graph
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import System.IO.Temp
import System.LogLevel
import Test.Tasty
import Test.Tasty.HUnit
import qualified Chainweb.Test.MultiNode
import Chainweb.Version
import Chainweb.Logger
import Chainweb.Pact.Backend.Types
import Chainweb.BlockHeaderDB
import Control.Monad.IO.Class
import Chainweb.Test.Pact.Utils
import System.FilePath
import System.Directory
import Chainweb.Pact.Backend.Utils
import Chainweb.PayloadProvider.Pact.BlockHistoryMigration (migrateBlockHistroyTable)
import Chainweb.Version.EvmTestnet (evmTestnet)
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Database.SQLite3.Direct as SQL

main :: IO ()
main = defaultMain suite

loglevel :: LogLevel
loglevel = Warn


cid :: ChainId
cid = unsafeChainId 0

ingestDataFromExport :: SQLiteEnv -> FilePath -> IO ()
ingestDataFromExport sql fp = either (error "could not create table") id <$> runExceptT (do
        stmts <- liftIO $ BS.readFile fp
        exec_ sql
            "CREATE TABLE IF NOT EXISTS BlockHistory \
            \(blockheight UNSIGNED BIGINT NOT NULL, \
            \ endingtxid UNSIGNED BIGINT NOT NULL, \
            \ hash BLOB NOT NULL, \
            \ CONSTRAINT blockHeightConstraint UNIQUE (blockheight), \
            \ CONSTRAINT hashConstraint UNIQUE (hash));"
        exec_ sql (SQL.Utf8 stmts))


suite :: TestTree
suite = testCase "Migrate data" $ do
  let dbDir = "./tmp-data/"
  logger <- getTestLogger
  sql <- startSqliteDb cid logger dbDir False
  ingestDataFromExport sql "/Users/rsoeldner/Downloads/file.sql"
  withReadOnlyRocksDb "/Users/rsoeldner/Downloads/test_db_new" modernDefaultOptions $ \rocksdb -> do
    (bh, bhdb) <- toyBlockHeaderDb rocksdb cid
    withVersion evmTestnet $ do
      migrateBlockHistroyTable logger sql bhdb
