{-# LANGUAGE RankNTypes, OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main
( main
) where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Graph
import Chainweb.Logger
import Chainweb.Pact.Backend.ChainwebPactDb
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.PactState (qryStream)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.PayloadProvider.Pact.BlockHistoryMigration (migrateBlockHistoryTable)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.MultiNode qualified
import Chainweb.Test.Pact.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.TreeDB (lookupRankedM)
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.EvmTestnet (evmTestnet)
import Chainweb.Version.Mainnet
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString qualified as BS
import Data.Function
import Database.SQLite3.Direct qualified as SQL
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Temp
import System.LogLevel
import Test.HUnit (assertFailure)
import Test.Tasty
import Test.Tasty.HUnit

loglevel :: LogLevel
loglevel = Warn

cid :: ChainId
cid = unsafeChainId 1

prepareSQLite :: SQLiteEnv -> IO ()
prepareSQLite sql = throwOnDbError $ do
  exec_ sql
    "CREATE TABLE IF NOT EXISTS BlockHistory \
    \(blockheight UNSIGNED BIGINT NOT NULL, \
    \ endingtxid UNSIGNED BIGINT NOT NULL, \
    \ hash BLOB NOT NULL, \
    \ CONSTRAINT blockHeightConstraint UNIQUE (blockheight), \
    \ CONSTRAINT hashConstraint UNIQUE (hash));"

  liftIO $ ChainwebPactDb.initSchema sql

requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= maybe (fail $ name ++ " not set") pure

main :: IO ()
main = withSystemTempDirectory "sql-db" $ \dbDir -> do
  logger <- getTestLogger

  -- /Volumes/Extreme Pro/db-chainweb-node-mainnet/0/sqlite/pact-v1-chain-1.sqlite
  dbpath <- requireEnv "SQLITE_CHAIN1_FILEPATH"
  -- /Volumes/Extreme Pro/db-chainweb-node-mainnet/0/rocksDb
  rocksdbPath <- requireEnv "ROCKSDB_DIRPATH"

  copyFile dbpath (dbDir </> "pact-v1-chain-1.sqlite")

  sql <- startSqliteDb cid logger dbDir False

  prepareSQLite sql
  n <- nTableEntries sql "BlockHistory"
  n2 <- nTableEntries sql "BlockHistory2"

  assert (n > 0) "BlockHistory Table is not empty"
  assert (n2 == 0) "BlockHistory2 Table is empty"

  withRocksDb rocksdbPath modernDefaultOptions $ \rocksdb -> do
    withVersion Mainnet01 $ runResourceT $ do
      bhdb <- withBlockHeaderDb rocksdb cid
      liftIO $ do
        migrateBlockHistoryTable logger sql bhdb False

        let qstmt = "SELECT A.blockheight, A.endingtxid, \
                    \ B.hash AS b_hash, B.payloadhash AS b_payload_hash, \
                    \ A.hash AS a_hash \
                    \ FROM BlockHistory AS A INNER JOIN BlockHistory2 AS B \
                    \ ON A.blockheight = B.blockheight AND A.endingtxid = B.endingtxid \
                    \ ORDER BY A.blockheight, A.endingtxid"
            rty = [RInt, RInt, RBlob, RBlob, RBlob]

        qryStream sql  qstmt [] rty $ \rs -> do
          rs & flip S.mapM_ $ \[SInt a_bh, SInt a_etxid, SBlob b_hash, SBlob b_payload, SBlob a_hash] -> do

            assert (a_hash == b_hash) $
              "Hash mismatch at block " ++ show a_bh ++ " / txid " ++ show a_etxid

            let rowBlockHeight = fromIntegral a_bh
            rowBlockHash <- runGetS decodeBlockHash a_hash
            blockHeader <- lookupRankedM bhdb rowBlockHeight rowBlockHash
            let bph = view blockPayloadHash blockHeader
                enc = runPutS $ encodeBlockPayloadHash bph

            assert (b_payload == enc) $
              "Payload Hash mismatch at block " ++ show a_bh ++ " / txid " ++ show a_etxid

        n2' <- nTableEntries sql "BlockHistory2"
        assert (n == n2') "BlockHistory2 has the same number of rows as BlockHistory"

  where
  assert p msg =
      if p then pure () else Test.HUnit.assertFailure msg

  nTableEntries sql tname = throwOnDbError $ qry_ sql ("SELECT count(*) from " <> tname) [RInt] >>= \case
        [[SInt n]] -> pure n
        _ -> error "unexpected row shape"
