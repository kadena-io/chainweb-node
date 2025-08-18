{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Chainweb.Test.Pact.BlockHistoryMigrationTest
  (tests)
where
import Test.Tasty
import Chainweb.Test.Utils
import Chainweb.Version
import Test.Tasty.HUnit
import qualified Chainweb.Pact.Backend.ChainwebPactDb as ChainwebPactDb
import Control.Monad.Except
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.Types
import Chainweb.PayloadProvider.Pact.BlockHistoryMigration (migrateBlockHistroyTable)
import Chainweb.Test.Pact.Utils
import Chainweb.Storage.Table.RocksDB
import Chainweb.Logger
import Chainweb.BlockHeaderDB
import Chainweb.Utils

cid :: ChainId
cid = unsafeChainId 0

createLegacyBlockHistoryTable :: SQLiteEnv -> IO ()
createLegacyBlockHistoryTable sql = either (error "could not create table") id <$> runExceptT (do
        exec_ sql
            "CREATE TABLE IF NOT EXISTS BlockHistory \
            \(blockheight UNSIGNED BIGINT NOT NULL, \
            \ endingtxid UNSIGNED BIGINT NOT NULL, \
            \ hash BLOB NOT NULL, \
            \ CONSTRAINT blockHeightConstraint UNIQUE (blockheight), \
            \ CONSTRAINT hashConstraint UNIQUE (hash));")



initSchema :: SQLiteEnv -> IO ()
initSchema sql = do
  ChainwebPactDb.initSchema sql     -- create the BlockHistory2 table
  createLegacyBlockHistoryTable sql -- create the legacy BlockHistory table

withSetup
  :: TestName
  -> (GenericLogger -> SQLiteEnv -> IO ())
  -> (HasVersion => GenericLogger -> SQLiteEnv -> BlockHeaderDb -> IO ())
  -> TestTree
withSetup name setup action = withResourceT (withTempChainSqlite cid) $ \sqlIO -> do
        testCase name $ do
            logger <- getTestLogger
            (sql, _sqlReadPool) <- sqlIO

            _ <- setup logger sql

            withTempRocksDb "chainweb-tests" $ \rdb -> do
              (_, bhdb) <- toyBlockHeaderDb rdb cid
              withVersion toyVersion $
                action logger sql bhdb

tests :: TestTree
tests = testGroup "BlockHistory Table Migration"
    [ withSetup "test without BlockHistoryTable"
        (const createLegacyBlockHistoryTable)
        migrateBlockHistroyTable
    , withSetup "test with empty BlockHistoryTable"
        (const initSchema)
        migrateBlockHistroyTable
    , withSetup "test successful migration cleanup"
        (const initSchema)
        $ \lf sdb bhdb -> do
            let qryIO = throwOnDbError $ qry sdb "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'BlockHistory'" [] [RInt]
            [[SInt pre]] <- qryIO
            assertExpectation "Table should be present" (Expected 1) (Actual pre)
            migrateBlockHistroyTable lf sdb bhdb
            post <- qryIO
            assertExpectation "Table should not be present" (Expected []) (Actual post)
    ]
