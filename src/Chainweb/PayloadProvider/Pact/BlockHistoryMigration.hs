{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.PayloadProvider.Pact.BlockHistoryMigration where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.Logger
import Chainweb.Pact.Backend.Types (SQLiteEnv)
import Chainweb.TreeDB
import Chainweb.Version (HasVersion)
import Control.Lens
import Control.Monad.IO.Class
import qualified System.Logger as L
import System.LogLevel
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.PactState (qryStream)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Chainweb.Utils (sshow, whenM)
import Control.Exception.Safe
import Chainweb.Utils.Serialization (runPutS, runGetS)
import Data.IORef (newIORef, readIORef, modifyIORef')

-- | Migrates the @BlockHistory@ table if it is outdated or missing required columns.
--
-- Due to SQLite limitations, we cannot simply add the missing column and migrate
-- the data afterward. Specifically, constraints (such as the uniqueness of the
-- @blockhash@ column) must be satisfied at the time the column is added, and
-- SQLite does not allow adding columns with unique constraints via @ALTER TABLE@.
-- See <https://www.sqlite.org/lang_altertable.html> for more details.
--
-- The migration is performed only if 'tableNeedsMigration' indicates it's necessary.
-- It proceeds in two main steps:
--
--   1. Creation of a new table named @BlockHistory2@ with the correct schema.
--   2. Row-wise migration of existing data from the old @BlockHistory@ table and
--      the 'BlockHeaderDb' (backed by RocksDb), performed in chunks of 10,000 entries.
--
-- Each chunk is committed in a transaction block.
migrateBlockHistroyTable
  :: HasVersion
  => Logger logger
  => logger        -- ^ logger
  -> SQLiteEnv     -- ^ sqlite database
  -> BlockHeaderDb -- ^ block header database
  -> IO ()
migrateBlockHistroyTable logger sdb bhdb
  = L.withLoggerLabel ("component", "migrateBlockHistoryTable") logger $ \lf ->
    whenM (tableNeedsMigration lf sdb) $ do
      let logf serv msg = liftIO $ logFunctionText lf serv msg

      nRows <- throwOnDbError $ qry_ sdb "SELECT count(*) from BlockHistory" [RInt] >>= \case
        [[SInt n]] -> pure n
        _ -> error "unexpected row shape"

      let qstmt =  "SELECT blockheight, endingtxid, hash from BlockHistory ORDER BY blockheight,endingtxid ASC"
          rty = [RInt, RInt, RBlob]

      e <- qryStream sdb  qstmt [] rty $ \rs -> do
        remainingRowsRef <- newIORef nRows
        rs
          & S.chunksOf 10_000
          & S.mapsM_ (\chunk -> do
            remainingRows <- readIORef remainingRowsRef
            logf Info $ "Table migration: Process remaining rows: " <> sshow remainingRows
            r <- tx $ flip S.mapM_ chunk $ \case
              rr@[SInt bh, SInt _, SBlob h] -> do
                let rowBlockHeight = fromIntegral bh
                rowBlockHash <- runGetS decodeBlockHash h

                blockHeader <- lookupRankedM bhdb rowBlockHeight rowBlockHash
                  `catch` \case
                    e@(TreeDbKeyNotFound{} :: TreeDbException BlockHeaderDb) -> do
                      logf Error $ "BlockHeader Entry missing for "
                        <> "blockHeight="
                        <> sshow rowBlockHeight
                        <> ", blockHash="
                        <> sshow rowBlockHash
                      throwM e
                    e -> throwM e

                let bph = view blockPayloadHash blockHeader
                    enc = runPutS $ encodeBlockPayloadHash bph
                throwOnDbError $ exec' sdb "INSERT INTO BlockHistory2 (blockheight, endingtxid, hash, payloadhash) VALUES (?1, ?2, ?3, ?4)"
                  $ rr ++ [SBlob enc]
              _ -> error "unexpected row shape"

            modifyIORef' remainingRowsRef (\old -> max 0 (old - 10_000))
            pure r)

      case e of
        Left e' -> error $ "Table migration failure: " <> sshow e'
        Right () -> do
          logf Info "Data migration completed, cleaning up"
          throwOnDbError $ exec_ sdb "DROP TABLE BlockHistory"
          logf Info "Table migration successful"

  where
    tx = bracket_
      (throwOnDbError $ exec_ sdb "BEGIN TRANSACTION;")
      (throwOnDbError $ exec_ sdb "COMMIT TRANSACTION;")

-- | Checks whether the @BlockHistory@ table is (still) existing
--   and therefore requires a data migration step.
tableNeedsMigration
  :: Logger logger
  => logger
  -> SQLiteEnv
  -> IO Bool
tableNeedsMigration lf sqe = do
  let logf serv msg = liftIO $ logFunctionText lf serv msg

  r <- throwOnDbError $ qry sqe "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'BlockHistory';" [] [RInt]
  case r of
    [[SInt _]] -> do
        logf Info "BlockHistory table exists, need to migrate data."
        pure True
    _ -> do
      logf Info "BlockHistory table does not exists. No table migration required."
      pure False
