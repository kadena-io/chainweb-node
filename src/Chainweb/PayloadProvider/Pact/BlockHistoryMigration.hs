{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import System.Logger qualified as L
import System.LogLevel
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Backend.PactState (qryStream)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import Chainweb.Utils (sshow, whenM)
import Control.Exception.Safe
import Chainweb.Utils.Serialization (runPutS, runGetS)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Control.Monad
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Migrates the @BlockHistory@ table if it is outdated or missing required columns.
--
-- Due to SQLite limitations, we cannot simply add the missing column and migrate
-- the data afterward. Specifically, constraints (such as the uniqueness of the
-- @blockhash@ column) must be satisfied at the time the column is added, and
-- SQLite does not allow adding columns with unique constraints via @ALTER TABLE@.
-- See <https://www.sqlite.org/lang_altertable.html> for more details.
--
-- The migration is performed only if 'tableNeedsMigration' indicates it's necessary.
-- It proceeds in a row-wise migration of existing data from the old @BlockHistory@ table and
-- the 'BlockHeaderDb' (backed by RocksDb), performed in chunks of 10,000 entries.
--
-- Each chunk is committed in a transaction block.
--
-- NOTE: THIS Module can be deleted after migration !!!
--
migrateBlockHistoryTable
  :: HasVersion
  => Logger logger
  => logger        -- ^ logger
  -> SQLiteEnv     -- ^ sqlite database
  -> BlockHeaderDb -- ^ block header database
  -> Bool          -- ^ cleanup table after migration
  -> IO ()
migrateBlockHistoryTable logger sdb bhdb cleanup
  = L.withLoggerLabel ("component", "migrateBlockHistoryTable") logger $ \lf ->
    whenM (tableNeedsMigration lf sdb) $ do
      let logf serv msg = liftIO $ logFunctionText lf serv msg

      nBlockHistory2 <- nTableEntries "BlockHistory2"
      nBlockHistory <- nTableEntries "BlockHistory"

      when (nBlockHistory2 <= nBlockHistory) $ do
        logf Info "Partial migration detected, cleaning up and restarting."
        throwOnDbError $ exec_ sdb "DELETE FROM BlockHistory2"

      let qstmt = "SELECT blockheight, endingtxid, hash from BlockHistory ORDER BY blockheight,endingtxid ASC"
          rty = [RInt, RInt, RBlob]

      start <- getCurrentTime

      e <- qryStream sdb qstmt [] rty $ \rs -> do
        remainingRowsRef <- newIORef nBlockHistory
        rs
          & S.chunksOf 10_000
          & S.mapsM_ (\chunk -> do
            remainingRows <- readIORef remainingRowsRef
            let perc = (1.0 - fromIntegral @_ @Double remainingRows / fromIntegral @_ @Double nBlockHistory) * 100.0
            logf Info $ "Table migration: Process remaining rows: " <> sshow remainingRows <> " ("<> sshow perc <> ")"
            r <- tx $ flip S.mapM_ chunk $ \case
              rr@[SInt bh, SInt _, SBlob h] -> do
                let rowBlockHeight = fromIntegral bh
                rowBlockHash <- runGetS decodeBlockHash h
                blockHeader <- lookupRanked bhdb rowBlockHeight rowBlockHash >>= \case
                    Nothing -> do
                      error $ "BlockHeader Entry missing for "
                        <> "blockHeight="
                        <> sshow rowBlockHeight
                        <> ", blockHash="
                        <> sshow rowBlockHash
                    Just blockHeader -> return blockHeader

                let bph = view blockPayloadHash blockHeader
                    enc = runPutS $ encodeBlockPayloadHash bph

                throwOnDbError $ exec' sdb "INSERT INTO BlockHistory2 (blockheight, endingtxid, hash, payloadhash) VALUES (?, ?, ?, ?)"
                  $ rr ++ [SBlob enc]
              _ -> error "unexpected row shape"

            modifyIORef' remainingRowsRef (\old -> max 0 (old - 10_000))
            pure r)

      case e of
        Left e' -> error $ "Table migration failure: " <> sshow e'
        Right () -> do
          end <- getCurrentTime
          logf Info $ "Elapsed Time: " <> sshow (diffUTCTime end start)

          when cleanup $ do
            logf Info "Data migration completed, cleaning up"
            throwOnDbError $ exec_ sdb "DROP TABLE BlockHistory"

          logf Info "Table migration successful"

  where
    tx = bracket_
      (throwOnDbError $ exec_ sdb "BEGIN TRANSACTION")
      (throwOnDbError $ exec_ sdb "COMMIT TRANSACTION")
    nTableEntries tname = throwOnDbError $ qry_ sdb ("SELECT count(*) from " <> tname) [RInt] >>= \case
        [[SInt n]] -> pure n
        _ -> error "unexpected row shape"


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
