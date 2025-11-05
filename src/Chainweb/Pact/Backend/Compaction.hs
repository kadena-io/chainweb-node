{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


module Chainweb.Pact.Backend.Compaction
  (
    -- * Compaction executable implementation
    main

  -- * Exported for testing
  , compactPactState
  , compactRocksDb
  , Retainment(..)
  , defaultRetainment

  -- * Compaction logging utilities
  , withDefaultLogger
  , withPerChainFileLogger
  )
  where

import Control.Exception hiding (Handler)
import Chainweb.BlockHash
import Chainweb.BlockHeader (blockHeight, blockHash, blockPayloadHash)
import Chainweb.BlockHeaderDB.Internal (BlockHeaderDb(..), RankedBlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut.CutHashes (cutIdToText)
import Chainweb.CutDB (cutHashesTable)
import Chainweb.Logger (Logger, l2l, setComponent, logFunctionText)
import Chainweb.Pact.Backend.ChainwebPactDb ()
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.Payload.PayloadStore (addNewPayload, lookupPayloadWithHeight)
import Chainweb.Pact.Payload.PayloadStore.RocksDB (newPayloadDb)
import Chainweb.Storage.Table (Iterator(..), Entry(..), withTableIterator, unCasify, tableInsert)
import Chainweb.Storage.Table.RocksDB (RocksDb, withRocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Utils (sshow, fromTextM, toText, int)
import Chainweb.Version (ChainId, HasVersion(..), withVersion, ChainwebVersion(..))
import Chainweb.Version.Guards (minimumBlockHeaderHistory)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (findKnownVersion)
import Chainweb.Version.Testnet04 (testnet04)
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb, initWebBlockHeaderDb)
import Control.Lens (set, over, (^.), _3, view)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Function ((&))
import Data.Int (Int64)
import Data.LogMessage (SomeLogMessage, logText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.RocksDB.Types (Options(..), Compression(..))
import Database.SQLite3 qualified as Lite
import Database.SQLite3.Direct (Utf8(..), Database)
import Options.Applicative qualified as O
import Prelude hiding (log)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle)
import System.IO qualified as IO
import System.LogLevel qualified as LL
import System.Logger hiding (Logger)
import System.Logger qualified as YAL
import System.Logger.Backend.ColorOption (useColor)
import UnliftIO.Async (pooledForConcurrently_)

withDefaultLogger :: LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend_ logText handleCfg $ \b ->
  withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel (l2l ll) l)
  where
    handleCfg = defaultHandleBackendConfig
      { _handleBackendConfigHandle = StdErr
      }

withRocksDbFileLogger :: FilePath -> LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withRocksDbFileLogger ld ll f = do
  createDirectoryIfMissing True {- do create parents -} ld
  let logFile = ld </> "rocksDb.log"
  let handleConfig = defaultHandleBackendConfig
        { _handleBackendConfigHandle = FileHandle logFile
        }
  withHandleBackend_' logText handleConfig $ \h b -> do
    IO.hSetBuffering h IO.LineBuffering
    withLogger defaultLoggerConfig b $ \l -> do
      let logger = setComponent "compaction"
            $ set setLoggerLevel (l2l ll) l
      f logger

withPerChainFileLogger :: FilePath -> ChainId -> LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withPerChainFileLogger ld chainId ll f = do
  createDirectoryIfMissing True {- do create parents -} ld
  let logFile = ld </> ("chain-" <> cid <> ".log")
  let handleConfig = defaultHandleBackendConfig
        { _handleBackendConfigHandle = FileHandle logFile
        }
  withHandleBackend_' logText handleConfig $ \h b -> do
    IO.hSetBuffering h IO.LineBuffering
    withLogger defaultLoggerConfig b $ \l -> do
      let logger = setComponent "compaction"
            $ over setLoggerScope (("chain", toText chainId) :)
            $ set setLoggerLevel (l2l ll) l
      f logger
  where
    cid = Text.unpack (toText chainId)

withHandleBackend_' :: (MonadIO m, MonadBaseControl IO m)
  => (msg -> Text)
  -> HandleBackendConfig
  -> (Handle -> LoggerBackend msg -> m a)
  -> m a
withHandleBackend_' format conf inner =
  case conf ^. handleBackendConfigHandle of
    StdErr -> run IO.stderr
    StdOut -> run IO.stdout
    FileHandle file -> liftBaseOp (IO.withFile file IO.AppendMode) run
  where
    run h = do
      colored <- liftIO $ useColor (conf ^. handleBackendConfigColor) h
      inner h (handleBackend_ format h colored)

data Config = Config
  { chainwebVersion :: ChainwebVersion
  , fromDir :: FilePath
  , toDir :: FilePath
  , concurrent :: ConcurrentChains
  , logDir :: FilePath
  , noRocksDb :: Bool
    -- ^ Don't produce a new RocksDB at all.
  , noPactState :: Bool
    -- ^ Don't produce a new Pact State at all.
  , keepFullTransactionIndex :: Bool
    -- ^ Whether or not to keep the entire TransactionIndex table. Some APIs rely on this table.
  }

data Retainment = Retainment
  { keepFullTransactionIndex :: Bool
  , compactThese :: CompactThese
  }

defaultRetainment :: Retainment
defaultRetainment = Retainment
  { keepFullTransactionIndex = False
  , compactThese = CompactBoth
  }

data CompactThese = CompactOnlyRocksDb | CompactOnlyPactState | CompactBoth | CompactNeither
  deriving stock (Eq)

data ConcurrentChains = SingleChain | ManyChainsAtOnce

getConfig :: IO Config
getConfig = do
  O.execParser opts
  where
    opts :: O.ParserInfo Config
    opts = O.info (parser O.<**> O.helper)
      (O.fullDesc <> O.progDesc "Pact DB Compaction Tool - create a compacted copy of the source database directory Pact DB into the target directory.")

    parser :: O.Parser Config
    parser = Config
      <$> (parseVersion <$> O.strOption (O.long "chainweb-version" <> O.value "mainnet01"))
      <*> O.strOption (O.long "from" <> O.help "Directory containing SQLite Pact state and RocksDB block data to compact (expected to be in $DIR/0/{sqlite,rocksDb}")
      <*> O.strOption (O.long "to" <> O.help "Directory where to place the compacted Pact state and block data. It will place them in $DIR/0/{sqlite,rocksDb}, respectively.")
      <*> O.flag SingleChain ManyChainsAtOnce (O.long "parallel" <> O.help "Turn on multi-threaded compaction. The threads are per-chain.")
      <*> O.strOption (O.long "log-dir" <> O.help "Directory where compaction logs will be placed.")
      -- Hidden options
      <*> O.switch (O.long "keep-full-rocksdb" <> O.hidden)
      <*> O.switch (O.long "no-rocksdb" <> O.hidden)
      <*> O.switch (O.long "no-pact" <> O.hidden)

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      fromMaybe (error "ChainwebVersion parse failed")
      . (>>= findKnownVersion)
      . fromTextM

main :: IO ()
main = do
  compact =<< getConfig


compactPactState :: (Logger logger) => logger -> Retainment -> BlockHeight -> SQLiteEnv -> SQLiteEnv -> IO ()
compactPactState logger rt targetBlockHeight srcDb targetDb = do
  let log = logFunctionText logger

  -- These pragmas are tuned for fast insertion on systems with a wide range
  -- of system resources.
  --
  -- journal_mode = OFF is terrible for prod but probably OK here
  -- since we are just doing a bunch of bulk inserts
  --
  -- See SQLite Pragma docs: https://www.sqlite.org/pragma.html
  let fastBulkInsertPragmas =
        [ "journal_mode = OFF"
        , "synchronous = OFF"
        , "cache_size = -9766" -- 10 Megabytes
        , "temp_store = FILE"
        , "shrink_memory"
        ]

  -- Establish pragmas for bulk insert performance
  --
  -- Note that we can't apply pragmas to the src
  -- because we can't guarantee it's not being accessed
  -- by another process.
  runPragmas targetDb fastBulkInsertPragmas

  -- Create checkpointer tables on the target
  createCheckpointerTables targetDb logger

  -- Compact BlockHistory
  -- This is extremely fast and low residency
  do
    log LL.Info "Compacting BlockHistory"
    activeRow <- getBlockHistoryRowAt logger srcDb targetBlockHeight
    throwOnDbError $ exec' targetDb "INSERT INTO BlockHistory2 ('blockheight', 'hash', 'payloadhash', 'endingtxid') VALUES (?1, ?2, ?3, ?4)" activeRow

  -- Compact VersionedTableMutation
  -- This is extremely fast and low residency
  do
    log LL.Info "Compacting VersionedTableMutation"
    activeRows <- getVersionedTableMutationRowsAt logger srcDb targetBlockHeight
    Lite.withStatement targetDb "INSERT INTO VersionedTableMutation VALUES (?1, ?2)" $ \stmt -> do
      forM_ activeRows $ \row -> do
        throwOnDbError $ bindParams stmt row
        void $ stepThenReset stmt

  -- Copy over VersionedTableCreation. Read-only rewind needs to know
  -- when the table existed at that time, so we can't compact this.
  --
  -- This is pretty fast and low residency
  do
    log LL.Info "Copying over VersionedTableCreation"
    let wholeTableQuery = "SELECT tablename, createBlockheight FROM VersionedTableCreation"
    throwSqlError $ qryStream srcDb wholeTableQuery [] [RText, RInt] $ \tblRows -> do
      Lite.withStatement targetDb "INSERT INTO VersionedTableCreation VALUES (?1, ?2)" $ \stmt -> do
        flip S.mapM_ tblRows $ \row -> do
          throwOnDbError $ bindParams stmt row
          void $ stepThenReset stmt

  -- Copy over TransactionIndex.
  --
  -- If the user specifies that they want to keep the entire table, then we do so, otherwise,
  -- we compact this based on the RocksDB 'blockHeightKeepDepth'.
  --
  -- /poll and SPV rely on having this table synchronised with RocksDB.
  -- We need to document APIs which need TransactionIndex.
  --
  -- Maybe consider
  -- https://tableplus.com/blog/2018/07/sqlite-how-to-copy-table-to-another-database.html
  do
    (query, args) <-
      if rt.keepFullTransactionIndex
      then do
        log LL.Info "Copying over entire TransactionIndex table. This could take a while"
        let wholeTableQuery = "SELECT txhash, blockheight FROM TransactionIndex ORDER BY blockheight"
        pure (wholeTableQuery, [])
      else do
        log LL.Info "Copying over compacted TransactionIndex"
        let wholeTableQuery = "SELECT txhash, blockheight FROM TransactionIndex WHERE blockheight >= ?1 ORDER BY blockheight"
        pure (wholeTableQuery, [SInt (int (targetBlockHeight - blockHeightKeepDepth))])

    throwSqlError $ qryStream srcDb query args [RBlob, RInt] $ \tblRows -> do
      Lite.withStatement targetDb "INSERT INTO TransactionIndex VALUES (?1, ?2)" $ \stmt -> do
        -- I experimented a bunch with chunk sizes, to keep transactions
        -- small. As far as I can tell, there isn't really much
        -- difference in any of them wrt residency, but there is wrt
        -- speed. More experimentation may be needed here, but 10k is
        -- fine so far.
        S.chunksOf 10_000 tblRows
          & S.mapsM_ (\chunk -> do
              inTx targetDb $ flip S.mapM_ chunk $ \row -> do
                throwOnDbError $ bindParams stmt row
                void (stepThenReset stmt)
            )

    -- Vacuuming after copying over all of the TransactionIndex data,
    -- but before creating its indices, makes a big differences in
    -- memory residency (~0.5G), at the expense of speed (~20s increase)
    throwOnDbError $ exec_ targetDb "VACUUM;"

  -- Create the checkpointer table indices after bulk-inserting into them
  -- This is faster than creating the indices before
  createCheckpointerIndexes targetDb logger

  -- Grab the endingtxid for determining latest state at the
  -- target height
  endingTxId <- getEndingTxId srcDb targetBlockHeight
  log LL.Info $ "Ending TxId is " <> sshow endingTxId

  -- Compact all user tables
  log LL.Info "Starting user tables"
  getLatestPactTableNamesAt srcDb targetBlockHeight
    & S.mapM_ (\tblname -> do
        compactTable logger srcDb targetDb (fromUtf8 tblname) endingTxId
      )

  log LL.Info "Compaction done"

-- We are trying to make sure that we keep around at least 3k blocks.
-- The compaction target is 1k blocks prior to the latest common
-- blockheight (i.e. min (map blockHeight allChains)), so we take
-- the target and add 1k, but the chains can differ at most by the
-- diameter of the chaingraph, so we also add that to make sure that
-- we have full coverage of every chain.
--
-- To keep around another 2k blocks (to get to ~3k), we subtract 2k
-- from the target.
--
-- Note that the number 3k was arbitrary but chosen to be a safe
-- amount of data more than what is in SQLite.
blockHeightKeepDepth :: BlockHeight
blockHeightKeepDepth = 2_000

compact :: Config -> IO ()
compact cfg = withVersion cfg.chainwebVersion $ do
  let cids = allChains

  let _compactThese = case (cfg.noRocksDb, cfg.noPactState) of
        (True, True) -> CompactNeither
        (True, False) -> CompactOnlyPactState
        (False, True) -> CompactOnlyRocksDb
        (False, False) -> CompactBoth

  -- Get the target blockheight.
  targetBlockHeight <- withDefaultLogger LL.Debug $ \logger -> do
    -- Locate the latest (safe) blockheight as per the pact state.
    -- See 'locateLatestSafeTarget' for the definition of 'safe' here.
    targetBlockHeight <- locateLatestSafeTarget logger cfg.chainwebVersion (pactDir cfg.fromDir) cids
    logFunctionText logger LL.Debug $ "targetBlockHeight: " <> sshow targetBlockHeight

    let initDir = do
          -- Check that the target directory doesn't exist already,
          -- then create its entire tree.
          toDirExists <- doesDirectoryExist cfg.toDir
          when toDirExists $ do
            exitLog logger "Compaction \"To\" directory already exists. Aborting."

    case _compactThese of
      CompactNeither -> do
        exitLog logger "No compaction requested. Exiting."

      CompactOnlyRocksDb -> do
        initDir
        createDirectoryIfMissing True (rocksDir cfg.toDir)

      CompactOnlyPactState -> do
        initDir
        createDirectoryIfMissing True (pactDir cfg.toDir)

      CompactBoth -> do
        initDir
        createDirectoryIfMissing True (rocksDir cfg.toDir)
        createDirectoryIfMissing True (pactDir cfg.toDir)

    pure targetBlockHeight

  -- Compact RocksDB.
  unless cfg.noRocksDb $ do
    withRocksDbFileLogger cfg.logDir LL.Debug $ \logger -> do
      withReadOnlyRocksDb (rocksDir cfg.fromDir) modernDefaultOptions $ \srcRocksDb -> do
        withRocksDb (rocksDir cfg.toDir) (modernDefaultOptions { compression = NoCompression }) $ \targetRocksDb -> do
          compactRocksDb (set setLoggerLevel (l2l LL.Info) logger) cids (targetBlockHeight - blockHeightKeepDepth) srcRocksDb targetRocksDb

  -- Compact the pact state.
  let retainment = Retainment
        { keepFullTransactionIndex = cfg.keepFullTransactionIndex
        , compactThese = _compactThese
        }
  unless cfg.noPactState $ do
    forChains_ cfg.concurrent cids $ \cid -> do
      withPerChainFileLogger cfg.logDir cid LL.Debug $ \logger -> runResourceT $ do
        srcDb <- withChainDb cid logger (pactDir cfg.fromDir)
        targetDb <- withChainDb cid logger (pactDir cfg.toDir)
        liftIO $ compactPactState logger retainment targetBlockHeight srcDb targetDb

compactTable :: (Logger logger)
  => logger      -- ^ logger
  -> Database    -- ^ source database (where we get the active pact state)
  -> Database    -- ^ target database (where we put the compacted state, + use as a rowkey cache)
  -> Text        -- ^ the table we are compacting
  -> Int64       -- ^ target blockheight
  -> IO ()
compactTable logger srcDb targetDb tblname endingTxId = do
  let log = logFunctionText logger
  let tblnameUtf8 = toUtf8 tblname

  log LL.Info $ "Creating table " <> tblname
  createUserTable targetDb tblnameUtf8

  -- We create the user table indices before inserting into the table.
  -- This makes the insertions slower, but it's for good reason.
  --
  -- The query that grabs the pact state from the source db groups rowkeys
  -- in descending order by txid. We then simply need to keep only the first
  -- appearance of each rowkey. A simple in-memory cache does not suffice,
  -- because we have strict max residency requirements. So in order to fully
  -- stream with minimal residency, we use the target database as a rowkey cache.
  -- For each rowkey, we check if it appears in the target, and if it does, we
  -- discard that row and move on to the next. This is why we need the indices,
  -- because this membership check is extremely slow without it, and it far
  -- outweighs the insert slowdowns imposed by the indices.
  log LL.Info $ "Creating table indices for " <> tblname
  createUserTableIndex targetDb tblnameUtf8

  -- Create a temporary index on 'rowkey' for a user table, so that upserts work correctly.
  inTx targetDb $ do
    throwOnDbError $ exec_ targetDb $ mconcat
      [ "CREATE UNIQUE INDEX IF NOT EXISTS ", tbl (tblnameUtf8 <> "_rowkey_unique_ix_TEMP"), " ON "
      , tbl tblnameUtf8, " (rowkey)"
      ]

  --   If the rowkey is in the target database, and the txid is greater than the one in the target database, then update the row.
  --   If the rowkey is not in the target database, then insert the row.
  let upsertQuery = Text.concat
        [ "INSERT INTO ", fromUtf8 (tbl tblnameUtf8), " (rowkey, txid, rowdata) "
        , " VALUES (?1, ?2, ?3) "
        , " ON CONFLICT(rowkey) DO UPDATE SET "
        , "   txid=excluded.txid,"
        , "   rowdata=excluded.rowdata"
        , " WHERE excluded.txid > ", fromUtf8 (tbl tblnameUtf8), ".txid"
        ]

  -- This query gets all rows at or below (older than) the target blockheight.
  -- Note that endingtxid is exclusive, so we need to use '<' instead of '<='.
  --
  -- We order by rowid descending because rowid order *generally* (but does not always) agrees
  -- with txid order. This allows the query to be performed as a linear scan on disk. Post-compaction,
  -- the rowid order should always be the same as the txid order, because we set rowid to AUTOINCREMENT, meaning
  -- compacted an already-compacted database will be even faster.
  let activeStateQryText = "SELECT rowkey, txid, rowdata FROM "
              <> "[" <> tblnameUtf8 <> "]"
              <> " WHERE txid < ?1"
              <> " ORDER BY rowid DESC"
  let activeStateQryArgs = [SInt endingTxId]
  let activeStateQryRetTypes = [RText, RInt, RBlob]

  e <- qryStream srcDb activeStateQryText activeStateQryArgs activeStateQryRetTypes $ \rs -> do
    Lite.withStatement targetDb upsertQuery $ \upsertRow -> do
      log LL.Info $ "Inserting compacted rows into table " <> tblname

      rs
        & S.chunksOf 10_000
        & S.mapsM_ (\chunk -> do
            inTx targetDb $ flip S.mapM_ chunk $ \row -> do
              case row of
                [SText _, SInt _, SBlob _] -> do
                  throwOnDbError $ bindParams upsertRow row
                  void $ stepThenReset upsertRow
                _badRowShape -> do
                  exitLog logger "Encountered invalid row shape while compacting"
          )

  -- This index only makes sense during construction of the target database, not after.
  -- If we were to keep this index around, the node would not be able to operate, since
  -- we need to update new rows for the same rowkey.
  inTx targetDb $ do
    throwOnDbError $ exec_ targetDb $ mconcat
      [ "DROP INDEX IF EXISTS ", tbl (tblnameUtf8 <> "_rowkey_unique_ix_TEMP")
      ]


  case e of
    Left sqlErr -> exitLog logger $ "Encountered SQLite error while compacting: " <> sshow sqlErr
    Right () -> pure ()

  log LL.Info $ "Done compacting table " <> tblname

-- | Create all the checkpointer tables
createCheckpointerTables :: (Logger logger)
  => Database
  -> logger
  -> IO ()
createCheckpointerTables db logger = do
  let log = logFunctionText logger LL.Info

  log "Creating Checkpointer table BlockHistory"
  inTx db $ throwOnDbError $ exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS BlockHistory2 "
    , "(blockheight UNSIGNED BIGINT NOT NULL"
    , ", endingtxid UNSIGNED BIGINT NOT NULL"
    , ", hash BLOB NOT NULL"
    , ", payloadhash BLOB NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableCreation"
  inTx db $ throwOnDbError $ exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableCreation "
    , "(tablename TEXT NOT NULL"
    , ", createBlockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableMutation"
  inTx db $ throwOnDbError $ exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableMutation "
    , "(tablename TEXT NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table TransactionIndex"
  inTx db $ throwOnDbError $ exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS TransactionIndex "
    , "(txhash BLOB NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  -- We have to delete from these tables because of the way the test harnesses work.
  -- Ideally in the future this can be removed.
  forM_ ["BlockHistory2", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"] $ \tblname -> do
    log $ "Deleting from table " <> fromUtf8 tblname
    throwOnDbError $ exec_ db $ "DELETE FROM " <> tbl tblname

-- | Create all the indexes for the checkpointer tables.
createCheckpointerIndexes :: (Logger logger) => Database -> logger -> IO ()
createCheckpointerIndexes db logger = do
  let log = logFunctionText logger LL.Info

  log "Creating BlockHistory index"
  inTx db $ throwOnDbError $ exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS BlockHistory_blockheight_unique_ix ON BlockHistory2 (blockheight)"

  log "Creating VersionedTableCreation index"
  inTx db $ throwOnDbError $ exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableCreation_createBlockheight_tablename_unique_ix ON VersionedTableCreation (createBlockheight, tablename)"

  log "Creating VersionedTableMutation index"
  inTx db $ throwOnDbError $ exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableMutation_blockheight_tablename_unique_ix ON VersionedTableMutation (blockheight, tablename)"

  log "Creating TransactionIndex indexes"
  inTx db $ throwOnDbError $ exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS TransactionIndex_txhash_unique_ix ON TransactionIndex (txhash)"
  inTx db $ throwOnDbError $ exec_ db
    "CREATE INDEX IF NOT EXISTS TransactionIndex_blockheight_ix ON TransactionIndex (blockheight)"

-- | Create a single user table
createUserTable :: Database -> Utf8 -> IO ()
createUserTable db tblname = do
  throwOnDbError $ exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS ", tbl tblname, " "
    , "(rowid INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", rowkey TEXT" -- This should be NOT NULL; we need to make a follow-up PR to chainweb-node to update this here and the checkpointer schema
    , ", txid UNSIGNED BIGINT NOT NULL"
    , ", rowdata BLOB NOT NULL"
    , ");"
    ]

  -- We have to delete from the table because of the way the test harnesses work.
  -- Ideally in the future this can be removed.
  throwOnDbError $ exec_ db $ "DELETE FROM " <> tbl tblname

-- | Create the indexes for a single user table
createUserTableIndex :: Database -> Utf8 -> IO ()
createUserTableIndex db tblname = do
  inTx db $ do
    throwOnDbError $ exec_ db $ mconcat
      [ "CREATE UNIQUE INDEX IF NOT EXISTS ", tbl (tblname <> "_rowkey_txid_unique_ix"), " ON "
      , tbl tblname, " (rowkey, txid)"
      ]
    throwOnDbError $ exec_ db $ mconcat
      [ "CREATE INDEX IF NOT EXISTS ", tbl (tblname <> "_txid_ix"), " ON "
      , tbl tblname, " (txid DESC)"
      ]

-- | Returns the active @(blockheight, hash, endingtxid)@ from BlockHistory2
getBlockHistoryRowAt :: (Logger logger)
  => logger
  -> Database
  -> BlockHeight
  -> IO [SType]
getBlockHistoryRowAt logger db target = do
  r <- throwOnDbError $ qry db "SELECT blockheight, hash, payloadhash, endingtxid FROM BlockHistory2 WHERE blockheight = ?1" [SInt (int target)] [RInt, RBlob, RBlob, RInt]
  case r of
    [row@[SInt bh, SBlob _hash, SBlob _phash, SInt _endingTxId]] -> do
      unless (target == int bh) $ do
        exitLog logger "BlockHeight mismatch in BlockHistory2 query. This is a bug in the compaction tool. Please report it on the issue tracker or discord."
      pure row
    _ -> do
      exitLog logger "getBlockHistoryRowAt query: invalid query"

-- | Returns active @[(tablename, blockheight)]@ from VersionedTableMutation
getVersionedTableMutationRowsAt :: (Logger logger)
  => logger
  -> Database
  -> BlockHeight
  -> IO [[SType]]
getVersionedTableMutationRowsAt logger db target = do
  r <- throwOnDbError $ qry db "SELECT tablename, blockheight FROM VersionedTableMutation WHERE blockheight = ?1" [SInt (int target)] [RText, RInt]
  forM r $ \case
    row@[SText _, SInt bh] -> do
      unless (target == int bh) $ do
        exitLog logger "BlockHeight mismatch in VersionedTableMutation query. This is a bug in the compaction tool. Please report it."
      pure row
    _ -> do
      exitLog logger "getVersionedTableMutationRowsAt query: invalid query"

-- | Locate the latest "safe" target blockheight for compaction.
--
--   In mainnet/testnet, this is determined
--   to be the @mininum (map latestBlockHeight chains) - 1000@.
--
--   In devnet, this is just the latest common blockheight
--   (or @minimum (map latestBlockHeight chains)@).
locateLatestSafeTarget :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath
  -> [ChainId]
  -> IO BlockHeight
locateLatestSafeTarget logger v dbDir cids = do
  let log = logFunctionText logger

  let logger' = set setLoggerLevel (l2l LL.Error) logger
  latestCommon <- getLatestCommonBlockHeight logger' dbDir cids
  earliestCommon <- getEarliestCommonBlockHeight logger' dbDir cids

  log LL.Debug $ "Latest Common BlockHeight: " <> sshow latestCommon
  log LL.Debug $ "Earliest Common BlockHeight: " <> sshow earliestCommon

  -- Make sure we have at least 1k blocks of depth for prod.
  -- In devnet or testing versions we don't care.
  let safeDepth :: BlockHeight
      safeDepth
        | v == mainnet || v == testnet04 = BlockHeight 1_000
        | otherwise = BlockHeight 0

  when (latestCommon - earliestCommon < safeDepth) $ do
    exitLog logger "locateLatestSafeTarget: Not enough history to safely compact. Aborting."

  let target = latestCommon - safeDepth
  log LL.Debug $ "Compaction target blockheight is: " <> sshow target
  pure target

-- | Log an error message, then exit with code 1.
exitLog :: (Logger logger)
  => logger
  -> Text
  -> IO a
exitLog logger msg = do
  logFunctionText logger LL.Error msg
  exitFailure

-- | Step through a prepared statement, then clear the statement's bindings
--   and reset the statement.
stepThenReset :: Lite.Statement -> IO Lite.StepResult
stepThenReset stmt = do
  Lite.stepNoCB stmt `finally` (Lite.clearBindings stmt >> Lite.reset stmt)

-- | This is either 'forM_' or 'pooledForConcurrently_', depending on
--   the 'ConcurrentChains' input.
forChains_ :: ConcurrentChains -> [ChainId] -> (ChainId -> IO a) -> IO ()
forChains_ = \case
  SingleChain -> forM_
  ManyChainsAtOnce -> pooledForConcurrently_

-- | Swallow a SQLite 'Lite.Error' and throw it.
throwSqlError :: IO (Either Lite.Error a) -> IO a
throwSqlError ioe = do
  e <- ioe
  case e of
    Left err -> error (show err)
    Right a -> pure a

-- | Run the 'IO' action inside of a transaction.
inTx :: Database -> IO a -> IO a
inTx db io = do
  bracket_
    (throwOnDbError $ exec_ db "BEGIN;")
    (throwOnDbError $ exec_ db "COMMIT;")
    io

pactDir :: FilePath -> FilePath
pactDir db = db </> "0/sqlite"

rocksDir :: FilePath -> FilePath
rocksDir db = db </> "0/rocksDb"

-- | Copy over all CutHashes, all BlockHeaders, and only some Payloads.
compactRocksDb :: (Logger logger)
  => HasVersion
  => logger
  -> [ChainId] -- ^ ChainIds
  -> BlockHeight -- ^ minBlockHeight for payload copying
  -> RocksDb -- ^ source db, should be opened read-only
  -> RocksDb -- ^ target db
  -> IO ()
compactRocksDb logger cids minBlockHeight srcDb targetDb = do
  let log = logFunctionText logger

  -- Copy over entirety of CutHashes table
  let srcCutHashes = cutHashesTable srcDb
  let targetCutHashes = cutHashesTable targetDb
  log LL.Info "Copying over CutHashes table"
  withTableIterator (unCasify srcCutHashes) $ \srcIt -> do
    let go = do
          iterEntry srcIt >>= \case
            Nothing -> do
              pure ()
            Just (Entry k v) -> do
              log LL.Debug $ "Copying over Cut " <> cutIdToText (k ^. _3)
              tableInsert targetCutHashes k v
              iterNext srcIt
              go
    go

  -- Migrate BlockHeaders and Payloads
  let srcPayloads = newPayloadDb srcDb
  let targetPayloads = newPayloadDb targetDb

  -- The target payload db has to be initialised. TODO PP: does it?
  log LL.Info "Initializing payload db"
  srcWbhdb <- initWebBlockHeaderDb srcDb
  targetWbhdb <- initWebBlockHeaderDb targetDb
  forM_ cids $ \cid -> do
    let log' = logFunctionText (addChainIdLabel cid logger)
    log' LL.Info $ "Starting chain " <> toText cid
    srcBlockHeaderDb <- getWebBlockHeaderDb srcWbhdb cid
    targetBlockHeaderDb <- getWebBlockHeaderDb targetWbhdb cid

    withTableIterator (_chainDbCas srcBlockHeaderDb) $ \it -> do
      -- Grab the latest header, for progress logging purposes.
      latestHeader <- do
        iterLast it
        iterValue it >>= \case
          Nothing -> exitLog logger "Missing final payload. This is likely due to a corrupted database."
          Just rbh -> pure (_getRankedBlockHeader rbh ^. blockHeight)

      -- The header that we start at depends on whether or not
      -- we have a minimal block header history window.
      --
      -- On old enough chainweb versions, we just copy over every
      -- single block header that's available.
      --
      -- On new enough chainweb versions, we want to only copy over
      -- the minimal number of block headers.
      case minimumBlockHeaderHistory latestHeader of
        -- Go to the earliest possible entry. We migrate all BlockHeaders, for now.
        -- They are needed for SPV.
        --
        -- Constructing SPV proofs actually needs the payloads, but validating
        -- them does not.
        Nothing -> do
          iterFirst it

        Just minBlockHeaderHistory -> do
          let runBack =
                let x = int latestHeader
                    y = minBlockHeaderHistory
                in if x >= y then x - y else 0
          iterSeek it $ RankedBlockHash (BlockHeight runBack) nullBlockHash

      earliestHeader <- do
        iterValue it >>= \case
          Nothing -> exitLog logger "Missing first payload. This is likely due to a corrupted database."
          Just rbh -> pure (_getRankedBlockHeader rbh ^. blockHeight)

      -- Ensure that we log progress 100 times per chain
      -- I just made this number up as something that felt somewhat sensible
      let offset = (latestHeader - earliestHeader) `div` 100
      let headerProgressPoints = [earliestHeader + i * offset | i <- [1..100]]

      let logHeaderProgress bHeight = do
            when (bHeight `elem` headerProgressPoints) $ do
              let percentDone = sshow $ 100 * fromIntegral @_ @Double (bHeight - earliestHeader) / fromIntegral @_ @Double (latestHeader - earliestHeader)
              log' LL.Info $ percentDone <> "% done."

      let go = do
            iterValue it >>= \case
              Nothing -> do
                log' LL.Info "Finished copying headers and payloads"
              Just rankedBlockHeader -> do
                let blkHeader = _getRankedBlockHeader rankedBlockHeader
                let blkHeight = view blockHeight blkHeader
                let blkHash   = view blockHash blkHeader

                -- Migrate the ranked block table and rank table
                -- unconditionally.
                -- Right now, the headers are definitely needed (we can't delete any).
                --
                -- Not sure about the rank table, though. We keep it to be
                -- conservative.
                log' LL.Debug $ "Copying over BlockHeader " <> toText blkHash
                tableInsert (_chainDbCas targetBlockHeaderDb) (RankedBlockHash blkHeight blkHash) rankedBlockHeader
                tableInsert (_chainDbRankTable targetBlockHeaderDb) blkHash blkHeight

                -- We only add the payloads for blocks that are in the
                -- interesting range.
                when (blkHeight >= minBlockHeight) $ do
                  -- Insert the payload into the new database
                  let payloadHash = blkHeader ^. blockPayloadHash
                  log' LL.Info $ "Migrating block payload " <> sshow payloadHash <> " for BlockHeight " <> sshow blkHeight
                  lookupPayloadWithHeight srcPayloads (Just blkHeight) payloadHash >>= \case
                    Nothing -> do
                      exitLog logger "Missing payload: This is likely due to a corrupted database."
                    Just payloadWithOutputs -> do
                      addNewPayload targetPayloads blkHeight payloadWithOutputs

                logHeaderProgress blkHeight

                iterNext it
                go
      go
