{-# language
    BangPatterns
  , CPP
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DuplicateRecordFields
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , ImportQualifiedPost
  , LambdaCase
  , NumericUnderscores
  , OverloadedRecordDot
  , OverloadedStrings
  , PackageImports
  , ScopedTypeVariables
  , TypeApplications
  , RecordWildCards
  , ApplicativeDo
#-}

module Chainweb.Pact.Backend.Compaction
  (
    -- * Compaction executable implementation
    main

  -- * Exported for testing
  , doCompactPactState
  , doCompactRocksDb
  , Retainment(..)
  , defaultRetainment

  -- * Compaction logging utilities
  , withDefaultLogger
  , withPerChainFileLogger
  )
  where

import "base" Control.Exception hiding (Handler)
import "base" Control.Monad (forM, forM_, unless, void, when)
import "base" Control.Monad.IO.Class (MonadIO(liftIO))
import "base" Data.Function ((&))
import "base" Data.Int (Int64)
import "base" Data.Maybe (fromMaybe)
import "base" Prelude hiding (log)
import "base" System.Exit (exitFailure)
import "base" System.IO (Handle)
import "base" System.IO qualified as IO
import "chainweb-storage" Chainweb.Storage.Table (Iterator(..), Entry(..), withTableIterator, unCasify, tableInsert)
import "chainweb-storage" Chainweb.Storage.Table.RocksDB (RocksDb, withRocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import "direct-sqlite" Database.SQLite3 qualified as Lite
import "direct-sqlite" Database.SQLite3.Direct (Utf8(..), Database)
import "directory" System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import "filepath" System.FilePath ((</>))
import "lens" Control.Lens (set, over, (^.), _3, view)
import "loglevel" System.LogLevel qualified as LL
import "monad-control" Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import "optparse-applicative" Options.Applicative qualified as O
import "pact" Pact.Types.SQLite (SType(..), RType(..))
import "pact" Pact.Types.SQLite qualified as Pact
import "rocksdb-haskell-kadena" Database.RocksDB.Types (Options(..), Compression(..))
import "streaming" Streaming qualified as S
import "streaming" Streaming.Prelude qualified as S
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "unliftio" UnliftIO.Async (pooledForConcurrently_)
import "yet-another-logger" System.Logger hiding (Logger)
import "yet-another-logger" System.Logger qualified as YAL
import "yet-another-logger" System.Logger.Backend.ColorOption (useColor)
import Chainweb.BlockHash
import Chainweb.BlockHeader (blockHeight, blockHash, blockPayloadHash)
import Chainweb.BlockHeaderDB.Internal (BlockHeaderDb(..), RankedBlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut.CutHashes (cutIdToText)
import Chainweb.CutDB (cutHashesTable)
import Chainweb.Logger (Logger, l2l, setComponent, logFunctionText)
import Chainweb.Pact4.Backend.ChainwebPactDb ()
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Payload.PayloadStore (initializePayloadDb, addNewPayload, lookupPayloadWithHeight)
import Chainweb.Payload.PayloadStore.RocksDB (newPayloadDb)
import Chainweb.Utils (sshow, fromText, toText, int)
import Chainweb.Version (ChainId, ChainwebVersion(..), chainIdToText)
import Chainweb.Version.Guards (minimumBlockHeaderHistory)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Testnet04 (testnet04)
import Chainweb.WebBlockHeaderDB (getWebBlockHeaderDb, initWebBlockHeaderDb)
import Data.LogMessage (SomeLogMessage, logText)

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
            $ over setLoggerScope (("chain", chainIdToText chainId) :)
            $ set setLoggerLevel (l2l ll) l
      f logger
  where
    cid = Text.unpack (chainIdToText chainId)

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
  , compactRocksDb :: Bool
    -- ^ Compact the RocksDB at all?
  , compactPactState :: Bool
    -- ^ Compact the Pact State at all?
  , compactTransactionIndex :: Bool
    -- ^ Compact the TransactionIndex table in the Pact state?
    -- Some APIs (e.g. /poll) rely on this table, so the default is False.
  }

data Retainment = Retainment
  { compactTransactionIndex :: Bool
  , compactThese :: CompactThese
  }

defaultRetainment :: Retainment
defaultRetainment = Retainment
  { compactTransactionIndex = False
  , compactThese = CompactOnlyPactState
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
    parser = do
      chainwebVersion <- parseVersion <$> O.strOption (O.long "chainweb-version" <> O.value "mainnet01")
      fromDir <- O.strOption (O.long "from" <> O.help "Directory containing SQLite Pact state and RocksDB block data to compact (expected to be in $DIR/0/{sqlite,rocksDb}")
      toDir <- O.strOption (O.long "to" <> O.help "Directory where to place the compacted Pact state and block data. It will place them in $DIR/0/{sqlite,rocksDb}, respectively.")
      concurrent <- O.flag SingleChain ManyChainsAtOnce (O.long "parallel" <> O.help "Turn on multi-threaded compaction. The threads are per-chain.")
      logDir <- O.strOption (O.long "log-dir" <> O.help "Directory where compaction logs will be placed.")
      -- Hidden options
      compactRocksDb <- O.switch
        (O.long "compact-rocksdb"
        <> O.help "Compact rocksDB block data. Some interfaces require this data for historical blocks, like the /poll Pact endpoint or the /header Chainweb endpoint, so it is not compacted by default."
        )
      compactPactState <- not <$> O.switch
        (O.long "no-compact-pact"
        <> O.help "Do not compact Pact state. Pact state is not used by any public interface, so it is compacted by default, and the space savings are usually large on mainnet."
        )
      compactTransactionIndex <- O.switch
        (O.long "compact-transaction-index"
        <> O.help "Compact the TransactionIndex table in the Pact state. For historical blocks, the /poll Pact endpoint relies on this table, so it is not compacted by default."
        )
      return Config {..}

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      lookupVersionByName
      . fromMaybe (error "ChainwebVersion parse failed")
      . fromText

main :: IO ()
main = do
  compact =<< getConfig

doCompactPactState :: (Logger logger) => logger -> Retainment -> BlockHeight -> SQLiteEnv -> SQLiteEnv -> IO ()
doCompactPactState logger rt targetBlockHeight srcDb targetDb = do
  let logfun = logFunctionText logger

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
  Pact.runPragmas targetDb fastBulkInsertPragmas

  -- Create checkpointer tables on the target
  createCheckpointerTables targetDb logger

  -- Compact BlockHistory
  -- This is extremely fast and low residency
  do
    logfun LL.Info "Compacting BlockHistory"
    activeRow <- getBlockHistoryRowAt logger srcDb targetBlockHeight
    Pact.exec' targetDb "INSERT INTO BlockHistory VALUES (?1, ?2, ?3)" activeRow

  -- Compact VersionedTableMutation
  -- This is extremely fast and low residency
  do
    logfun LL.Info "Compacting VersionedTableMutation"
    activeRows <- getVersionedTableMutationRowsAt logger srcDb targetBlockHeight
    Lite.withStatement targetDb "INSERT INTO VersionedTableMutation VALUES (?1, ?2)" $ \stmt -> do
      forM_ activeRows $ \row -> do
        Pact.bindParams stmt row
        void $ stepThenReset stmt

  -- Copy over VersionedTableCreation. Read-only rewind needs to know
  -- when the table existed at that time, so we can't compact this.
  --
  -- This is pretty fast and low residency
  do
    logfun LL.Info "Copying over VersionedTableCreation"
    let wholeTableQuery = "SELECT tablename, createBlockheight FROM VersionedTableCreation"
    throwSqlError $ qryStream srcDb wholeTableQuery [] [RText, RInt] $ \tblRows -> do
      Lite.withStatement targetDb "INSERT INTO VersionedTableCreation VALUES (?1, ?2)" $ \stmt -> do
        flip S.mapM_ tblRows $ \row -> do
          Pact.bindParams stmt row
          void $ stepThenReset stmt

  -- Copy over TransactionIndex.
  --
  -- If the user specifies that they want to compact the table, then we do so based on the RocksDB 'blockHeightKeepDepth'.
  --
  -- /poll and SPV rely on having this table synchronised with RocksDB.
  -- We need to document APIs which need TransactionIndex.
  --
  -- Maybe consider
  -- https://tableplus.com/blog/2018/07/sqlite-how-to-copy-table-to-another-database.html
  do
    (qry, args) <-
      if not rt.compactTransactionIndex
      then do
        logfun LL.Info "Copying over entire TransactionIndex table. This could take a while"
        let wholeTableQuery = "SELECT txhash, blockheight FROM TransactionIndex ORDER BY blockheight"
        pure (wholeTableQuery, [])
      else do
        logfun LL.Info "Copying over compacted TransactionIndex"
        let wholeTableQuery = "SELECT txhash, blockheight FROM TransactionIndex WHERE blockheight >= ?1 ORDER BY blockheight"
        pure (wholeTableQuery, [SInt (int (targetBlockHeight - blockHeightKeepDepth))])

    throwSqlError $ qryStream srcDb qry args [RBlob, RInt] $ \tblRows -> do
      Lite.withStatement targetDb "INSERT INTO TransactionIndex VALUES (?1, ?2)" $ \stmt -> do
        -- I experimented a bunch with chunk sizes, to keep transactions
        -- small. As far as I can tell, there isn't really much
        -- difference in any of them wrt residency, but there is wrt
        -- speed. More experimentation may be needed here, but 10k is
        -- fine so far.
        S.chunksOf 10_000 tblRows
          & S.mapsM_ (\chunk -> do
              inTx targetDb $ flip S.mapM_ chunk $ \row -> do
                Pact.bindParams stmt row
                void (stepThenReset stmt)
            )

    -- Vacuuming after copying over all of the TransactionIndex data,
    -- but before creating its indices, makes a big differences in
    -- memory residency (~0.5G), at the expense of speed (~20s increase)
    Pact.exec_ targetDb "VACUUM;"

  -- Create the checkpointer table indices after bulk-inserting into them
  -- This is faster than creating the indices before
  createCheckpointerIndexes targetDb logger

  -- Grab the endingtxid for determining latest state at the
  -- target height
  endingTxId <- getEndingTxId srcDb targetBlockHeight
  logfun LL.Info $ "Ending TxId is " <> sshow endingTxId

  -- Compact all user tables
  logfun LL.Info "Starting user tables"
  getLatestPactTableNamesAt srcDb targetBlockHeight
    & S.mapM_ (\tblname -> do
        compactTable logger srcDb targetDb (fromUtf8 tblname) endingTxId
      )

  logfun LL.Info "Compaction done"

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
compact cfg = do
  let cids = allChains cfg.chainwebVersion

  let _compactThese = case (cfg.compactRocksDb, cfg.compactPactState) of
        (False, False) -> CompactNeither
        (False, True) -> CompactOnlyPactState
        (True, False) -> CompactOnlyRocksDb
        (True, True) -> CompactBoth

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
  when cfg.compactRocksDb $ do
    withRocksDbFileLogger cfg.logDir LL.Debug $ \logger -> do
      withReadOnlyRocksDb (rocksDir cfg.fromDir) modernDefaultOptions $ \srcRocksDb -> do
        withRocksDb (rocksDir cfg.toDir) (modernDefaultOptions { compression = NoCompression }) $ \targetRocksDb -> do
          doCompactRocksDb (set setLoggerLevel (l2l LL.Info) logger) cfg.chainwebVersion cids (targetBlockHeight - blockHeightKeepDepth) srcRocksDb targetRocksDb

  -- Compact the pact state.
  let retainment = Retainment
        { compactTransactionIndex = cfg.compactTransactionIndex
        , compactThese = _compactThese
        }
  when cfg.compactPactState $ do
    forChains_ cfg.concurrent cids $ \cid -> do
      withPerChainFileLogger cfg.logDir cid LL.Debug $ \logger -> do
        withChainDb cid logger (pactDir cfg.fromDir) $ \_ srcDb -> do
          withChainDb cid logger (pactDir cfg.toDir) $ \_ targetDb -> do
            doCompactPactState logger retainment targetBlockHeight srcDb targetDb

compactTable :: (Logger logger)
  => logger      -- ^ logger
  -> Database    -- ^ source database (where we get the active pact state)
  -> Database    -- ^ target database (where we put the compacted state, + use as a rowkey cache)
  -> Text        -- ^ the table we are compacting
  -> Int64       -- ^ target blockheight
  -> IO ()
compactTable logger srcDb targetDb tblname endingTxId = do
  let logfun = logFunctionText logger
  let tblnameUtf8 = toUtf8 tblname

  logfun LL.Info $ "Creating table " <> tblname
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
  logfun LL.Info $ "Creating table indices for " <> tblname
  createUserTableIndex targetDb tblnameUtf8

  -- Create a temporary index on 'rowkey' for a user table, so that upserts work correctly.
  inTx targetDb $ do
    Pact.exec_ targetDb $ mconcat
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
      logfun LL.Info $ "Inserting compacted rows into table " <> tblname

      rs
        & S.chunksOf 10_000
        & S.mapsM_ (\chunk -> do
            inTx targetDb $ flip S.mapM_ chunk $ \row -> do
              case row of
                [SText _, SInt _, SBlob _] -> do
                  Pact.bindParams upsertRow row
                  void $ stepThenReset upsertRow
                _badRowShape -> do
                  exitLog logger "Encountered invalid row shape while compacting"
          )

  -- This index only makes sense during construction of the target database, not after.
  -- If we were to keep this index around, the node would not be able to operate, since
  -- we need to update new rows for the same rowkey.
  inTx targetDb $ do
    Pact.exec_ targetDb $ mconcat
      [ "DROP INDEX IF EXISTS ", tbl (tblnameUtf8 <> "_rowkey_unique_ix_TEMP")
      ]


  case e of
    Left sqlErr -> exitLog logger $ "Encountered SQLite error while compacting: " <> sshow sqlErr
    Right () -> pure ()

  logfun LL.Info $ "Done compacting table " <> tblname

-- | Create all the checkpointer tables
createCheckpointerTables :: (Logger logger)
  => Database
  -> logger
  -> IO ()
createCheckpointerTables db logger = do
  let logfun = logFunctionText logger LL.Info

  logfun "Creating Checkpointer table BlockHistory"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS BlockHistory "
    , "(blockheight UNSIGNED BIGINT NOT NULL"
    , ", hash BLOB NOT NULL"
    , ", endingtxid UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  logfun "Creating Checkpointer table VersionedTableCreation"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableCreation "
    , "(tablename TEXT NOT NULL"
    , ", createBlockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  logfun "Creating Checkpointer table VersionedTableMutation"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableMutation "
    , "(tablename TEXT NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  logfun "Creating Checkpointer table TransactionIndex"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS TransactionIndex "
    , "(txhash BLOB NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  -- We have to delete from these tables because of the way the test harnesses work.
  -- Ideally in the future this can be removed.
  forM_ ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"] $ \tblname -> do
    logfun $ "Deleting from table " <> fromUtf8 tblname
    Pact.exec_ db $ "DELETE FROM " <> tbl tblname

-- | Create all the indexes for the checkpointer tables.
createCheckpointerIndexes :: (Logger logger) => Database -> logger -> IO ()
createCheckpointerIndexes db logger = do
  let logfun = logFunctionText logger LL.Info

  logfun "Creating BlockHistory index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS BlockHistory_blockheight_unique_ix ON BlockHistory (blockheight)"

  logfun "Creating VersionedTableCreation index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableCreation_createBlockheight_tablename_unique_ix ON VersionedTableCreation (createBlockheight, tablename)"

  logfun "Creating VersionedTableMutation index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableMutation_blockheight_tablename_unique_ix ON VersionedTableMutation (blockheight, tablename)"

  logfun "Creating TransactionIndex indexes"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS TransactionIndex_txhash_unique_ix ON TransactionIndex (txhash)"
  inTx db $ Pact.exec_ db
    "CREATE INDEX IF NOT EXISTS TransactionIndex_blockheight_ix ON TransactionIndex (blockheight)"

-- | Create a single user table
createUserTable :: Database -> Utf8 -> IO ()
createUserTable db tblname = do
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS ", tbl tblname, " "
    , "(rowid INTEGER PRIMARY KEY AUTOINCREMENT"
    , ", rowkey TEXT" -- This should be NOT NULL; we need to make a follow-up PR to chainweb-node to update this here and the checkpointer schema
    , ", txid UNSIGNED BIGINT NOT NULL"
    , ", rowdata BLOB NOT NULL"
    , ");"
    ]

  -- We have to delete from the table because of the way the test harnesses work.
  -- Ideally in the future this can be removed.
  Pact.exec_ db $ "DELETE FROM " <> tbl tblname

-- | Create the indexes for a single user table
createUserTableIndex :: Database -> Utf8 -> IO ()
createUserTableIndex db tblname = do
  inTx db $ do
    Pact.exec_ db $ mconcat
      [ "CREATE UNIQUE INDEX IF NOT EXISTS ", tbl (tblname <> "_rowkey_txid_unique_ix"), " ON "
      , tbl tblname, " (rowkey, txid)"
      ]
    Pact.exec_ db $ mconcat
      [ "CREATE INDEX IF NOT EXISTS ", tbl (tblname <> "_txid_ix"), " ON "
      , tbl tblname, " (txid DESC)"
      ]

-- | Returns the active @(blockheight, hash, endingtxid)@ from BlockHistory
getBlockHistoryRowAt :: (Logger logger)
  => logger
  -> Database
  -> BlockHeight
  -> IO [SType]
getBlockHistoryRowAt logger db target = do
  r <- Pact.qry db "SELECT blockheight, hash, endingtxid FROM BlockHistory WHERE blockheight = ?1" [SInt (int target)] [RInt, RBlob, RInt]
  case r of
    [row@[SInt bh, SBlob _hash, SInt _endingTxId]] -> do
      unless (target == int bh) $ do
        exitLog logger "BlockHeight mismatch in BlockHistory query. This is a bug in the compaction tool. Please report it on the issue tracker or discord."
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
  r <- Pact.qry db "SELECT tablename, blockheight FROM VersionedTableMutation WHERE blockheight = ?1" [SInt (int target)] [RText, RInt]
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
  let logfun = logFunctionText logger

  let logger' = set setLoggerLevel (l2l LL.Error) logger
  latestCommon <- getLatestCommonBlockHeight logger' dbDir cids
  earliestCommon <- getEarliestCommonBlockHeight logger' dbDir cids

  logfun LL.Debug $ "Latest Common BlockHeight: " <> sshow latestCommon
  logfun LL.Debug $ "Earliest Common BlockHeight: " <> sshow earliestCommon

  -- Make sure we have at least 1k blocks of depth for prod.
  -- In devnet or testing versions we don't care.
  let safeDepth :: BlockHeight
      safeDepth
        | v == mainnet || v == testnet04 = BlockHeight 1_000
        | otherwise = BlockHeight 0

  when (latestCommon - earliestCommon < safeDepth) $ do
    exitLog logger "locateLatestSafeTarget: Not enough history to safely compact. Aborting."

  let target = latestCommon - safeDepth
  logfun LL.Debug $ "Compaction target blockheight is: " <> sshow target
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
    (Pact.exec_ db "BEGIN;")
    (Pact.exec_ db "COMMIT;")
    io

pactDir :: FilePath -> FilePath
pactDir db = db </> "0/sqlite"

rocksDir :: FilePath -> FilePath
rocksDir db = db </> "0/rocksDb"

-- | Copy over all CutHashes, all BlockHeaders, and only some Payloads.
doCompactRocksDb :: (Logger logger)
  => logger
  -> ChainwebVersion -- ^ cw version
  -> [ChainId] -- ^ ChainIds
  -> BlockHeight -- ^ minBlockHeight for payload copying
  -> RocksDb -- ^ source db, should be opened read-only
  -> RocksDb -- ^ target db
  -> IO ()
doCompactRocksDb logger cwVersion cids minBlockHeight srcDb targetDb = do
  let logfun = logFunctionText logger

  -- Copy over entirety of CutHashes table
  let srcCutHashes = cutHashesTable srcDb
  let targetCutHashes = cutHashesTable targetDb
  logfun LL.Info "Copying over CutHashes table"
  withTableIterator (unCasify srcCutHashes) $ \srcIt -> do
    let go = do
          iterEntry srcIt >>= \case
            Nothing -> do
              pure ()
            Just (Entry k v) -> do
              logfun LL.Debug $ "Copying over Cut " <> cutIdToText (k ^. _3)
              tableInsert targetCutHashes k v
              iterNext srcIt
              go
    go

  -- Migrate BlockHeaders and Payloads
  let srcPayloads = newPayloadDb srcDb
  let targetPayloads = newPayloadDb targetDb

  -- The target payload db has to be initialised.
  logfun LL.Info "Initializing payload db"
  initializePayloadDb cwVersion targetPayloads

  srcWbhdb <- initWebBlockHeaderDb srcDb cwVersion
  targetWbhdb <- initWebBlockHeaderDb targetDb cwVersion
  forM_ cids $ \cid -> do
    let log' = logFunctionText (addChainIdLabel cid logger)
    log' LL.Info $ "Starting chain " <> chainIdToText cid
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
      case minimumBlockHeaderHistory cwVersion latestHeader of
        -- Go to the earliest possible entry. We migrate all BlockHeaders, for now.
        -- They are needed for SPV.
        --
        -- Constructing SPV proofs actually needs the payloads, but validating
        -- them does not.
        Nothing -> do
          iterFirst it

        Just minBlockHeaderHistory -> do
          let runBack =
                let x = int minBlockHeight
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
