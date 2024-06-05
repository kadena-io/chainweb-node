{-# language
    BangPatterns
  , CPP
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
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
#-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -Wwarn #-}

module Chainweb.Pact.Backend.CompactionInMemory
  ( main
  )
  where

import Data.Semigroup (Min(..))
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar, retry)
import Control.Exception hiding (Handler)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import System.Mem.Weak (deRefWeak)

#if !mingw32_HOST_OS
import System.Posix.Signals
#else
import Foreign.Ptr
#endif

import System.ProgressBar qualified as ProgressBar
import System.ProgressBar (Progress(..), newProgressBar)
import "base" System.Exit (exitFailure, exitSuccess)
import "loglevel" System.LogLevel qualified as LL
import "yet-another-logger" System.Logger hiding (Logger)
import "yet-another-logger" System.Logger qualified as YAL
import Chainweb.BlockHash (nullBlockHash)
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeaderDB.Internal (BlockHeaderDb(..), RankedBlockHash(..), RankedBlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.CutDB (cutHashesTable)
import Chainweb.Graph (diameter)
import Chainweb.Logger (Logger, l2l, setComponent, logFunctionText)
import Chainweb.Pact.Backend.ChainwebPactDb ()
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Utils (fromUtf8, toUtf8)
import Chainweb.Payload.PayloadStore (initializePayloadDb, addNewPayload, lookupPayloadWithHeight)
import Chainweb.Payload.PayloadStore.RocksDB (newPayloadDb)
import Chainweb.Storage.Table (Iterator(..), Entry(..), withTableIterator, unCasify, tableInsert)
import Chainweb.Storage.Table.RocksDB (RocksDb, RocksDbTableIter(..), withRocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Utils (sshow, fromText, int)
import Chainweb.Version (ChainId, ChainwebVersion(..), chainIdToText, chainGraphAt)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Testnet (testnet)
import Chainweb.WebBlockHeaderDB (WebBlockHeaderDb, getWebBlockHeaderDb, initWebBlockHeaderDb)
import Control.Concurrent (forkIO, threadDelay, myThreadId, mkWeakThreadId)
import Control.Concurrent.MVar (swapMVar, readMVar, newMVar)
import Control.Exception (bracket_, finally)
import Control.Lens (set, over, (^.))
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC8
import Data.Function (fix, (&))
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.LogMessage (SomeLogMessage, logText)
import Data.LruCache (LruCache)
import Data.LruCache qualified as Lru
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Database.RocksDB.Iterator (iterGetError)
import Database.RocksDB.Types (Options(..), Compression(..))
import Database.SQLite3 qualified as Lite
import Database.SQLite3 qualified as SQL
import Database.SQLite3.Direct (Utf8(..), Database)
import Options.Applicative qualified as O
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Prelude hiding (log)
import Streaming (Stream, Of)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO (Handle)
import System.IO qualified as IO
import System.Logger.Backend.ColorOption (useColor)
import UnliftIO.Async (pooledForConcurrently_)

withDefaultLogger :: LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend_ logText handleCfg $ \b ->
  withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel (l2l ll) l)
  where
    handleCfg = defaultHandleBackendConfig
      { _handleBackendConfigHandle = StdErr
      }

withPerChainFileLogger :: FilePath -> ChainId -> LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withPerChainFileLogger ld chainId ll f = do
  createDirectoryIfMissing True {- do create parents -} ld
  let logFile = ld </> ("chain-" <> cid <> ".log")
  let handleConfig = defaultHandleBackendConfig
        { _handleBackendConfigHandle = FileHandle logFile
        }
  withHandleBackend_' logText handleConfig $ \h b -> do

    done <- newMVar False
    void $ forkIO $ fix $ \go -> do
      doneYet <- readMVar done
      let flush = do
            w <- IO.hIsOpen h
            when w (IO.hFlush h)
      unless doneYet $ do
        flush
        threadDelay 5_000_000
        go
      flush

    withLogger defaultLoggerConfig b $ \l -> do
      let logger = setComponent "compaction"
            $ over setLoggerScope (("chain", chainIdToText chainId) :)
            $ set setLoggerLevel (l2l ll) l
      a <- f logger
      void $ swapMVar done True
      pure a
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

newtype TableName = TableName { getTableName :: Utf8 }
  deriving newtype (Eq, Ord)
  deriving stock (Show)

data Config = Config
  { chainwebVersion :: ChainwebVersion
  , sourcePactDir :: FilePath
  , targetPactDir :: FilePath
  , sourceRocksDir :: FilePath
  , targetRocksDir :: FilePath
  , concurrent :: ConcurrentChains
  , logDir :: FilePath
  , onlyRocksDb :: Bool
  }

data ConcurrentChains = SingleChain | ManyChainsAtOnce

getConfig :: IO Config
getConfig = do
  O.execParser opts
  where
    opts :: O.ParserInfo Config
    opts = O.info (parser O.<**> O.helper)
      (O.fullDesc <> O.progDesc "Pact DB Compaction Tool - create a compacted copy of the source directory Pact DB into the target directory.")

    parser :: O.Parser Config
    parser = Config
      <$> (parseVersion <$> O.strOption (O.long "chainweb-version" <> O.value "mainnet01"))
      <*> O.strOption (O.long "source-sqlite-directory")
      <*> O.strOption (O.long "target-sqlite-directory")
      <*> O.strOption (O.long "source-rocksdb-directory")
      <*> O.strOption (O.long "target-rocksdb-directory")
      <*> O.flag SingleChain ManyChainsAtOnce (O.long "parallel")
      <*> O.strOption (O.long "log-dir")
      <*> O.switch (O.long "only-rocksdb" <> O.hidden)

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      lookupVersionByName
      . fromMaybe (error "ChainwebVersion parse failed")
      . fromText

main :: IO ()
main = do
  --status <- newTVarIO Running
  --installFatalSignalHandlers status [ sigHUP, sigTERM, sigXCPU, sigXFSZ ]

  compact =<< getConfig

compact :: Config -> IO ()
compact cfg = do
  let cids = allChains cfg.chainwebVersion

  -- Get the target blockheight.
  targetBlockHeight <- withDefaultLogger LL.Error $ \logger -> do
    -- Locate the latest (safe) blockheight as per the pact state.
    -- See 'locateLatestSafeTarget' for the definition of 'safe' here.
    targetBlockHeight <- locateLatestSafeTarget logger cfg.chainwebVersion cfg.sourcePactDir cids

    when (not cfg.onlyRocksDb) $ do
      -- Check that the target sqlite directory doesn't exist already,
      -- then create it.
      targetDirExists <- doesDirectoryExist cfg.targetPactDir
      when targetDirExists $ do
        exitLog logger "Target SQLite directory already exists. Aborting."
      createDirectoryIfMissing True cfg.targetPactDir

    -- Check that the target rocksdb directory doesn't exist already,
    -- then create it.
    targetRocksDirExists <- doesDirectoryExist cfg.targetRocksDir
    when targetRocksDirExists $ do
      exitLog logger "Target RocksDB directory already exists. Aborting."
    createDirectoryIfMissing True cfg.targetRocksDir

    pure targetBlockHeight

  -- TODO: this logic is brittle right now, we need to make sure
  -- we have at least this amount, or abort.
  --
  -- TODO: make 1k/3k constants; no magic numbers. perhaps these constants
  -- should be shared with locateLatestSafeTarget. The 2k can be derived
  -- from that and a constant for the 3k.

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
  let minBlockHeight = targetBlockHeight - 2_000
  let maxBlockHeight = targetBlockHeight + 1_000 + int (diameter (chainGraphAt cfg.chainwebVersion targetBlockHeight))

  -- Trim RocksDB here
  withReadOnlyRocksDb cfg.sourceRocksDir modernDefaultOptions $ \srcRocksDb -> do
    withRocksDb cfg.targetRocksDir (modernDefaultOptions { compression = NoCompression }) $ \targetRocksDb -> do
      trimRocksDb cfg.chainwebVersion cids minBlockHeight maxBlockHeight srcRocksDb targetRocksDb

  when cfg.onlyRocksDb exitSuccess

  -- These pragmas are tuned for fast insertion on systems with a wide range
  -- of resources.
  --
  -- journal_mode = OFF is terrible for prod but probably OK here
  -- since we are just doing a bunch of bulk inserts
  let fastBulkInsertPragmas =
        [ "journal_mode = OFF"
        , "synchronous = OFF"
        , "cache_size = -9766" -- 10 Megabytes
        , "temp_store = FILE"
        , "shrink_memory"
        ]

  forChains_ cfg.concurrent cids $ \cid -> do
    withPerChainFileLogger cfg.logDir cid LL.Debug $ \logger -> do
      withChainDb cid logger cfg.sourcePactDir $ \_ srcDb -> do
        withChainDb cid logger cfg.targetPactDir $ \_ targetDb -> do
          let log = logFunctionText logger

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
            log LL.Info "Compacting BlockHistory"
            activeRow <- getBlockHistoryRowAt logger srcDb targetBlockHeight
            Pact.exec' targetDb "INSERT INTO BlockHistory VALUES (?1, ?2, ?3)" activeRow

          -- Compact VersionedTableMutation
          -- This is extremely fast and low residency
          do
            log LL.Info "Compacting VersionedTableMutation"
            activeRows <- getVersionedTableMutationRowsAt logger srcDb targetBlockHeight
            Lite.withStatement targetDb "INSERT INTO VersionedTableMutation VALUES (?1, ?2)" $ \stmt -> do
              forM_ activeRows $ \row -> do
                Pact.bindParams stmt row
                void $ stepThenReset stmt

          -- Copy over VersionedTableCreation (it isn't compacted)
          -- This is pretty fast and low residency
          do
            log LL.Info "Copying over VersionedTableCreation"
            let wholeTableQuery = "SELECT tablename, createBlockheight FROM VersionedTableCreation"
            throwSqlError $ qryStream srcDb wholeTableQuery [] [RText, RInt] $ \tblRows -> do
              Lite.withStatement targetDb "INSERT INTO VersionedTableCreation VALUES (?1, ?2)" $ \stmt -> do
                flip S.mapM_ tblRows $ \row -> do
                  Pact.bindParams stmt row
                  void $ stepThenReset stmt

          -- Copy over TransactionIndex
          --
          -- It isn't (currently) compacted so the amount of data is quite large
          -- This, SYS:Pacts, and coin_coin-table were all used as benchmarks
          -- for optimisations.
          --
          -- Maybe consider
          -- https://tableplus.com/blog/2018/07/sqlite-how-to-copy-table-to-another-database.html
          do
            log LL.Info "Copying over TransactionIndex"
            let wholeTableQuery = "SELECT txhash, blockheight FROM TransactionIndex WHERE blockheight >= ?1 ORDER BY blockheight"

            throwSqlError $ qryStream srcDb wholeTableQuery [SInt (int minBlockHeight)] [RBlob, RInt] $ \tblRows -> do
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

          -- Compact all user tables
          log LL.Info "Starting user tables"
          getLatestPactTableNamesAt srcDb targetBlockHeight
            & S.mapM_ (\tblname -> do
                compactTable logger srcDb targetDb (fromUtf8 tblname) targetBlockHeight
              )

          log LL.Info "Compaction done"

compactTable :: (Logger logger)
  => logger      -- ^ logger
  -> Database    -- ^ source database (where we get the active pact state)
  -> Database    -- ^ target database (where we put the compacted state, + use as a rowkey cache)
  -> Text        -- ^ the table we are compacting
  -> BlockHeight -- ^ target blockheight
  -> IO ()
compactTable logger srcDb targetDb tblname targetBlockHeight = do
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
  -- outweighs the insert slowdowns imposed by the indices. However, because
  -- the SQLite-based rowkey cache is still somewhat slow, we also employ
  -- an in-memory LRU cache that is checked beforehand. The LRU cache stores
  -- hashes of the rowkeys, so the unbounded size of rowkeys is not a problem
  -- for its residency. Though, at any point we could encounter a very large
  -- rowkey.
  log LL.Info $ "Creating table indices for " <> tblname
  createUserTableIndex targetDb tblnameUtf8

  -- Grab the endingtxid for determining latest state at the
  -- target height
  endingTxId <- getEndingTxId srcDb targetBlockHeight

  -- | Get the active pact state (a 'Stream' of 'PactRow').
  --   Uses an LruCache of RowKeys - the LRU Cache internally just stores
  --   hashes.
  let getActiveState :: ()
        => Lite.Statement
        -> LruCache ByteString ()
        -> Stream (Of [SType]) IO (Either SQL.Error ())
        -> Stream (Of PactRow) IO ()
      getActiveState rkSqliteCache = go
        where
          go rkMemCache s = do
            e <- liftIO (S.next s)
            case e of
              Left (Left sqlErr) -> do
                liftIO $ exitLog logger $ "Encountered SQL Error during getActiveState: " <> sshow sqlErr
              Left (Right ()) -> do
                pure ()
              Right (row, rest) -> do
                case row of
                  [SText (Utf8 rk), SInt tid, SBlob rd] -> do
                    -- Lookup in the LRU Cache, then fall back to the SQLite
                    -- cache
                    (maybeRow, newRkCache) <- liftIO $ do
                      case Lru.lookup rk rkMemCache of
                       Nothing -> do
                         inTargetDb <- do
                           Pact.qrys rkSqliteCache [SText (Utf8 rk)] [RInt] >>= \case
                             [] -> pure False
                             [[SInt 1]] -> pure True
                             _ -> exitLog logger "getActiveState: invalid membership query"
                         let !newCache = Lru.insert rk () rkMemCache
                         if inTargetDb
                         then do
                           pure (Nothing, newCache)
                         else do
                           pure (Just (PactRow rk rd tid), newCache)
                       Just ((), newCache) -> do
                         pure (Nothing, newCache)

                    -- Yield the row if it was found
                    forM_ maybeRow $ \r -> S.yield r

                    -- Recurse with the new cache
                    go newRkCache rest
                  _ -> do
                    liftIO $ exitLog logger "Encountered invalid row shape during getActiveState"

  -- This query gets all rows at or below (older than) the target blockheight
  let activeStateQryText = "SELECT rowkey, txid, rowdata FROM "
              <>  "[" <> tblnameUtf8 <> "]"
              <> " WHERE txid < ?1"
              <> " ORDER BY rowid DESC" -- txid ordering agrees with rowid ordering, but rowid is much faster
  let activeStateQryArgs = [SInt endingTxId]
  let activeStateQryRetTypes = [RText, RInt, RBlob]

  -- This query checks for rowkey membership in the target db (implements the
  -- SQLite part of the rowkey cache)
  let checkTargetForRkText = "SELECT 1 FROM [" <> tblname <> "] WHERE rowkey = ?1"
  -- This query inserts rows into the target.
  -- I tried using bulk inserts and it didn't make a difference.
  let insertQryText = "INSERT INTO " <> fromUtf8 (tbl tblnameUtf8) <> " (rowkey, txid, rowdata) VALUES (?1, ?2, ?3)"
  -- The LRU rowkey cache. Right now a capacity of 500 is used. This was just
  -- determined by vibes. It can probably be larger because it just contains
  -- hashes.
  let rkMemCache = Lru.empty @ByteString @() 500
  qryStream srcDb activeStateQryText activeStateQryArgs activeStateQryRetTypes $ \rs -> do
    Lite.withStatement targetDb checkTargetForRkText $ \rkSqliteCacheStmt -> do
      Lite.withStatement targetDb insertQryText $ \insertStmt -> do
        log LL.Info $ "Inserting compacted rows into table " <> tblname
        getActiveState rkSqliteCacheStmt rkMemCache rs
          -- Need to experiment more with chunk sizes. Trying to keep
          -- transaction residency low while balancing speed.
          --
          -- Perhaps instead of number of rows, this needs to be based
          -- on the cumulative size (in bytes) of the rowkey+rowdata
          & S.chunksOf 10_000
          & S.mapsM_ (\chunk -> do
              inTx targetDb $ flip S.mapM_ chunk $ \pr -> do
                let row = [SText (Utf8 pr.rowKey), SInt pr.txId, SBlob pr.rowData]
                Pact.bindParams insertStmt row
                void $ stepThenReset insertStmt
            )

  log LL.Info $ "Done compacting table " <> tblname

-- | Create all the checkpointer tables
createCheckpointerTables :: (Logger logger)
  => Database
  -> logger
  -> IO ()
createCheckpointerTables db logger = do
  let log = logFunctionText logger LL.Info

  log "Creating Checkpointer table BlockHistory"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS BlockHistory "
    , "(blockheight UNSIGNED BIGINT NOT NULL"
    , ", hash BLOB NOT NULL"
    , ", endingtxid UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableCreation"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableCreation "
    , "(tablename TEXT NOT NULL"
    , ", createBlockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableMutation"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableMutation "
    , "(tablename TEXT NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  log "Creating Checkpointer table TransactionIndex"
  inTx db $ Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS TransactionIndex "
    , "(txhash BLOB NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ");"
    ]

  forM_ ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"] $ \tblname -> do
    log $ "Deleting from table " <> fromUtf8 tblname
    Pact.exec_ db $ "DELETE FROM " <> tbl tblname

-- | Create all the indexes for the checkpointer tables.
createCheckpointerIndexes :: (Logger logger) => Database -> logger -> IO ()
createCheckpointerIndexes db logger = do
  let log = logFunctionText logger LL.Info

  log "Creating BlockHistory index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS BlockHistory_blockheight_unique_ix ON BlockHistory (blockheight)"

  log "Creating VersionedTableCreation index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableCreation_createBlockheight_tablename_unique_ix ON VersionedTableCreation (createBlockheight, tablename)"

  log "Creating VersionedTableMutation index"
  inTx db $ Pact.exec_ db
    "CREATE UNIQUE INDEX IF NOT EXISTS VersionedTableMutation_blockheight_tablename_unique_ix ON VersionedTableMutation (blockheight, tablename)"

  log "Creating TransactionIndex indexes"
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
    , ", rowkey TEXT" -- This should probably be NOT NULL, but we have no proof of that, so for now this is just kept the same as chainweb-node's implementation.
    , ", txid UNSIGNED BIGINT NOT NULL"
    , ", rowdata BLOB NOT NULL"
    , ");"
    ]

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

tbl :: Utf8 -> Utf8
tbl u = "[" <> u <> "]"

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
        | v == mainnet || v == testnet = BlockHeight 1_000
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
    (Pact.exec_ db "BEGIN;")
    (Pact.exec_ db "COMMIT;")
    io

-- | Copy over all CutHashes, all BlockHeaders, and only some Payloads.
trimRocksDb :: ()
  => ChainwebVersion -- ^ cw version
  -> [ChainId] -- ^ ChainIds
  -> BlockHeight -- ^ minBlockHeight
  -> BlockHeight -- ^ maxBlockHeight
  -> RocksDb -- ^ source db, should be opened read-only
  -> RocksDb -- ^ target db
  -> IO ()
trimRocksDb cwVersion cids minBlockHeight maxBlockHeight srcDb targetDb = do

  -- Copy over entirety of CutHashes table
  let srcCutHashes = cutHashesTable srcDb
  let targetCutHashes = cutHashesTable targetDb
  withTableIterator (unCasify srcCutHashes) $ \srcIt -> do
    let go = do
          iterEntry srcIt >>= \case
            Nothing -> do
              pure ()
            Just (Entry k v) -> do
              tableInsert targetCutHashes k v
              iterNext srcIt
              go
    go

  -- Migrate BlockHeaders and Payloads
  let srcPayloads = newPayloadDb srcDb
  let targetPayloads = newPayloadDb targetDb

  -- The target payload db has to be initialised.
  initializePayloadDb cwVersion targetPayloads

  srcWbhdb <- initWebBlockHeaderDb srcDb cwVersion
  targetWbhdb <- initWebBlockHeaderDb targetDb cwVersion
  forM_ cids $ \cid -> do
    srcBlockHeaderDb <- getWebBlockHeaderDb srcWbhdb cid
    targetBlockHeaderDb <- getWebBlockHeaderDb targetWbhdb cid

    withTableIterator (_chainDbCas srcBlockHeaderDb) $ \it -> do
      -- Go to the earliest entry. We migrate all BlockHeaders, for now
      iterFirst it

      let go = do
            iterValue it >>= \case
              Nothing -> do
                pure ()
              Just rankedBlockHeader -> do
                let blockHeader = _getRankedBlockHeader rankedBlockHeader
                let blockHeight = _blockHeight blockHeader
                let blockHash   = _blockHash blockHeader

                -- Migrate the ranked block table and rank table
                -- unconditionally.
                -- Right now, the headers are definitely needed (we can't delete any).
                --
                -- Not sure about the rank table, though. We keep it to be
                -- conservative.
                tableInsert (_chainDbCas targetBlockHeaderDb) (RankedBlockHash blockHeight blockHash) rankedBlockHeader
                tableInsert (_chainDbRankTable targetBlockHeaderDb) blockHash blockHeight

                -- We only add the payloads for blocks that are in the
                -- interesting range.
                when (blockHeight >= minBlockHeight && blockHeight <= maxBlockHeight) $ do
                  -- Insert the payload into the new database
                  lookupPayloadWithHeight srcPayloads (Just blockHeight) (_blockPayloadHash blockHeader) >>= \case
                    Nothing -> do
                      error "Missing payload: This is likely due to a corrupted database."
                    Just payloadWithOutputs -> do
                      addNewPayload targetPayloads blockHeight payloadWithOutputs

                iterNext it
                go
      go
