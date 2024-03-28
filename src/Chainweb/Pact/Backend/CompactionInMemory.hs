{-# language
    BangPatterns
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
#-}

{-# options_ghc -fno-warn-unused-imports #-}

-- TODO: check size of compacted db (sigma vs old)
--
module Chainweb.Pact.Backend.CompactionInMemory
  ( main
  )
  where

import "base" System.Exit (ExitCode(..), exitFailure, exitWith)
import "loglevel" System.LogLevel qualified as LL
import "yet-another-logger" System.Logger hiding (Logger)
import "yet-another-logger" System.Logger qualified as YAL
import Prelude hiding (log)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (Logger, l2l, setComponent, logFunctionText)
import Chainweb.Pact.Backend.ChainwebPactDb ()
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Utils (fromUtf8, toUtf8, withSqliteDb)
import Chainweb.Utils (sshow, fromText, toText, int)
import Chainweb.Version (ChainId, ChainwebVersion(..), unsafeChainId, chainIdToText)
import Chainweb.Version.Development (devnet)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Testnet (testnet)
import Chainweb.Version.Utils (chainIdsAt)
import Chronos qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (swapMVar, readMVar, newMVar)
import Control.Exception (Exception, SomeException(..), finally)
import Control.Lens (makeLenses, set, over, view, (^.), _2, (^?!), ix)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Csv (ToRecord(..))
import Data.Csv qualified as Csv
import Data.Csv.Builder qualified as Csv
import Data.Foldable qualified as F
import Data.Function (fix, (&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, readIORef, newIORef, atomicModifyIORef')
import Data.Int (Int64)
import Data.List qualified as List
import Data.LogMessage
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import Database.SQLite3 qualified as Lite
import Database.SQLite3.Direct (Utf8(..), Database)
import GHC.Stack (HasCallStack)
import Options.Applicative qualified as O
import Pact.Types.Persistence (TxId(..))
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), addExtension)
import System.IO (Handle)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Logger.Backend.ColorOption (useColor)
import System.Process (shell, readCreateProcessWithExitCode)
import UnliftIO.Async (pooledMapConcurrentlyN_)

withDefaultLogger :: LL.LogLevel -> (YAL.Logger SomeLogMessage -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend_ logText handleCfg $ \b ->
  withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel (l2l ll) l)
  where
    handleCfg = defaultHandleBackendConfig
      { _handleBackendConfigHandle = StdErr
      }

newtype TableName = TableName { getTableName :: Utf8 }
  deriving newtype (Eq, Ord)
  deriving stock (Show)

data Config = Config
  { chainwebVersion :: ChainwebVersion
  , sourceDir :: FilePath
  , targetDir :: FilePath
  }

getConfig :: IO Config
getConfig = do
  O.execParser opts
  where
    opts :: O.ParserInfo Config
    opts = O.info (parser O.<**> O.helper)
      (O.fullDesc <> O.progDesc "Pact DB Compaction Tool - create a compacted copy of the source directory Pact DB into the target directory.")

    parser :: O.Parser Config
    parser = Config
      <$> (parseVersion <$> O.strOption (O.long "chainweb-version"))
      <*> O.strOption (O.long "source-directory")
      <*> O.strOption (O.long "target-directory")

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      lookupVersionByName
      . fromMaybe (error "ChainwebVersion parse failed")
      . fromText

main :: IO ()
main = do
  compact =<< getConfig

-- demo: compact a single chain.
compact :: Config -> IO ()
compact cfg = do
  -- TODO (chessai): fail if targetDir exists at all

  -- TODO (chessai): these may need tuning
  --
  -- journal_mode = OFF is terrible for prod but probably OK here
  -- since we are just doing a bunch of bulk inserts
  let fastBulkInsertPragmas =
        [ "journal_mode = OFF"
        , "synchronous = 0"
        , "cache_size = 1000000"
        , "temp_store = MEMORY"
        ]

  withDefaultLogger LL.Debug $ \logger -> do
    let cids = allChains cfg.chainwebVersion
    targetBlockHeight <- locateLatestSafeTarget logger cfg.chainwebVersion cfg.sourceDir cids

    -- Create the target directory, and its parents too (if missing)
    createDirectoryIfMissing True cfg.targetDir

    forM_ cids $ \cid -> do
      withChainDb cid logger cfg.sourceDir $ \_ srcDb -> do
        withChainDb cid logger cfg.targetDir $ \_ targetDb -> do
          let log = logFunctionText logger

          -- Establish pragmas for bulk insert performance
          --
          -- Note that we can't apply pragmas to the src
          -- because we can't guarantee it's not being accessed.
          Pact.runPragmas targetDb fastBulkInsertPragmas

          -- Create checkpointer tables on the target
          createCheckpointerTables targetDb logger

          -- TODO (chessai): when compacting system tables,
          -- we may want to try a few things to improve performance:
          -- 1. creating all indices on them after inserting
          -- 2. import-from-sql (is this a thing)?

          -- Compact BlockHistory
          do
            log LL.Info "Compacting BlockHistory"
            activeRow <- getBlockHistoryRowAt logger srcDb targetBlockHeight
            Pact.exec' targetDb "INSERT INTO BlockHistory VALUES (?1, ?2, ?3)" activeRow

          -- Compact VersionedTableMutation
          do
            log LL.Info "Compacting VersionedTableMutation"
            activeRows <- getVersionedTableMutationRowsAt logger srcDb targetBlockHeight
            Lite.withStatement targetDb "INSERT INTO VersionedTableMutation VALUES (?1, ?2)" $ \stmt -> do
              forM_ activeRows $ \row -> do
                Pact.bindParams stmt row
                void $ stepThenReset stmt

          -- Compact user tables
          log LL.Debug "Starting user tables"
          withLatestPactStateAt srcDb targetBlockHeight $ \tblname tblRows -> do
            let tblnameUtf8 = toUtf8 tblname

            log LL.Info $ "Creating table " <> tblname
            createUserTable targetDb tblnameUtf8

            log LL.Info $ "Inserting compacted rows into " <> tblname

            let qryText = "INSERT INTO " <> fromUtf8 (tbl tblnameUtf8) <> " VALUES (?1, ?2, ?3)"
            Lite.withStatement targetDb qryText $ \stmt -> do
              void $ flip S.mapM_ tblRows $ \pr -> do
                let row = [SText (Utf8 pr.rowKey), SInt pr.txId, SBlob pr.rowData]
                Pact.bindParams stmt row
                void $ stepThenReset stmt

            log LL.Info $ "Creating table indices for " <> tblname
            createUserTableIndex targetDb tblnameUtf8

-- TODO: use initSchema
createCheckpointerTables :: (Logger logger)
  => Database
  -> logger
  -> IO ()
createCheckpointerTables db logger = do
  let log = logFunctionText logger LL.Info

  log "Creating Checkpointer table BlockHistory"
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS BlockHistory "
    , "(blockheight UNSIGNED BIGINT NOT NULL"
    , ", hash BLOB NOT NULL"
    , ", endingtxid UNSIGNED BIGINT NOT NULL"
    , ", CONSTRAINT blockHashConstraint UNIQUE (blockheight)"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableCreation"
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableCreation "
    , "(tablename TEXT NOT NULL"
    , ", createBlockheight UNSIGNED BIGINT NOT NULL"
    , ", CONSTRAINT creation_unique UNIQUE(createBlockheight, tablename)"
    , ");"
    ]

  log "Creating Checkpointer table VersionedTableMutation"
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS VersionedTableMutation "
    , "(tablename TEXT NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ", CONSTRAINT mutation_unique UNIQUE(blockheight, tablename)"
    , ");"
    ]

  log "Creating Checkpointer table TransactionIndex"
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS TransactionIndex "
    , "(txhash BLOB NOT NULL"
    , ", blockheight UNSIGNED BIGINT NOT NULL"
    , ", CONSTRAINT transactionIndexConstraint UNIQUE(txhash)"
    , ");"
    ]
  Pact.exec_ db $ mconcat
    [ "CREATE INDEX IF NOT EXISTS "
    , "transactionIndexByBH ON TransactionIndex(blockheight);"
    ]

  forM_ ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"] $ \tblname -> do
    log $ "Deleting from table " <> fromUtf8 tblname
    Pact.exec_ db $ "DELETE FROM " <> tbl tblname

createUserTable :: Database -> Utf8 -> IO ()
createUserTable db tblname = do
  Pact.exec_ db $ mconcat
    [ "CREATE TABLE IF NOT EXISTS ", tbl tblname, " "
    , "(rowkey TEXT" -- investigate making this NOT NULL
    , ", txid UNSIGNED BIGINT NOT NULL"
    , ", rowdata BLOB NOT NULL"
    -- , ", UNIQUE (rowkey, txid)"
    , ");"
    ]

  Pact.exec_ db $ "DELETE FROM " <> tbl tblname

createUserTableIndex :: Database -> Utf8 -> IO ()
createUserTableIndex db tblname = do
  Pact.exec_ db $ mconcat
    [ "CREATE UNIQUE INDEX IF NOT EXISTS ", tbl (tblname <> "rowkey_txid_unique_ix"), " ON "
    , tbl tblname, " (rowkey, txid)"
    ]
  Pact.exec_ db $ mconcat
    [ "CREATE INDEX IF NOT EXISTS ", tbl (tblname <> "_ix"), " ON "
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
        exitLog logger "BlockHeight mismatch in BlockHistory query. This is a bug in the compaction tool. Please report it."
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

locateLatestSafeTarget :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath
  -> [ChainId]
  -> IO BlockHeight
locateLatestSafeTarget logger v dbDir cids = do
  let logger' = set setLoggerLevel (l2l LL.Error) logger
  let log = logFunctionText logger

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

  pure (latestCommon - safeDepth)

exitLog :: (Logger logger)
  => logger
  -> Text
  -> IO a
exitLog logger msg = do
  logFunctionText logger LL.Error msg
  exitFailure

stepThenReset :: Lite.Statement -> IO Lite.StepResult
stepThenReset stmt = do
  Lite.stepNoCB stmt `finally` Lite.reset stmt
