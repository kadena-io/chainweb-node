{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Pact.Backend.Compaction
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: see LICENSE.md
--
-- Compact Checkpointer PactDbs by culling old journal rows.
--

module Chainweb.Pact.Backend.Compaction
  ( CompactFlag(..)
  , TargetBlockHeight(..)
  , compact
  , main

    -- * Used in various tools
  , withDefaultLogger
  , withPerChainFileLogger
  ) where

import Chronos qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (swapMVar, readMVar, newMVar)
import Control.Exception (Exception, SomeException(..))
import Control.Lens (makeLenses, set, over, view, (^.), _2)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Data.Foldable qualified as F
import Data.Function (fix)
import Data.IORef (IORef, readIORef, newIORef, atomicModifyIORef')
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.SQLite3.Direct (Utf8(..), Database)
import GHC.Stack (HasCallStack)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async (pooledMapConcurrentlyN_)

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (l2l, setComponent)
import Chainweb.Utils (sshow, HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainId, ChainwebVersion(..), ChainwebVersionName, unsafeChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, withSqliteDb)

import "yet-another-logger" System.Logger
import "loglevel" System.LogLevel qualified as LL
import System.Logger.Backend.ColorOption (useColor)
import Data.LogMessage

import Pact.Types.Persistence (TxId(..))
import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact

newtype TableName = TableName { getTableName :: Utf8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data CompactException
  = CompactExceptionInternal !Text
  | CompactExceptionDb !SomeException
  | CompactExceptionInvalidBlockHeight !BlockHeight
  | CompactExceptionTableVerificationFailure !TableName
  | CompactExceptionNoLatestBlockHeight
  deriving stock (Show)
  deriving anyclass (Exception)

data CompactFlag
  = KeepCompactTables
    -- ^ Keep compaction tables post-compaction for inspection.
  | NoVacuum
    -- ^ Don't VACUUM database
  deriving stock (Eq, Show)

internalError :: MonadThrow m => Text -> m a
internalError = throwM . CompactExceptionInternal

data CompactEnv = CompactEnv
  { _ceLogger :: Logger SomeLogMessage
  , _ceDb :: Database
  , _ceFlags :: [CompactFlag]
  }
makeLenses ''CompactEnv

withDefaultLogger :: LL.LogLevel -> (Logger SomeLogMessage -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend_ logText defaultHandleBackendConfig $ \b ->
    withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel (l2l ll) l)

withPerChainFileLogger :: FilePath -> ChainId -> LL.LogLevel -> (Logger SomeLogMessage -> IO a) -> IO a
withPerChainFileLogger logDir chainId ll f = do
  createDirectoryIfMissing False {- don't create parents -} logDir
  let logFile = logDir </> ("chain-" <> cid <> ".log")
  !_ <- writeFile logFile ""
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
      when (not doneYet) $ do
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

newtype CompactM a = CompactM { unCompactM :: ReaderT CompactEnv IO a }
  deriving newtype (Functor,Applicative,Monad,MonadReader CompactEnv,MonadIO,MonadThrow,MonadCatch)

instance MonadLog Text CompactM where
  localScope :: (LogScope -> LogScope) -> CompactM x -> CompactM x
  localScope f = local (over (ceLogger . setLoggerScope) f)

  logg :: LogLevel -> Text -> CompactM ()
  logg ll m = do
    l <- view ceLogger
    liftIO $ loggerFunIO l ll $ toLogMessage $ TextLog m

  withLevel :: LogLevel -> CompactM x -> CompactM x
  withLevel l = local (set (ceLogger . setLoggerLevel) l)

  withPolicy :: LogPolicy -> CompactM x -> CompactM x
  withPolicy p = local (set (ceLogger . setLoggerPolicy) p)

-- | Run compaction monad
runCompactM :: CompactEnv -> CompactM a -> IO a
runCompactM e a = runReaderT (unCompactM a) e

-- | Prepare/Execute a "$VTABLE$"-templated query.
execM_ :: ()
  => Text -- ^ query name (for logging purposes)
  -> TableName -- ^ table name
  -> Text -- ^ "$VTABLE$"-templated query
  -> CompactM ()
execM_ msg tbl q = do
  db <- view ceDb
  q' <- templateStmt tbl q
  queryDebug msg (Just tbl) $ Pact.exec_ db q'

execNoTemplateM_ :: ()
  => Text -- ^ query name (for logging purposes)
  -> Utf8 -- ^ query
  -> CompactM ()
execNoTemplateM_ msg q = do
  db <- view ceDb
  queryDebug msg Nothing $ Pact.exec_ db q

-- | Prepare/Execute a "$VTABLE$"-templated, parameterised query.
--   The parameters are the results of the 'CompactM' 'SType' computations.
execM' :: ()
  => Text -- ^ query name (for logging purposes)
  -> TableName -- ^ table name
  -> Text -- ^ "$VTABLE$"-templated query
  -> [SType] -- ^ parameters
  -> CompactM ()
execM' msg tbl stmt ps = do
  db <- view ceDb
  stmt' <- templateStmt tbl stmt
  queryDebug msg (Just tbl) $ Pact.exec' db stmt' ps

exec_ :: ()
  => Text
  -> Utf8
  -> CompactM ()
exec_ msg q = do
  db <- view ceDb
  queryDebug msg Nothing $ Pact.exec_ db q

-- | Prepare/Execute a "$VTABLE$"-templated, parameterised query.
--   'RType's are the expected results.
qryM :: ()
  => Text -- ^ query name (for logging purposes)
  -> TableName -- ^ table name
  -> Text -- ^ "$VTABLE$"-templated query
  -> [SType] -- ^ parameters
  -> [RType] -- ^ result types
  -> CompactM [[SType]]
qryM msg tbl q ins outs = do
  db <- view ceDb
  q' <- templateStmt tbl q
  queryDebug msg (Just tbl) $ Pact.qry db q' ins outs

qryNoTemplateM :: ()
  => Text -- ^ query name (for logging purposes)
  -> Utf8 -- ^ query
  -> [SType] -- ^ parameters
  -> [RType] -- ^ results
  -> CompactM [[SType]]
qryNoTemplateM msg q ins outs = do
  db <- view ceDb
  queryDebug msg Nothing $ Pact.qry db q ins outs

queryDebug :: Text -> Maybe TableName -> IO x -> CompactM x
queryDebug qryName mTblName performQuery = do
  logg Info $ "Starting query " <> qryName
  (ts, r) <- liftIO $ Chronos.stopwatch performQuery
  logg Info $ "Completed query " <> qryName <> ". It took " <> Text.pack (show (Chronos.asSeconds ts)) <> "s"
  liftIO $ atomicModifyIORef' queryTimes $ \qdbg -> case mTblName of
    Nothing -> (addOnce qryName ts qdbg, ())
    Just tblName -> (addTblQuery tblName qryName ts qdbg, ())
  pure r

data QueryDebug = QueryDebug
  { runOnce :: Set (Chronos.Timespan, Text)
  , tableQueries :: Map TableName (Set (Chronos.Timespan, Text))
  }

addOnce :: Text -> Chronos.Timespan -> QueryDebug -> QueryDebug
addOnce dbg t qdbg = qdbg { runOnce = Set.insert (t, dbg) qdbg.runOnce }

addTblQuery :: TableName -> Text -> Chronos.Timespan -> QueryDebug -> QueryDebug
addTblQuery tbl dbg t qdbg = qdbg { tableQueries = M.insertWith Set.union tbl (Set.singleton (t, dbg)) qdbg.tableQueries }

emptyQueryDebug :: QueryDebug
emptyQueryDebug = QueryDebug Set.empty M.empty

queryTimes :: IORef QueryDebug
queryTimes = unsafePerformIO (newIORef emptyQueryDebug)
{-# noinline queryTimes #-}

-- | Statements are templated with "$VTABLE$" substituted
-- with the currently-focused versioned table.
templateStmt :: TableName -> Text -> CompactM Utf8
templateStmt (TableName (Utf8 tblName)) s =
  pure $ Utf8 $ Text.encodeUtf8 $
    Text.replace "$VTABLE$" ("[" <> Text.decodeUtf8 tblName <> "]") s

-- | Execute a SQLite transaction, rolling back on failure.
--   Throws a 'CompactExceptionDb' on failure.
withTx :: HasCallStack => CompactM a -> CompactM a
withTx a = do
  exec_ "withTx.0" "SAVEPOINT compact_tx"
  catch (a >>= \r -> exec_ "withTx.1" "RELEASE SAVEPOINT compact_tx" >> pure r) $
      \e@SomeException {} -> do
        exec_ "withTx.2" "ROLLBACK TRANSACTION TO SAVEPOINT compact_tx"
        throwM $ CompactExceptionDb e

unlessFlagSet :: CompactFlag -> CompactM () -> CompactM ()
unlessFlagSet f x = do
  yeahItIs <- isFlagSet f
  unless yeahItIs x

isFlagSet :: CompactFlag -> CompactM Bool
isFlagSet f = view ceFlags >>= \fs -> pure (f `elem` fs)

withTables :: Vector TableName -> (TableName -> CompactM a) -> CompactM ()
withTables ts a = do
  V.iforM_ ts $ \i u@(TableName (Utf8 t')) -> do
    let lbl = Text.decodeUtf8 t' <> " (" <> sshow (i + 1) <> " of " <> sshow (V.length ts) <> ")"
    localScope (("table",lbl):) $ a u

-- | Takes a bunch of singleton tablename rows, sorts them, returns them as
--   @TableName@
sortedTableNames :: [[SType]] -> [TableName]
sortedTableNames rows = M.elems $ M.fromListWith const $ flip List.map rows $ \case
  [SText n@(Utf8 s)] -> (Text.toLower (Text.decodeUtf8 s), TableName n)
  _ -> error "sortedTableNames: expected text"

-- | CompactActiveRow collects all active rows from all tables.
createCompactActiveRow :: CompactM ()
createCompactActiveRow = do
  execNoTemplateM_ "createTable: CompactActiveRow"
      " CREATE TABLE IF NOT EXISTS CompactActiveRow \
      \ ( tablename TEXT NOT NULL \
      \ , rowkey TEXT NOT NULL \
      \ , vrowid INTEGER NOT NULL \
      \ , UNIQUE (tablename,rowkey) ); "

  execNoTemplateM_ "deleteFrom: CompactActiveRow"
      "DELETE FROM CompactActiveRow"

locateTarget :: TargetBlockHeight -> CompactM BlockHeight
locateTarget = \case
  Target bh -> do
    ensureBlockHeightExists bh
    pure bh
  Latest -> do
    getLatestBlockHeight

ensureBlockHeightExists :: BlockHeight -> CompactM ()
ensureBlockHeightExists bh = do
  r <- qryNoTemplateM
    "ensureBlockHeightExists.0"
    "SELECT blockheight FROM BlockHistory WHERE blockheight = ?1"
    [bhToSType bh]
    [RInt]
  case r of
    [[SInt rBH]] -> do
      when (fromIntegral bh /= rBH) $ do
        throwM $ CompactExceptionInvalidBlockHeight bh
    _ -> do
      error "ensureBlockHeightExists.0: impossible"

getLatestBlockHeight :: CompactM BlockHeight
getLatestBlockHeight = do
  r <- qryNoTemplateM
    "getLatestBlockHeight.0"
    "SELECT blockheight FROM BlockHistory ORDER BY blockheight DESC LIMIT 1"
    []
    [RInt]
  case r of
    [[SInt bh]] -> do
      pure (fromIntegral bh)
    _ -> do
      throwM CompactExceptionNoLatestBlockHeight

getEndingTxId :: BlockHeight -> CompactM TxId
getEndingTxId bh = do
  r <- qryNoTemplateM
       "getTxId.0"
       "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
       [bhToSType bh]
       [RInt]
  case r of
    [] -> do
      throwM (CompactExceptionInvalidBlockHeight bh)
    [[SInt t]] -> do
      pure (TxId (fromIntegral t))
    _ -> do
      internalError "initialize: expected single-row int"

getVersionedTables :: BlockHeight -> CompactM (Vector TableName)
getVersionedTables bh = do
  logg Info "getVersionedTables"
  rs <- qryNoTemplateM
        "getVersionedTables.0"
        " SELECT DISTINCT tablename FROM VersionedTableMutation \
        \ WHERE blockheight <= ? ORDER BY blockheight; "
        [bhToSType bh]
        [RText]
  pure (V.fromList (sortedTableNames rs))

tableRowCount :: TableName -> Text -> CompactM ()
tableRowCount tbl label =
  qryM "tableRowCount.0" tbl "SELECT COUNT(*) FROM $VTABLE$" [] [RInt] >>= \case
    [[SInt r]] -> logg Info $ label <> ":rowcount=" <> sshow r
    _ -> internalError "count(*) failure"

-- | For a given table, collect all active rows into CompactActiveRow
collectTableRows :: TxId -> TableName -> CompactM ()
collectTableRows txId tbl = do
  tableRowCount tbl "collectTableRows"
  let vt = tableNameToSType tbl
  let txid = txIdToSType txId

  let collectInsert = Text.concat
        [ "INSERT INTO CompactActiveRow "
        , "SELECT ?1,rowkey,rowid "
        , "FROM $VTABLE$ t1 "
        , "WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 "
        , "WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) "
        , "GROUP BY rowkey; "
        ]

  execM' "collectTableRows.0" tbl
    collectInsert
    [vt, txid]

-- | Delete non-active rows from given table.
compactTable :: TableName -> CompactM ()
compactTable tbl = do
  logg Info $ "compactTable: " <> fromUtf8 (getTableName tbl)

  execM'
      "compactTable.0"
      tbl
      " DELETE FROM $VTABLE$ WHERE rowid NOT IN \
      \ (SELECT t.rowid FROM $VTABLE$ t \
      \  LEFT JOIN CompactActiveRow v \
      \  WHERE t.rowid = v.vrowid AND v.tablename=?1); "
      [tableNameToSType tbl]

-- | Drop any versioned tables created after target blockheight.
dropNewTables :: BlockHeight -> CompactM ()
dropNewTables bh = do
  logg Info "dropNewTables"
  nts <- V.fromList . sortedTableNames <$> qryNoTemplateM "dropNewTables.0"
      " SELECT tablename FROM VersionedTableCreation \
      \ WHERE createBlockheight > ?1 ORDER BY createBlockheight; "
      [bhToSType bh]
      [RText]

  withTables nts $ \tbl -> do
    execM_ "dropNewTables.1" tbl "DROP TABLE IF EXISTS $VTABLE$"

-- | Delete all rows from Checkpointer system tables that are not for the target blockheight.
--
--   We currently do not compact TransactionIndex. This will change once we are
--   properly pruning RocksDB.
compactSystemTables :: BlockHeight -> CompactM ()
compactSystemTables bh = do
  let systemTables = ["BlockHistory", "VersionedTableMutation"]
  forM_ systemTables $ \tbl -> do
    let tblText = fromUtf8 (getTableName tbl)
    logg Info $ "Compacting system table " <> tblText
    let column =
          if tbl == "VersionedTableCreation"
          then "createBlockheight"
          else "blockheight"
    execM'
      ("compactSystemTables: " <> tblText)
      tbl
      ("DELETE FROM $VTABLE$ WHERE " <> column <> " != ?1;")
      [bhToSType bh]
  -- we must treat VersionedTableCreation specially; read-only rewind
  -- needs to know if tables have been created yet via this table, so
  -- we don't delete from its past.
  execM'
      "compactSystemTables: VersionedTableCreation"
      "VersionedTableCreation"
      "DELETE FROM VersionedTableCreation WHERE createBlockheight > ?1;"
      [bhToSType bh]


dropCompactTables :: CompactM ()
dropCompactTables = do
  execNoTemplateM_ "dropCompactTables.0"
    "DROP TABLE CompactActiveRow"

compact :: ()
  => TargetBlockHeight
  -> Logger SomeLogMessage
  -> Database
  -> [CompactFlag]
  -> IO ()
compact tbh logger db flags = runCompactM (CompactEnv logger db flags) $ do
  logg Info "Beginning compaction"

  withTx createCompactActiveRow

  blockHeight <- locateTarget tbh
  txId <- getEndingTxId blockHeight

  logg Info $ "Target blockheight: " <> sshow blockHeight
  logg Info $ "Ending TxId: " <> sshow txId

  versionedTables <- getVersionedTables blockHeight

  withTables versionedTables $ \tbl -> collectTableRows txId tbl

  withTx $ do
    withTables versionedTables $ \tbl -> do
      compactTable tbl
    dropNewTables blockHeight
    compactSystemTables blockHeight

  unlessFlagSet KeepCompactTables $ do
    logg Info "Dropping compact-specific tables"
    withTx dropCompactTables

  unlessFlagSet NoVacuum $ do
    logg Info "Vacuum"
    execNoTemplateM_ "VACUUM" "VACUUM;"

  debugLogs <- liftIO $ readIORef queryTimes
  let -- every query that takes >= 1 second
      expensiveQueries = List.filter (not . null . snd)
        $ List.map
            ( over _2
                (List.take 10
                  . List.sortOn (Down . fst)
                  . List.filter ((>= Chronos.second) . fst)
                  . Set.toList
                )
            )
        $ M.toList debugLogs.tableQueries

  forM_ expensiveQueries $ \(tblName, mostWanted) -> do
    logg Debug $ "Most expensive queries on table " <> fromUtf8 (getTableName tblName)
    forM_ mostWanted $ \(ts, qryMsg) -> do
      logg Debug $ "Query " <> qryMsg <> " took " <> Text.pack (show (Chronos.asSeconds ts)) <> "s"

  logg Info "Compaction complete"

data TargetBlockHeight
  = Target !BlockHeight
    -- ^ compact to this blockheight across all chains
  | Latest
    -- ^ for each chain, compact to its latest blockheight
  deriving stock (Eq, Show)

data CompactConfig = CompactConfig
  { ccBlockHeight :: TargetBlockHeight
  , ccDbDir :: FilePath
  , ccVersion :: ChainwebVersion
  , ccFlags :: [CompactFlag]
  , ccChains :: Maybe (Set ChainId)
  , logDir :: FilePath
  , ccThreads :: Int
  }
  deriving stock (Eq, Show)

compactAll :: CompactConfig -> IO ()
compactAll CompactConfig{..} = do
  latestBlockHeightChain0 <- do
    let cid = unsafeChainId 0
    withDefaultLogger LL.Error $ \logger -> do
      let resetDb = False
      withSqliteDb cid logger ccDbDir resetDb $ \(SQLiteEnv db _) -> do
        runCompactM (CompactEnv logger db []) getLatestBlockHeight

  let allCids = Set.fromList $ F.toList $ chainIdsAt ccVersion latestBlockHeightChain0
  let targetCids = Set.toList $ maybe allCids (Set.intersection allCids) ccChains

  flip (pooledMapConcurrentlyN_ ccThreads) targetCids $ \cid -> do
    withPerChainFileLogger logDir cid LL.Debug $ \logger -> do
      let resetDb = False
      withSqliteDb cid logger ccDbDir resetDb $ \(SQLiteEnv db _) -> do
        void $ compact ccBlockHeight logger db ccFlags

main :: IO ()
main = do
  config <- execParser opts
  compactAll config
  where
    opts :: ParserInfo CompactConfig
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Pact DB Compaction tool")

    collapseSum :: [Parser [a]] -> Parser [a]
    collapseSum = foldr (\x y -> (++) <$> x <*> y) (pure [])

    maybeList :: [a] -> Maybe [a]
    maybeList = \case
      [] -> Nothing
      xs -> Just xs

    parser :: Parser CompactConfig
    parser = CompactConfig
        <$> (fmap Target (fromIntegral @Int <$> option auto
             (short 'b'
              <> long "target-blockheight"
              <> metavar "BLOCKHEIGHT"
              <> help "Target blockheight")) <|> pure Latest)
        <*> strOption
             (short 'd'
              <> long "pact-database-dir"
              <> metavar "DBDIR"
              <> help "Pact database directory")
        <*> (lookupVersionByName . fromTextSilly @ChainwebVersionName <$> strOption
              (short 'v'
               <> long "graph-version"
               <> metavar "VERSION"
               <> help "Chainweb version for graph. Only needed for non-standard graphs."
               <> value (toText (_versionName mainnet))
               <> showDefault))
        <*> collapseSum
               [ flag [] [KeepCompactTables]
                  (long "keep-compact-tables"
                   <> help "Keep compaction tables post-compaction, for inspection.")
               , flag [] [NoVacuum]
                  (long "no-vacuum"
                   <> help "Don't VACUUM database.")
               ]
        <*> fmap (fmap Set.fromList . maybeList) (many (unsafeChainId <$> option auto
             (short 'c'
              <> long "chain"
              <> metavar "CHAINID"
              <> help "Add this chain to the target set of ones to compact.")))
        <*> strOption
              (long "log-dir"
               <> metavar "DIRECTORY"
               <> help "Directory where logs will be placed"
               <> value ".")
        <*> option auto
             (short 't'
              <> long "threads"
              <> metavar "THREADS"
              <> value 4
              <> help "Number of threads for compaction processing")

fromTextSilly :: HasTextRepresentation a => Text -> a
fromTextSilly t = case fromText t of
  Just a -> a
  Nothing -> error "fromText failed"

bhToSType :: BlockHeight -> SType
bhToSType bh = SInt (int bh)

txIdToSType :: TxId -> SType
txIdToSType (TxId txid) = SInt (fromIntegral txid)

tableNameToSType :: TableName -> SType
tableNameToSType (TableName tbl) = SText tbl
