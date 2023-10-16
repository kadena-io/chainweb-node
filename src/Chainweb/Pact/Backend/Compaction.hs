{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , CompactM
  , compact
  , compactAll
  , compactMain
  , withDefaultLogger
  , withPerChainFileLogger
  , getLatestPactState, PactRow(..)
  ) where

import UnliftIO.Async (pooledMapConcurrentlyN_)
import Control.Exception (Exception, SomeException(..))
import Control.Lens (makeLenses, set, over, view)
import Control.Monad (forM, forM_, when, void)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local)
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as M
import Data.Map (Map)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Data.Vector (Vector)
import Database.SQLite3.Direct (Utf8(..), Database)
import GHC.Stack (HasCallStack)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Utils (sshow, HasTextRepresentation, fromText, toText, int)
import Chainweb.Version (ChainId, ChainwebVersion(..), ChainwebVersionName, unsafeChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Utils (encodeB64Text)

import System.Logger
import Data.LogMessage

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact

newtype ITxId = ITxId Int64

newtype TableName = TableName { getTableName :: Utf8 }
  deriving stock (Show)

data CompactException
  = CompactExceptionInternal !Text
  | CompactExceptionDb !SomeException
  | CompactExceptionInvalidBlockHeight !BlockHeight
  | CompactExceptionTableVerificationFailure !TableName
  deriving stock (Show)
  deriving anyclass (Exception)

data CompactFlag
  = Flag_KeepCompactTables
    -- ^ Keep compaction tables post-compaction for inspection.
  | Flag_NoVacuum
    -- ^ Don't VACUUM database
  | Flag_NoDropNewTables
    -- ^ Don't drop new tables created after the compaction height.
  | Flag_NoGrandHash
    -- ^ Don't compute the grand hash.
  deriving stock (Eq,Show,Read,Enum,Bounded)

internalError :: MonadThrow m => Text -> m a
internalError = throwM . CompactExceptionInternal

data CompactEnv = CompactEnv
  { _ceDb :: !Database
  , _ceLogger :: Logger SomeLogMessage
  , _ceFlags :: [CompactFlag]
  }
makeLenses ''CompactEnv

withDefaultLogger :: LogLevel -> (Logger SomeLogMessage -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend_ logText defaultHandleBackendConfig $ \b ->
    withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel ll l)

withPerChainFileLogger :: FilePath -> ChainId -> LogLevel -> (Logger SomeLogMessage -> IO a) -> IO a
withPerChainFileLogger logDir chainId ll f = do
  createDirectoryIfMissing False {- don't create parents -} logDir
  let logFile = logDir </> ("compact-chain-" <> cid <> ".log")
  !_ <- writeFile logFile ""
  let handleConfig = defaultHandleBackendConfig
        { _handleBackendConfigHandle = FileHandle logFile
        }
  withHandleBackend_ logText handleConfig $ \b ->
    withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel ll l)
  where
    cid = Text.unpack (chainIdToText chainId)

-- | Set up compaction.
mkCompactEnv
    :: Logger SomeLogMessage
    -- ^ Logger
    -> Database
    -- ^ A single-chain pact database connection.
    -> [CompactFlag]
    -- ^ Execution flags.
    -> CompactEnv
mkCompactEnv logger db flags = CompactEnv db logger flags

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
  withLevel l = local (set (ceLogger.setLoggerLevel) l)

  withPolicy :: LogPolicy -> CompactM x -> CompactM x
  withPolicy p = local (set (ceLogger.setLoggerPolicy) p)

-- | Run compaction monad, see 'mkCompactEnv'.
runCompactM :: CompactEnv -> CompactM a -> IO a
runCompactM e a = runReaderT (unCompactM a) e

-- | Prepare/Execute a "$VTABLE$"-templated query.
execM_ :: ()
  => Text -- ^ query name (for logging purposes)
  -> TableName -- ^ table name
  -> Text -- ^ "$VTABLE$"-templated query
  -> CompactM ()
execM_ msg tbl q = do
  logQueryDebug msg
  q' <- templateStmt tbl q
  withDb $ \db -> liftIO $ Pact.exec_ db q'

execNoTemplateM_ :: ()
  => Text -- ^ query name (for logging purposes)
  -> Utf8 -- ^ query
  -> CompactM ()
execNoTemplateM_ msg qry = do
  logQueryDebug msg
  withDb $ \db -> liftIO $ Pact.exec_ db qry

-- | Prepare/Execute a "$VTABLE$"-templated, parameterised query.
--   The parameters are the results of the 'CompactM' 'SType' computations.
execM' :: ()
  => Text -- ^ query name (for logging purposes)
  -> TableName -- ^ table name
  -> Text -- ^ "$VTABLE$"-templated query
  -> [SType] -- ^ parameters
  -> CompactM ()
execM' msg tbl stmt ps = do
  logQueryDebug msg
  stmt' <- templateStmt tbl stmt
  withDb $ \db -> liftIO $ Pact.exec' db stmt' ps

exec_ :: ()
  => Text
  -> Database
  -> Utf8
  -> CompactM ()
exec_ msg db qry = do
  logQueryDebug msg
  liftIO $ Pact.exec_ db qry

qry_ :: ()
  => Text
  -> Database
  -> Utf8
  -> [RType]
  -> CompactM [[SType]]
qry_ msg db qry rs = do
  logQueryDebug msg
  liftIO $ Pact.qry_ db qry rs

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
  logQueryDebug msg
  q' <- templateStmt tbl q
  withDb $ \db -> liftIO $ Pact.qry db q' ins outs

qryNoTemplateM :: ()
  => Text -- ^ query name (for logging purposes)
  -> Utf8 -- ^ query
  -> [SType] -- ^ parameters
  -> [RType] -- ^ results
  -> CompactM [[SType]]
qryNoTemplateM msg q ins outs = do
  logQueryDebug msg
  withDb $ \db -> liftIO $ Pact.qry db q ins outs

logQueryDebug :: Text -> CompactM ()
logQueryDebug msg = do
  logg Info ("Query: " <> msg)

-- | Statements are templated with "$VTABLE$" substituted
-- with the currently-focused versioned table.
templateStmt :: TableName -> Text -> CompactM Utf8
templateStmt (TableName (Utf8 tblName)) s
    | tblTemplate `Text.isInfixOf` s =
        pure $ Utf8 $ Text.encodeUtf8 $
          Text.replace tblTemplate ("[" <> Text.decodeUtf8 tblName <> "]") s
    | otherwise = pure $ Utf8 $ Text.encodeUtf8 s
  where
    tblTemplate = "$VTABLE$"

utf8ToText :: Utf8 -> Text
utf8ToText (Utf8 u) = Text.decodeUtf8 u

textToUtf8 :: Text -> Utf8
textToUtf8 txt = Utf8 $ Text.encodeUtf8 $ Text.toLower txt

textToTableName :: Text -> TableName
textToTableName txt = TableName $ textToUtf8 txt

-- | Execute a SQLite transaction, rolling back on failure.
--   Throws a 'CompactExceptionDb' on failure.
withTx :: HasCallStack => CompactM a -> CompactM a
withTx a = withDb $ \db -> do
  exec_ "withTx.0" db $ "SAVEPOINT compact_tx"
  catch (a >>= \r -> exec_ "withTx.1" db "RELEASE SAVEPOINT compact_tx" >> pure r) $
      \e@SomeException {} -> do
        exec_ "withTx.2" db "ROLLBACK TRANSACTION TO SAVEPOINT compact_tx"
        throwM $ CompactExceptionDb e

withDb :: (Database -> CompactM a) -> CompactM a
withDb a = view ceDb >>= a

whenFlagUnset :: CompactFlag -> CompactM () -> CompactM ()
whenFlagUnset f x = do
  yeahItIs <- isFlagSet f
  when (not yeahItIs) x

isFlagSet :: CompactFlag -> CompactM Bool
isFlagSet f = view ceFlags >>= \fs -> pure (f `elem` fs)

withTables :: Vector TableName -> (TableName -> CompactM a) -> CompactM ()
withTables ts a = do
  V.iforM_ ts $ \((+ 1) -> i) u@(TableName (Utf8 t')) -> do
    let lbl = Text.decodeUtf8 t' <> " (" <> sshow i <> " of " <> sshow (V.length ts) <> ")"
    localScope (("table",lbl):) $ a u

-- | Takes a bunch of singleton tablename rows, sorts them, returns them as
--   @TableName@
sortedTableNames :: [[SType]] -> [TableName]
sortedTableNames rows = M.elems $ M.fromListWith const $ flip List.map rows $ \case
  [SText n@(Utf8 s)] -> (Text.toLower (Text.decodeUtf8 s), TableName n)
  _ -> error "sortedTableNames: expected text"

-- | CompactGrandHash associates table name with grand hash of its versioned rows,
-- and NULL with grand hash of all table hashes.
createCompactGrandHash :: CompactM ()
createCompactGrandHash = do
  logg Info "createTables"
  execNoTemplateM_ "createTable: CompactGrandHash"
      " CREATE TABLE IF NOT EXISTS CompactGrandHash \
      \ ( tablename TEXT \
      \ , hash BLOB \
      \ , UNIQUE (tablename) ); "

  execNoTemplateM_ "deleteFrom: CompactGrandHash"
      "DELETE FROM CompactGrandHash"

-- | CompactActiveRow collects all active rows from all tables.
createCompactActiveRow :: CompactM ()
createCompactActiveRow = do
  execNoTemplateM_ "createTable: CompactActiveRow"
      " CREATE TABLE IF NOT EXISTS CompactActiveRow \
      \ ( tablename TEXT NOT NULL \
      \ , rowkey TEXT NOT NULL \
      \ , vrowid INTEGER NOT NULL \
      \ , hash BLOB \
      \ , UNIQUE (tablename,rowkey) ); "

  execNoTemplateM_ "deleteFrom: CompactActiveRow"
      "DELETE FROM CompactActiveRow"

getEndingTxId :: BlockHeight -> CompactM ITxId
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
      pure (ITxId t)
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

bhToSType :: BlockHeight -> SType
bhToSType bh = SInt (int bh)

txIdToSType :: ITxId -> SType
txIdToSType (ITxId txid) = SInt txid

tableNameToSType :: TableName -> SType
tableNameToSType (TableName tbl) = SText tbl

tableRowCount :: TableName -> Text -> CompactM ()
tableRowCount tbl label =
  qryM "tableRowCount.0" tbl "SELECT COUNT(*) FROM $VTABLE$" [] [RInt] >>= \case
    [[SInt r]] -> logg Info $ label <> ":rowcount=" <> sshow r
    _ -> internalError "count(*) failure"

-- | For a given table, collect all active rows into CompactActiveRow,
-- and compute+store table grand hash in CompactGrandHash.
collectTableRows :: ITxId -> TableName -> CompactM ()
collectTableRows txId tbl = do
  tableRowCount tbl "collectTableRows"
  let vt = tableNameToSType tbl
  let txid = txIdToSType txId

  doGrandHash <- not <$> isFlagSet Flag_NoGrandHash
  if | doGrandHash -> do
         logg Info "collectTableRows:insert"
         execM' "collectTableRows.0, doGrandHash=True" tbl
           " INSERT INTO CompactActiveRow \
           \ SELECT ?1,rowkey,rowid, \
           \ sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata) \
           \ FROM $VTABLE$ t1 \
           \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
           \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
           \ GROUP BY rowkey; "
           [vt, txid]

         logg Info "collectTableRows:checksum"
         execM' "collectTableRows.1, doGrandHash=True" tbl
             " INSERT INTO CompactGrandHash \
             \ VALUES (?1, \
             \  (SELECT sha3a_256(hash) FROM CompactActiveRow \
             \   WHERE tablename=?1 ORDER BY rowkey)); "
             [vt]
     | otherwise -> do
         logg Info "collectTableRows:insert"
         execM' "collectTableRows.0, doGrandHash=False" tbl
           " INSERT INTO CompactActiveRow \
           \ SELECT ?1,rowkey,rowid, \
           \ NULL \
           \ FROM $VTABLE$ t1 \
           \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
           \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
           \ GROUP BY rowkey; "
           [vt, txid]

-- | Compute global grand hash from all table grand hashes.
computeGlobalHash :: CompactM ByteString
computeGlobalHash = do
  logg Info "computeGlobalHash"
  execNoTemplateM_ "computeGlobalHash.0"
      " INSERT INTO CompactGrandHash \
      \ VALUES (NULL, \
      \  (SELECT sha3a_256(hash) FROM CompactGrandHash \
      \   WHERE tablename IS NOT NULL ORDER BY tablename)); "

  withDb $ \db ->
    qry_ "computeGlobalHash.1" db "SELECT hash FROM CompactGrandHash WHERE tablename IS NULL" [RBlob] >>= \case
      [[SBlob h]] -> pure h
      _ -> throwM $ CompactExceptionInternal "computeGlobalHash: bad result"

-- | Delete non-active rows from given table.
compactTable :: TableName -> CompactM ()
compactTable tbl = do
  logg Info "compactTable"
  execM'
      "compactTable.0"
      tbl
      " DELETE FROM $VTABLE$ WHERE rowid NOT IN \
      \ (SELECT t.rowid FROM $VTABLE$ t \
      \  LEFT JOIN CompactActiveRow v \
      \  WHERE t.rowid = v.vrowid AND v.tablename=?1); "
      [tableNameToSType tbl]

-- | For given table, re-compute table grand hash and compare
-- with stored grand hash in CompactGrandHash.
verifyTable :: TableName -> CompactM ByteString
verifyTable tbl = do
  logg Info "verifyTable"
  curr <- computeTableHash tbl
  rs <- qryNoTemplateM "verifyTable.0"
      "SELECT hash FROM CompactGrandHash WHERE tablename=?1"
      [tableNameToSType tbl]
      [RBlob]
  case rs of
    [[SBlob prev]]
        | prev == curr -> do
            tableRowCount tbl "verifyTable"
            pure curr
        | otherwise ->
            throwM (CompactExceptionTableVerificationFailure tbl)
    _ -> throwM $ CompactExceptionInternal "verifyTable: bad result"

-- | For given table, compute table grand hash for max txid.
computeTableHash :: TableName -> CompactM ByteString
computeTableHash tbl = do
  rs <- qryM "computeTableHash.0" tbl
        " SELECT sha3a_256(hash) FROM \
        \ (SELECT sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata) as hash \
        \  FROM $VTABLE$ t1 \
        \  WHERE txid=(select max(txid) FROM $VTABLE$ t2 \
        \   WHERE t2.rowkey=t1.rowkey) GROUP BY rowkey); "
      [tableNameToSType tbl]
      [RBlob]
  case rs of
    [[SBlob curr]] -> pure curr
    _ -> throwM $ CompactExceptionInternal "checksumTable: bad result"

-- | Drop any versioned tables created after target blockheight.
dropNewTables :: BlockHeight -> CompactM ()
dropNewTables bh = do
  logg Info "dropNewTables"
  nts <- fmap (V.fromList . sortedTableNames) $ qryNoTemplateM "dropNewTables.0"
      " SELECT tablename FROM VersionedTableCreation \
      \ WHERE createBlockheight > ?1 ORDER BY createBlockheight; "
      [bhToSType bh]
      [RText]

  withTables nts $ \tbl -> do
    execM_ "dropNewTables.1" tbl "DROP TABLE IF EXISTS $VTABLE$"

-- | Delete all rows from Checkpointer system tables that are not for the target blockheight.
compactSystemTables :: BlockHeight -> CompactM ()
compactSystemTables bh = do
  let systemTables = ["BlockHistory", "VersionedTableMutation", "TransactionIndex", "VersionedTableCreation"]
  forM_ systemTables $ \tbl -> do
    logg Info $ "Compacting system table " <> tbl
    let column =
          if tbl == "VersionedTableCreation"
          then "createBlockheight"
          else "blockheight"
    execM'
      ("compactSystemTables: " <> tbl) (textToTableName tbl) ("DELETE FROM $VTABLE$ WHERE " <> column <> " != ?1;")
      [bhToSType bh]

dropCompactTables :: CompactM ()
dropCompactTables = do
  execNoTemplateM_ "dropCompactTables.0"
    " DROP TABLE CompactGrandHash; \
    \ DROP TABLE CompactActiveRow; "

compact :: ()
  => BlockHeight
  -> Logger SomeLogMessage
  -> Database
  -> [CompactFlag]
  -> IO (Maybe ByteString)
compact blockHeight logger db flags = runCompactM (mkCompactEnv logger db flags) $ do
  logg Info $ "Beginning compaction"

  doGrandHash <- not <$> isFlagSet Flag_NoGrandHash

  withTx $ do
    createCompactGrandHash
    createCompactActiveRow

  txId <- getEndingTxId blockHeight

  versionedTables <- getVersionedTables blockHeight

  gh <- withTx $ do
    withTables versionedTables $ \tbl -> collectTableRows txId tbl
    if doGrandHash
    then Just <$> computeGlobalHash
    else pure Nothing

  withTx $ do
    withTables versionedTables $ \tbl -> do
      compactTable tbl
      whenFlagUnset Flag_NoGrandHash $ void $ verifyTable tbl
    whenFlagUnset Flag_NoDropNewTables $ do
      logg Info "Dropping new tables"
      dropNewTables blockHeight
    compactSystemTables blockHeight

  whenFlagUnset Flag_KeepCompactTables $ do
    logg Info "Dropping compact-specific tables"
    withTx $ dropCompactTables

  whenFlagUnset Flag_NoVacuum $ do
    logg Info "Vacuum"
    execNoTemplateM_ "VACUUM" "VACUUM;"

  case gh of
    Just h -> do
      logg Info $ "Compaction complete, hash=" <> encodeB64Text h
    Nothing -> do
      logg Info $ "Compaction complete"

  pure gh

data CompactConfig = CompactConfig
  { ccBlockHeight :: BlockHeight
  , ccDbDir :: FilePath
  , ccVersion :: ChainwebVersion
  , ccFlags :: [CompactFlag]
  , ccChain :: Maybe ChainId
  , logDir :: FilePath
  , ccThreads :: Int
  }
  deriving stock (Eq, Show)

compactAll :: CompactConfig -> IO ()
compactAll CompactConfig{..} = do
  flip (pooledMapConcurrentlyN_ ccThreads) cids $ \cid -> do
    withPerChainFileLogger logDir cid Debug $ \logger' -> do
      let logger = over setLoggerScope (("chain",sshow cid):) logger'
      let resetDb = False
      withSqliteDb cid logger ccDbDir resetDb $ \(SQLiteEnv db _) -> do
        case ccChain of
          Just ccid | ccid /= cid -> do
            pure ()
          _ -> do
            void $ compact ccBlockHeight logger db ccFlags
  where
    cids = List.sort $ F.toList $ chainIdsAt ccVersion ccBlockHeight

compactMain :: IO ()
compactMain = do
  config <- execParser opts
  compactAll config
  where
    opts :: ParserInfo CompactConfig
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Pact DB Compaction tool")

    parser :: Parser CompactConfig
    parser = CompactConfig
        <$> (fromIntegral @Int <$> option auto
             (short 'b'
              <> long "target-blockheight"
              <> metavar "BLOCKHEIGHT"
              <> help "Target blockheight"))
        <*> strOption
             (short 'd'
              <> long "pact-database-dir"
              <> metavar "DBDIR"
              <> help "Pact database directory")
        <*> ((lookupVersionByName . fromTextSilly @ChainwebVersionName) <$> strOption
              (short 'v'
               <> long "graph-version"
               <> metavar "VERSION"
               <> help "Chainweb version for graph. Only needed for non-standard graphs."
               <> value (toText (_versionName mainnet))
               <> showDefault))
        <*> (foldr (\x y -> (++) <$> x <*> y) (pure [])
               [ flag [] [Flag_KeepCompactTables]
                  (long "keep-compact-tables"
                   <> help "Keep compaction tables post-compaction, for inspection.")
               , flag [] [Flag_NoVacuum]
                  (long "no-vacuum"
                   <> help "Don't VACUUM database.")
               , flag [] [Flag_NoDropNewTables]
                  (long "no-drop-new-tables"
                   <> help "Don't drop new tables.")
               , flag [] [Flag_NoGrandHash]
                  (long "no-grand-hash"
                   <> help "Don't compute the compact grand hash.")
               ])
        <*> optional (unsafeChainId <$> option auto
             (short 'c'
              <> metavar "CHAINID"
              <> help "If supplied, compact only this chain"))
        <*> strOption
              (long "log-dir"
               <> metavar "DIRECTORY"
               <> help "Directory where logs will be placed"
               <> value ".")
        <*> (option auto
             (short 't'
              <> long "threads"
              <> metavar "THREADS"
              <> value 4
              <> help "Number of threads for compaction processing"))

    fromTextSilly :: HasTextRepresentation a => Text -> a
    fromTextSilly t = case fromText t of
      Just a -> a
      Nothing -> error "fromText failed"

getLatestPactState :: Database -> IO (Map Text [PactRow])
getLatestPactState db = do
  let checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]
  let compactionTables = ["CompactGrandHash", "CompactActiveRow"]
  let excludeThese = checkpointerTables ++ compactionTables
  let fmtTable x = "\"" <> x <> "\""

  tables <- fmap sortedTableNames $ do
    let qry =
          "SELECT name FROM sqlite_schema \
          \WHERE \
          \  type = 'table' \
          \AND \
          \  name NOT LIKE 'sqlite_%'"
    Pact.qry db qry [] [RText]

  let takeHead :: [a] -> a
      takeHead = \case
        [] -> error "getLatestPactState.getActiveRows.takeHead: impossible case"
        (x : _) -> x

  let getActiveRows :: [PactRow] -> [PactRow]
      getActiveRows rows = id
        $ List.map takeHead
        $ List.map (List.sortOn (Down . txId))
        $ List.groupBy (\x y -> rowKey x == rowKey y)
        $ List.sortOn rowKey rows

  let go :: Map Text [PactRow] -> TableName -> IO (Map Text [PactRow])
      go m (TableName tbl) = do
        if tbl `notElem` excludeThese
        then do
          let qry = "SELECT rowkey, rowdata, txid FROM " <> fmtTable tbl
          userRows <- Pact.qry db qry [] [RText, RBlob, RInt]
          shapedRows <- forM userRows $ \case
            [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
              pure $ PactRow {..}
            _ -> error "getLatestPactState: unexpected shape of user table row"
          pure $ M.insert (utf8ToText tbl) shapedRows m
        else do
          pure m

  allRows <- F.foldlM go mempty tables
  let activeRows = M.map getActiveRows allRows
  pure activeRows

data PactRow = PactRow
  { rowKey :: ByteString
  , rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Ord, Show)
