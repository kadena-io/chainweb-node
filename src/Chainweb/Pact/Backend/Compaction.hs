{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ( mkCompactEnv
  , runCompactM
  , CompactFlag(..)
  , CompactM
  , compact
  , compactAll
  , compactMain
  , withDefaultLogger
  , withPerChainFileLogger
  ) where

import UnliftIO.Async (pooledMapConcurrentlyN_)
import Control.Exception (Exception, SomeException(..))
import Control.Lens (makeLenses, set, over, view)
import Control.Monad (forM_, forM, when, void)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local)
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Database.SQLite3.Direct (Utf8(..), Database)
import GHC.Stack (HasCallStack)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Utils (sshow, HasTextRepresentation, fromText, toText)
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

data CompactException
  = CompactExceptionInternal !Text
  | CompactExceptionDb !SomeException
  | CompactExceptionInvalidBlockHeight !BlockHeight
  | CompactExceptionTableVerificationFailure !Utf8
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
  , _ceBlockHeight :: !BlockHeight
  , _ceTxId :: !(Maybe Int64)
  , _ceVersionTables :: !(V.Vector Utf8)
  , _ceVersionTable :: !(Maybe (Utf8,Int))
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
    -> BlockHeight
    -- ^ Compaction blockheight.
    -> [CompactFlag]
    -- ^ Execution flags.
    -> CompactEnv
mkCompactEnv l d b = CompactEnv d b Nothing mempty Nothing l

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
  -> Text -- ^ "$VTABLE$"-templated query
  -> CompactM ()
execM_ msg q = do
  logQueryDebug msg
  q' <- templateStmt q
  withDb $ \db -> liftIO $ Pact.exec_ db q'

-- | Prepare/Execute a "$VTABLE$"-templated, parameterised query.
--   The parameters are the results of the 'CompactM' 'SType' computations.
execM' :: ()
  => Text -- ^ query name (for logging purposes)
  -> Text -- ^ "$VTABLE$"-templated query
  -> [CompactM SType] -- ^ parameters
  -> CompactM ()
execM' msg stmt ps' = do
  logQueryDebug msg
  ps <- sequence ps'
  stmt' <- templateStmt stmt
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
  -> Text -- ^ "$VTABLE$"-templated query
  -> [CompactM SType] -- ^ parameters
  -> [RType] -- ^ results
  -> CompactM [[SType]]
qryM msg q ins' outs = do
  logQueryDebug msg
  q' <- templateStmt q
  ins <- sequence ins'
  withDb $ \db -> liftIO $ Pact.qry db q' ins outs

logQueryDebug :: Text -> CompactM ()
logQueryDebug msg = do
  logg Info ("Query: " <> msg)

-- | Statements are templated with "$VTABLE$" substituted
-- with the currently-focused versioned table.
templateStmt :: Text -> CompactM Utf8
templateStmt s
    | tblTemplate `Text.isInfixOf` s =
        vtable' >>= \(Utf8 v) ->
          return $ Utf8 $ Text.encodeUtf8 $
            Text.replace tblTemplate ("[" <> Text.decodeUtf8 v <> "]") s
    | otherwise = pure $ Utf8 $ Text.encodeUtf8 s
  where
    tblTemplate = "$VTABLE$"

blockheight :: CompactM SType
blockheight = SInt . fromIntegral <$> view ceBlockHeight

txid :: CompactM SType
txid = view ceTxId >>= \case
  Just t -> pure $ SInt t
  Nothing -> internalError "txid not initialized!"

vtable :: CompactM SType
vtable = SText <$> vtable'

vtable' :: CompactM Utf8
vtable' = view ceVersionTable >>= \case
  Just t -> pure $ fst t
  Nothing -> internalError "version table not initialized!"

-- | Execute a SQLite transaction, rolling back on failure.
--   Throws a 'CompactExceptionDb' on failure.
withTx :: HasCallStack => CompactM a -> CompactM a
withTx a = withDb $ \db -> do
  exec_ "withTx.0" db $ "SAVEPOINT compact_tx"
  catch (a >>= \r -> exec_ "withTx.1" db "RELEASE SAVEPOINT compact_tx" >> return r) $
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

withTables :: CompactM () -> CompactM ()
withTables a = view ceVersionTables >>= \ts -> do
  V.iforM_ ts $ \((+ 1) -> i) u@(Utf8 t') -> do
    let lbl = Text.decodeUtf8 t' <> " (" <> sshow i <> " of " <> sshow (V.length ts) <> ")"
    local (set ceVersionTable $ Just (u, i)) $
      localScope (("table",lbl):) $ a

setTables :: [[SType]] -> CompactM a -> CompactM a
setTables rs next = do
  ts <- fmap (M.elems . M.fromListWith const) $
        forM rs $ \r -> case r of
          [SText n@(Utf8 s)] -> return (Text.toLower (Text.decodeUtf8 s), n)
          _ -> internalError "setTables: expected text"
  local (set ceVersionTables $ V.fromList ts) next

-- | CompactGrandHash associates table name with grand hash of its versioned rows,
-- and NULL with grand hash of all table hashes.
createCompactGrandHash :: CompactM ()
createCompactGrandHash = do
  logg Info "createTables"
  execM_ "createTable: CompactGrandHash"
      " CREATE TABLE IF NOT EXISTS CompactGrandHash \
      \ ( tablename TEXT \
      \ , hash BLOB \
      \ , UNIQUE (tablename) ); "

  execM_ "deleteFrom: CompactGrandHash"
      "DELETE FROM CompactGrandHash"

-- | CompactActiveRow collects all active rows from all tables.
createCompactActiveRow :: CompactM ()
createCompactActiveRow = do
  execM_ "createTable: CompactActiveRow"
      " CREATE TABLE IF NOT EXISTS CompactActiveRow \
      \ ( tablename TEXT NOT NULL \
      \ , rowkey TEXT NOT NULL \
      \ , vrowid INTEGER NOT NULL \
      \ , hash BLOB \
      \ , UNIQUE (tablename,rowkey) ); "

  execM_ "deleteFrom: CompactActiveRow"
      "DELETE FROM CompactActiveRow"

-- | Sets environment txid, which is the "endingtxid" of the target blockheight.
readTxId :: CompactM a -> CompactM a
readTxId next = do
  r <- qryM
       "readTxId.0"
       "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
       [blockheight]
       [RInt]

  case r of
    [] -> throwM =<< (CompactExceptionInvalidBlockHeight <$> view ceBlockHeight)
    [[SInt t]] ->
        local (set ceTxId (Just t)) next
    _ -> internalError "initialize: expected single-row int"

-- | Sets environment versioned tables, as all active tables created at
-- or before the target blockheight.
collectVersionedTables :: CompactM a -> CompactM a
collectVersionedTables next = do
  logg Info "collectVersionedTables"
  rs <- qryM
        "collectVersionedTables.0"
        " SELECT DISTINCT tablename FROM VersionedTableMutation \
        \ WHERE blockheight <= ? ORDER BY blockheight; "
        [blockheight]
        [RText]
  setTables rs next

tableRowCount :: Text -> CompactM ()
tableRowCount lbl =
  qryM "tableRowCount.0" "SELECT COUNT(*) FROM $VTABLE$" [] [RInt] >>= \case
    [[SInt r]] -> logg Info $ lbl <> ":rowcount=" <> sshow r
    _ -> internalError "count(*) failure"

-- | For a given table, collect all active rows into CompactActiveRow,
-- and compute+store table grand hash in CompactGrandHash.
collectTableRows :: CompactM ()
collectTableRows = do
  tableRowCount "collectTableRows"

  doGrandHash <- not <$> isFlagSet Flag_NoGrandHash
  if | doGrandHash -> do
         logg Info "collectTableRows:insert"
         execM' "collectTableRows.0, doGrandHash=True"
           " INSERT INTO CompactActiveRow \
           \ SELECT ?1,rowkey,rowid, \
           \ sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata) \
           \ FROM $VTABLE$ t1 \
           \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
           \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
           \ GROUP BY rowkey; "
           [vtable,txid]

         logg Info "collectTableRows:checksum"
         execM' "collectTableRows.1, doGrandHash=True"
             " INSERT INTO CompactGrandHash \
             \ VALUES (?1, \
             \  (SELECT sha3a_256(hash) FROM CompactActiveRow \
             \   WHERE tablename=?1 ORDER BY rowkey)); "
             [vtable]
     | otherwise -> do
         logg Info "collectTableRows:insert"
         execM' "collectTableRows.0, doGrandHash=False"
           " INSERT INTO CompactActiveRow \
           \ SELECT ?1,rowkey,rowid, \
           \ NULL \
           \ FROM $VTABLE$ t1 \
           \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
           \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
           \ GROUP BY rowkey; "
           [vtable,txid]

-- | Compute global grand hash from all table grand hashes.
computeGlobalHash :: CompactM ByteString
computeGlobalHash = do
  logg Info "computeGlobalHash"
  execM_ "computeGlobalHash.0"
      " INSERT INTO CompactGrandHash \
      \ VALUES (NULL, \
      \  (SELECT sha3a_256(hash) FROM CompactGrandHash \
      \   WHERE tablename IS NOT NULL ORDER BY tablename)); "

  withDb $ \db ->
    qry_ "computeGlobalHash.1" db "SELECT hash FROM CompactGrandHash WHERE tablename IS NULL" [RBlob] >>= \case
      [[SBlob h]] -> return h
      _ -> throwM $ CompactExceptionInternal "computeGlobalHash: bad result"

-- | Delete non-active rows from given table.
compactTable :: CompactM ()
compactTable = do
  logg Info "compactTable"
  execM' "compactTable.0"
      " DELETE FROM $VTABLE$ WHERE rowid NOT IN \
      \ (SELECT t.rowid FROM $VTABLE$ t \
      \  LEFT JOIN CompactActiveRow v \
      \  WHERE t.rowid = v.vrowid AND v.tablename=?1); "
      [vtable]

-- | For given table, re-compute table grand hash and compare
-- with stored grand hash in CompactGrandHash.
verifyTable :: CompactM ByteString
verifyTable = do
  logg Info "verifyTable"
  curr <- computeTableHash
  rs <- qryM "verifyTable.0"
      " SELECT hash FROM CompactGrandHash WHERE tablename=?1 "
      [vtable]
      [RBlob]
  case rs of
    [[SBlob prev]]
        | prev == curr -> do
            tableRowCount "verifyTable"
            return curr
        | otherwise ->
            vtable' >>= throwM . CompactExceptionTableVerificationFailure
    _ -> throwM $ CompactExceptionInternal "verifyTable: bad result"

-- | For given table, compute table grand hash for max txid.
computeTableHash :: CompactM ByteString
computeTableHash = do
  rs <- qryM "computeTableHash.0"
        " SELECT sha3a_256(hash) FROM \
        \ (SELECT sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata) as hash \
        \  FROM $VTABLE$ t1 \
        \  WHERE txid=(select max(txid) FROM $VTABLE$ t2 \
        \   WHERE t2.rowkey=t1.rowkey) GROUP BY rowkey); "
      [vtable]
      [RBlob]
  case rs of
    [[SBlob curr]] -> return curr
    _ -> throwM $ CompactExceptionInternal "checksumTable: bad result"

-- | Drop any versioned tables created after target blockheight.
dropNewTables :: CompactM ()
dropNewTables = do
  logg Info "dropNewTables"
  nts <- qryM "dropNewTables.0"
      " SELECT tablename FROM VersionedTableCreation \
      \ WHERE createBlockheight > ?1 ORDER BY createBlockheight; "
      [blockheight]
      [RText]

  setTables nts $ withTables $ do
    execM_ "dropNewTables.1" "DROP TABLE IF EXISTS $VTABLE$"

-- | Delete all rows from Checkpointer system tables that are not for the target blockheight.
compactSystemTables :: CompactM ()
compactSystemTables = do
  let systemTables = ["BlockHistory", "VersionedTableMutation", "TransactionIndex", "VersionedTableCreation"]
  forM_ systemTables $ \tbl -> do
    logg Info $ "Compacting system table " <> tbl
    let column =
          if tbl == "VersionedTableCreation"
          then "createBlockheight"
          else "blockheight"
    execM' ("compactSystemTables: " <> tbl) ("DELETE FROM " <> tbl <> " WHERE " <> column <> " != ?1;") [blockheight]

dropCompactTables :: CompactM ()
dropCompactTables = do
  execM_ "dropCompactTables.0"
      " DROP TABLE CompactGrandHash; \
      \ DROP TABLE CompactActiveRow; "

compact :: CompactM (Maybe ByteString)
compact = do
  doGrandHash <- not <$> isFlagSet Flag_NoGrandHash

  withTx $ do
    createCompactGrandHash
    createCompactActiveRow

  readTxId $ collectVersionedTables $ do

    gh <- withTx $ do
      withTables collectTableRows
      if doGrandHash
      then Just <$> computeGlobalHash
      else pure Nothing

    withTx $ do
      withTables $ do
        compactTable
        whenFlagUnset Flag_NoGrandHash $ void $ verifyTable
      whenFlagUnset Flag_NoDropNewTables $ do
        logg Info "Dropping new tables"
        dropNewTables
      compactSystemTables

    whenFlagUnset Flag_KeepCompactTables $ do
      logg Info "Dropping compact-specific tables"
      withTx $ dropCompactTables

    whenFlagUnset Flag_NoVacuum $ do
      logg Info "Vacuum"
      execM_ "VACUUM" "VACUUM;"

    return gh

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
      withSqliteDb cid logger ccDbDir resetDb $ \(SQLiteEnv db _) ->
        runCompactM (mkCompactEnv logger db ccBlockHeight ccFlags) $
          case ccChain of
            Just ccid | ccid /= cid -> logg Info $ "Skipping chain"
            _ -> do
              logg Info $ "Beginning compaction"
              m <- compact
              case m of
                Just h -> logg Info $ "Compaction complete, hash=" <> encodeB64Text h
                Nothing -> logg Info $ "Compaction complete"

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
