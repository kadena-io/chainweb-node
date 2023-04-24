{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
  ) where

import Control.Exception (Exception, SomeException(..))
import Control.Lens (makeLenses, set, over, view)
import Control.Monad (unless, forM, forM_, void)
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

import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Utils (sshow)
import Chainweb.Version (ChainId, ChainwebVersion(..), chainwebVersionFromText, unsafeChainId)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)

import System.Logger
import Data.LogMessage

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact

data CompactException
    = CompactExceptionInternal !Text
    | CompactExceptionDb !SomeException
    | CompactExceptionInvalidBlockHeight
    | CompactExceptionTableVerificationFailure !Utf8
  deriving Show
instance Exception CompactException

data CompactFlag
    = Flag_KeepCompactTables
    -- ^ Keep compaction tables post-compaction for inspection.
    | Flag_NoVacuum
    -- ^ Don't VACUUM database
    deriving (Eq,Show,Read,Enum,Bounded)

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

newtype CompactM a = CompactM {
  unCompactM :: ReaderT CompactEnv IO a
  }
  deriving (Functor,Applicative,Monad,MonadReader CompactEnv,MonadIO,MonadThrow,MonadCatch)

instance MonadLog Text CompactM where

  localScope f = local (over (ceLogger.setLoggerScope) f)

  logg ll m = do
    l <- view ceLogger
    liftIO $ loggerFunIO l ll $ toLogMessage $ TextLog m

  withLevel l = local (set (ceLogger.setLoggerLevel) l)

  withPolicy p = local (set (ceLogger.setLoggerPolicy) p)

-- | Run compaction monad, see 'mkCompactEnv'.
runCompactM :: CompactEnv -> CompactM a -> IO a
runCompactM e a = runReaderT (unCompactM a) e

execM_ :: Text -> CompactM ()
execM_ q = do
  q' <- templateStmt q
  withDb $ \db -> liftIO $ Pact.exec_ db q'

execM' :: Text -> [CompactM SType] -> CompactM ()
execM' stmt ps' = do
  ps <- sequence ps'
  stmt' <- templateStmt stmt
  withDb $ \db -> liftIO $ Pact.exec' db stmt' ps

qryM :: Text -> [CompactM SType] -> [RType] -> CompactM [[SType]]
qryM q ins' outs = do
  q' <- templateStmt q
  ins <- sequence ins'
  withDb $ \db -> liftIO $ Pact.qry db q' ins outs

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

withTx :: HasCallStack => CompactM a -> CompactM a
withTx a = withDb $ \db -> do
  liftIO $ Pact.exec_ db $ "SAVEPOINT compact_tx"
  catch (a >>= \r -> liftIO (Pact.exec_ db "RELEASE SAVEPOINT compact_tx") >> return r) $
      \e@SomeException {} -> do
        liftIO $ Pact.exec_ db "ROLLBACK TRANSACTION TO SAVEPOINT compact_tx"
        throwM $ CompactExceptionDb e

withDb :: (Database -> CompactM a) -> CompactM a
withDb a = view ceDb >>= a

unlessFlag :: CompactFlag -> CompactM () -> CompactM ()
unlessFlag f a = view ceFlags >>= \fs -> unless (f `elem` fs) a

withTables :: CompactM () -> CompactM ()
withTables a = view ceVersionTables >>= \ts ->
  forM_ (zip (F.toList ts) [1..]) $ \t@(Utf8 t',i) -> do
    let lbl = Text.decodeUtf8 t' <> " (" <> sshow i <> " of " <> sshow (V.length ts) <> ")"
    local (set ceVersionTable $ Just t) $
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
  execM_
      " CREATE TABLE IF NOT EXISTS CompactGrandHash \
      \ ( tablename TEXT \
      \ , hash BLOB \
      \ , UNIQUE (tablename) ); "

  execM_
      "DELETE FROM CompactGrandHash"


-- | CompactActiveRow collects all active rows from all tables.
createCompactActiveRow :: CompactM ()
createCompactActiveRow = do
  execM_
      " CREATE TABLE IF NOT EXISTS CompactActiveRow \
      \ ( tablename TEXT NOT NULL \
      \ , rowkey TEXT NOT NULL \
      \ , vrowid INTEGER NOT NULL \
      \ , hash BLOB \
      \ , UNIQUE (tablename,rowkey) ); "

  execM_
      "DELETE FROM CompactActiveRow"

-- | Sets environment txid, which is the "endingtxid" of the target blockheight.
readTxId :: CompactM a -> CompactM a
readTxId next = do

  r <- qryM
       "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
       [blockheight]
       [RInt]

  case r of
    [] -> throwM CompactExceptionInvalidBlockHeight
    [[SInt t]] ->
        local (set ceTxId (Just t)) next
    _ -> internalError "initialize: expected single-row int"

-- | Sets environment versioned tables, as all active tables created at
-- or before the target blockheight.
collectVersionedTables :: CompactM a -> CompactM a
collectVersionedTables next = do
  logg Info "collectVersionedTables"
  rs <- qryM
        " SELECT DISTINCT tablename FROM VersionedTableMutation \
        \ WHERE blockheight <= ? ORDER BY blockheight; "
        [blockheight]
        [RText]
  setTables rs next

tableRowCount :: Text -> CompactM ()
tableRowCount lbl =
  qryM "SELECT COUNT(*) FROM $VTABLE$" [] [RInt] >>= \case
    [[SInt r]] -> logg Info $ lbl <> ":rowcount=" <> sshow r
    _ -> internalError "count(*) failure"

-- | For a given table, collect all active rows into CompactActiveRow,
-- and compute+store table grand hash in CompactGrandHash.
collectTableRows :: CompactM ()
collectTableRows = do

  tableRowCount "collectTableRows"

  logg Info "collectTableRows:insert"
  execM'
      " INSERT INTO CompactActiveRow \
      \ SELECT ?1,rowkey,rowid, \
      \ sha3_256('T',?1,'K',rowkey,'I',txid,'D',rowdata) \
      \ FROM $VTABLE$ t1 \
      \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
      \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
      \ GROUP BY rowkey; "
      [vtable,txid]

  logg Info "collectTableRows:checksum"
  execM'
      " INSERT INTO CompactGrandHash \
      \ VALUES (?1, \
      \  (SELECT sha3a_256(hash) FROM CompactActiveRow \
      \   WHERE tablename=?1 ORDER BY rowkey)); "
      [vtable]

-- | Compute global grand hash from all table grand hashes.
computeGlobalHash :: CompactM ByteString
computeGlobalHash = do
  logg Info "computeGlobalHash"
  execM_
      " INSERT INTO CompactGrandHash \
      \ VALUES (NULL, \
      \  (SELECT sha3a_256(hash) FROM CompactGrandHash \
      \   WHERE tablename IS NOT NULL ORDER BY tablename)); "

  withDb $ \db ->
    liftIO $ Pact.qry_ db "SELECT hash FROM CompactGrandHash WHERE tablename IS NULL" [RBlob] >>= \case
      [[SBlob h]] -> return h
      _ -> throwM $ CompactExceptionInternal "computeGlobalHash: bad result"

-- | Delete non-active rows from given table.
compactTable :: CompactM ()
compactTable = do
  logg Info "compactTable"
  execM'
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
  rs <- qryM " SELECT hash FROM CompactGrandHash WHERE tablename=?1 "
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
  rs <- qryM
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
  nts <- qryM
      " SELECT tablename FROM VersionedTableCreation \
      \ WHERE createBlockheight > ?1 ORDER BY createBlockheight; "
      [blockheight]
      [RText]

  setTables nts $ withTables $ do
    execM_ "DROP TABLE IF EXISTS $VTABLE$"

-- | Delete all rows from Checkpointer system tables that are not for the target blockheight.
compactSystemTables :: CompactM ()
compactSystemTables = do
  execM'
      " DELETE FROM BlockHistory WHERE blockheight != ?1; \
      \ DELETE FROM VersionedTableMutation WHERE blockheight != ?1; \
      \ DELETE FROM TransactionIndex WHERE blockheight != ?1; \
      \ DELETE FROM VersionedTableCreation WHERE createBlockheight != ?1; "
      [blockheight]

dropCompactTables :: CompactM ()
dropCompactTables = do
  execM_
      " DROP TABLE CompactGrandHash; \
      \ DROP TABLE CompactActiveRow; "


compact :: CompactM ByteString
compact = do

    withTx $ do
      createCompactGrandHash
      createCompactActiveRow

    readTxId $ collectVersionedTables $ do

        gh <- withTx $ do
          withTables collectTableRows
          computeGlobalHash

        withTx $ do
          withTables $ do
            compactTable
            void $ verifyTable
          dropNewTables
          compactSystemTables

        unlessFlag Flag_KeepCompactTables $ withTx $ dropCompactTables

        unlessFlag Flag_NoVacuum $ do
          logg Info "Vacuum"
          execM_ "VACUUM;"

        return gh


data CompactConfig v = CompactConfig
  { ccBlockHeight :: BlockHeight
  , ccDbDir :: FilePath
  , ccVersion :: v
  , ccFlags :: [CompactFlag]
  , ccChain :: Maybe ChainId
  } deriving (Eq,Show,Functor,Foldable,Traversable)

compactAll :: CompactConfig ChainwebVersion -> IO ()
compactAll CompactConfig{..} = withDefaultLogger Debug $ \logger' ->
  forM_ cids $ \cid -> do
    let logger = over setLoggerScope (("chain",sshow cid):) logger'
    withSqliteDb cid logger ccDbDir False $ \(SQLiteEnv db _) ->
      runCompactM (mkCompactEnv logger db ccBlockHeight ccFlags) $
        case ccChain of
          Just ccid | ccid /= cid -> logg Info $ "Skipping chain"
          _ -> do
            logg Info $ "Beginning compaction"
            h <- compact
            logg Info $ "Compaction complete, hash=" <> sshow h

  where
    cids = List.sort $ F.toList $ chainIdsAt ccVersion ccBlockHeight

compactMain :: IO ()
compactMain = do
  execParser opts >>= \cc -> do
    traverse chainwebVersionFromText cc >>= compactAll
  where
    opts :: ParserInfo (CompactConfig Text)
    opts = info (parser <**> helper)
        (fullDesc <> progDesc "Pact DB Compaction tool")

    parser :: Parser (CompactConfig Text)
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
        <*> (Text.pack <$> strOption
              (short 'v'
               <> long "graph-version"
               <> metavar "VERSION"
               <> help "Chainweb version for graph. Only needed for non-standard graphs."
               <> value (show Mainnet01)
               <> showDefault))
        <*> ((<>)
               <$> flag [] [Flag_KeepCompactTables]
                     (long "keep-compact-tables"
                      <> help "Keep compaction tables post-compaction, for inspection.")
               <*> flag [] [Flag_NoVacuum]
                     (long "no-vacuum"
                      <> help "Don't VACUUM database."))
        <*> optional (unsafeChainId <$> option auto
             (short 'c'
              <> metavar "CHAINID"
              <> help "If supplied, compact only this chain"))
