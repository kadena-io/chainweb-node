{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Pact.Backend.Compaction
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: see LICENSE.md
-- Maintainer: stuart@kadena.io
-- Stability: experimental
--

module Chainweb.Pact.Backend.Compaction
  ( mkCompactEnv
  , runCompactM
  , CompactFlag(..)
  , CompactM
  , compact
  , readGrandHash
  , withDefaultLogger
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text,replace,isInfixOf)
import Data.Text.Encoding

import Database.SQLite3.Direct

import Prelude hiding (log)

import Chainweb.BlockHeight
import Chainweb.Utils (sshow)

import System.Logger

import Pact.Types.SQLite

data CompactException
    = CompactExceptionInternal Text
    | CompactExceptionDb SomeException
    | CompactExceptionInvalidBlockHeight
    | CompactExceptionTableVerificationFailure Utf8
  deriving Show
instance Exception CompactException

data CompactFlag
    = Flag_KeepCompactTables
    | Flag_MigrateVersionTables
    deriving (Eq,Show)

internalError :: MonadThrow m => Text -> m a
internalError = throwM . CompactExceptionInternal

data CompactEnv = CompactEnv
  { _ceDb :: Database
  , _ceBlockHeight :: BlockHeight
  , _ceTxId :: Maybe Int64
  , _ceVersionTables :: [Utf8]
  , _ceVersionTable :: Maybe Utf8
  , _ceLogger :: Logger Text
  , _ceFlags :: [CompactFlag]
  }
makeLenses ''CompactEnv

withDefaultLogger :: LogLevel -> (Logger Text -> IO a) -> IO a
withDefaultLogger ll f = withHandleBackend defaultHandleBackendConfig $ \b ->
    withLogger defaultLoggerConfig b $ \l -> f (set setLoggerLevel ll l)

-- | Set up compaction.
mkCompactEnv
    :: Logger Text
    -- ^ Logger
    -> Database
    -- ^ A single-chain pact database connection.
    -> BlockHeight
    -- ^ Compaction blockheight.
    -> [CompactFlag]
    -- ^ Execution flags.
    -> CompactEnv
mkCompactEnv l d b = CompactEnv d b Nothing [] Nothing l

newtype CompactM a = CompactM {
  unCompactM :: ReaderT CompactEnv IO a
  }
  deriving (Functor,Applicative,Monad,MonadReader CompactEnv,MonadIO,MonadThrow,MonadCatch)

instance MonadLog Text CompactM where

  localScope f = local (over (ceLogger.setLoggerScope) f)

  logg ll m = do
    l <- view ceLogger
    liftIO $ loggerFunIO l ll m

  withLevel l = local (set (ceLogger.setLoggerLevel) l)

  withPolicy p = local (set (ceLogger.setLoggerPolicy) p)

-- | Run compaction monad, see 'mkCompactEnv'.
runCompactM :: CompactEnv -> CompactM a -> IO a
runCompactM e a = runReaderT (unCompactM a) e


execM_ :: Text -> CompactM ()
execM_ q = do
  q' <- templateStmt q
  withDb $ \db -> liftIO $ exec_ db q'

execM' :: Text -> [CompactM SType] -> CompactM ()
execM' stmt ps' = do
  ps <- sequence ps'
  stmt' <- templateStmt stmt
  withDb $ \db -> liftIO $ exec' db stmt' ps

qryM :: Text -> [CompactM SType] -> [RType] -> CompactM [[SType]]
qryM q ins' outs = do
  q' <- templateStmt q
  ins <- sequence ins'
  withDb $ \db -> liftIO $ qry db q' ins outs

-- | Statements are templated with "$VTABLE$" substituted
-- with the currently-focused versioned table.
templateStmt :: Text -> CompactM Utf8
templateStmt s
    | tblTemplate `isInfixOf` s =
        vtable' >>= \(Utf8 v) ->
          return $ Utf8 $ encodeUtf8 $
            replace tblTemplate ("[" <> decodeUtf8 v <> "]") s
    | otherwise = pure $ Utf8 $ encodeUtf8 s
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
  Just t -> pure t
  Nothing -> internalError "version table not initialized!"

withTx :: CompactM a -> CompactM a
withTx a = withDb $ \db -> do
  liftIO $ exec_ db $ "BEGIN TRANSACTION"
  catch (a >>= \r -> liftIO (exec_ db "COMMIT TRANSACTION") >> return r) $
      \e@SomeException {} -> do
        liftIO $ exec_ db "ROLLBACK TRANSACTION"
        throwM $ CompactExceptionDb e

withDb :: (Database -> CompactM a) -> CompactM a
withDb a = view ceDb >>= a

unlessFlag :: CompactFlag -> CompactM () -> CompactM ()
unlessFlag f a = view ceFlags >>= \fs -> unless (f `elem` fs) a

whenFlag :: CompactFlag -> CompactM () -> CompactM ()
whenFlag f a = view ceFlags >>= \fs -> when (f `elem` fs) a

withTables :: CompactM () -> CompactM ()
withTables a = view ceVersionTables >>= \ts ->
  forM_ ts $ \t@(Utf8 t') ->
    local (set ceVersionTable $ Just t) $
      localScope (("table",decodeUtf8 t'):) $ a

setTables :: [[SType]] -> CompactM () -> CompactM ()
setTables rs next = do
  ts <- forM rs $ \r -> case r of
    [SText n] -> return n
    _ -> internalError "setTables: expected text"
  local (set ceVersionTables ts) next

-- | CompactTableChecksum associates table name with grand hash of its versioned rows,
-- and NULL with grand hash of all table hashes.
createCompactTableChecksum :: CompactM ()
createCompactTableChecksum = do
  logg Info "createTables"
  execM_
      " CREATE TABLE IF NOT EXISTS CompactTableChecksum \
      \ ( tablename TEXT \
      \ , hash BLOB \
      \ , UNIQUE (tablename) ); "

  execM_
      "DELETE FROM CompactTableChecksum"


-- | CompactActiveVersion collects all active rows from all tables.
createCompactActiveVersion :: CompactM ()
createCompactActiveVersion = do
  execM_
      " CREATE TABLE IF NOT EXISTS CompactActiveVersion \
      \ ( tablename TEXT NOT NULL \
      \ , rowkey TEXT NOT NULL \
      \ , vrowid INTEGER NOT NULL \
      \ , hash BLOB \
      \ , UNIQUE (tablename,rowkey) ); "

  execM_
      "DELETE FROM CompactActiveVersion"

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
collectVersionedTables :: CompactM () -> CompactM ()
collectVersionedTables next = do
  logg Info "collectVersionedTables"
  rs <- qryM
        " SELECT DISTINCT tablename FROM VersionedTableMutation \
        \ WHERE blockheight <= ? ORDER BY tablename"
        [blockheight]
        [RText]
  setTables rs next

-- | For a given table, collect all active rows into CompactActiveVersion,
-- and compute+store table grand hash in CompactTableChecksum.
computeTableHash :: CompactM ()
computeTableHash = do
  logg Info "computeTableHash:insert"
  execM'
      " INSERT INTO CompactActiveVersion \
      \ SELECT ?1,rowkey,rowid,hash FROM $VTABLE$ t1 \
      \ WHERE txid=(SELECT MAX(txid) FROM $VTABLE$ t2 \
      \  WHERE t2.rowkey=t1.rowkey AND t2.txid<?2) \
      \ GROUP BY rowkey "
      [vtable,txid]

  logg Info "computeTableHash:checksum"
  execM'
      " INSERT INTO CompactTableChecksum \
      \ VALUES (?1, \
      \  (SELECT sha3a_256(hash) FROM CompactActiveVersion \
      \   WHERE tablename=?1 ORDER BY rowkey)) "
      [vtable]

-- | Compute global grand hash from all table grand hashes.
computeGlobalHash :: CompactM ()
computeGlobalHash = do
  logg Info "computeGlobalHash"
  execM_
      " INSERT INTO CompactTableChecksum \
      \ VALUES (NULL, \
      \  (SELECT sha3a_256(hash) FROM CompactTableChecksum \
      \   WHERE tablename IS NOT NULL ORDER BY tablename)) "

-- | Delete non-active rows from given table.
compactTable :: CompactM ()
compactTable = do
  logg Info "compactTable"
  execM'
      " DELETE FROM $VTABLE$ WHERE rowid NOT IN \
      \ (SELECT t.rowid FROM $VTABLE$ t \
      \  LEFT JOIN CompactActiveVersion v \
      \  WHERE t.rowid = v.vrowid AND v.tablename=?1) "
      [vtable]

-- | For given table, re-compute table grand hash and compare
-- with stored grand hash in CompactTableChecksum.
verifyTable :: CompactM ()
verifyTable = do
  logg Info "verifyTable"
  rs <- qryM
        " SELECT hash FROM CompactTableChecksum WHERE tablename=?1 \
        \ UNION ALL \
        \ SELECT sha3a_256(hash) FROM (SELECT hash FROM $VTABLE$ t1 \
        \  WHERE txid=(select max(txid) FROM $VTABLE$ t2 \
        \   WHERE t2.rowkey=t1.rowkey) GROUP BY rowkey) "
      [vtable]
      [RBlob]
  case rs of
    [[SBlob prev],[SBlob curr]] | prev == curr -> return ()
    _ -> vtable' >>= throwM . CompactExceptionTableVerificationFailure

-- | Drop any versioned tables created after target blockheight.
dropNewTables :: CompactM ()
dropNewTables = do
  logg Info "dropNewTables"
  nts <- qryM
      "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight > ?1"
      [blockheight]
      [RText]

  setTables nts $ withTables $ do
    execM_ "DROP TABLE $VTABLE$"

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
      " DROP TABLE CompactTableChecksum; \
      \ DROP TABLE CompactActiveVersion; "

readGrandHash :: Maybe Utf8 -> CompactM ByteString
readGrandHash tblM = do
  r <- withDb $ \db -> liftIO $ case tblM of
         (Just tbl) -> qry db
             "SELECT hash FROM CompactTableChecksum WHERE tablename = ?1;"
             [SText tbl]
             [RBlob]
         Nothing ->
             qry_ db "SELECT hash FROM CompactTableChecksum WHERE tablename IS NULL" [RBlob]
  case r of
    [[SBlob h]] -> return h
    _ -> throwM $ CompactExceptionInternal $ "invalid/unknown grand hash: " <> sshow (tblM,r)

migrateVersionTables :: CompactM ()
migrateVersionTables = return () -- TODO


compact :: CompactM ByteString
compact = do

    whenFlag Flag_MigrateVersionTables $
        migrateVersionTables

    withTx $ do
      createCompactTableChecksum
      createCompactActiveVersion

    readTxId $ collectVersionedTables $ do

        withTx $ do
          withTables computeTableHash
          computeGlobalHash

        withTx $ do
          withTables $ do
            compactTable
            verifyTable
          dropNewTables
          compactSystemTables

    h <- readGrandHash Nothing

    unlessFlag Flag_KeepCompactTables $ withTx $ dropCompactTables

    return h
