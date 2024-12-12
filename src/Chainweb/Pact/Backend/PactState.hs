{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Backend.PactState
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: see LICENSE.md
--
-- This module contains various utilities for querying the Pact State.
--
-- The code in this module operates primarily on 'Stream's, because the amount
-- of user data can grow quite large. by comparing one table at a time, we can
-- keep maximum memory utilisation in check.
--
module Chainweb.Pact.Backend.PactState
  ( getPactTableNames
  , getPactTables
  , getLatestPactStateDiffable
  , getLatestPactStateAt
  , withLatestPactStateAt
  , getLatestPactTableNamesAt
  , getLatestPactStateAtDiffable
  , getLatestBlockHeight
  , getEarliestBlockHeight
  , getLatestCommonBlockHeight
  , getEarliestCommonBlockHeight
  , getEndingTxId
  , ensureBlockHeightExists
  , withChainDb
  , addChainIdLabel
  , doesPactDbExist
  , chainDbFileName
  , allChains
  , qryStream

  , PactRow(..)
  , PactRowContents(..)
  , Table(..)
  , TableDiffable(..)
  )
  where

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (Logger, addLabel)
import Chainweb.Pact.Backend.Utils (fromUtf8, withSqliteDb)
import Chainweb.Utils (T2(..), int)
import Chainweb.Version (ChainId, ChainwebVersion, chainIdToText)
import Chainweb.Version.Utils (chainIdsAt)
import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON(..), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3 qualified as SQL
import Database.SQLite3.Direct (Utf8(..), Database)
import Database.SQLite3.Direct qualified as Direct

import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
import Chainweb.Pact.Backend.Types

excludedTables :: [Utf8]
excludedTables = checkpointerTables ++ compactionTables
  where
    checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]
    compactionTables = ["CompactGrandHash", "CompactActiveRow"]

-- | Get the latest blockheight on chain.
getLatestBlockHeight :: Database -> IO BlockHeight
getLatestBlockHeight db = do
  let qryText = "SELECT MAX(blockheight) FROM BlockHistory"
  Pact.qry db qryText [] [RInt] >>= \case
    [[SInt bh]] -> pure (BlockHeight (int bh))
    _ -> error "getLatestBlockHeight: expected int"

getEarliestBlockHeight :: Database -> IO BlockHeight
getEarliestBlockHeight db = do
  let qryText = "SELECT MIN(blockheight) FROM BlockHistory"
  Pact.qry db qryText [] [RInt] >>= \case
    [[SInt bh]] -> pure (BlockHeight (int bh))
    _ -> error "getEarliestBlockHeight: expected int"

-- | Make sure that the blockheight exists on chain.
--
--   Throws an exception if it doesn't.
ensureBlockHeightExists :: Database -> BlockHeight -> IO ()
ensureBlockHeightExists db bh = do
  r <- Pact.qry db "SELECT blockheight FROM BlockHistory WHERE blockheight = ?1" [SInt (fromIntegral bh)] [RInt]
  case r of
    [[SInt rBH]] -> do
      when (fromIntegral bh /= rBH) $ do
        error "ensureBlockHeightExists: malformed query"
    _ -> do
      error $ "ensureBlockHeightExists: empty BlockHistory: height=" ++ show bh

getLatestCommonBlockHeight :: (Logger logger)
  => logger
  -> FilePath
  -> [ChainId]
  -> IO BlockHeight
getLatestCommonBlockHeight logger path cids = do
  fmap minimum $ forM cids $ \cid -> withChainDb cid logger path $ \_ sqlEnv -> do
    getLatestBlockHeight sqlEnv

getEarliestCommonBlockHeight :: (Logger logger)
  => logger
  -> FilePath
  -> [ChainId]
  -> IO BlockHeight
getEarliestCommonBlockHeight logger path cids = do
  fmap maximum $ forM cids $ \cid -> withChainDb cid logger path $ \_ sqlEnv -> do
    getEarliestBlockHeight sqlEnv

-- | Wrapper around 'withSqliteDb' that adds the chainId label to the logger
--   and sets resetDb to False.
withChainDb :: (Logger logger)
  => ChainId
  -> logger
  -> FilePath
  -> (logger -> SQLiteEnv -> IO x)
  -> IO x
withChainDb cid logger' path f = do
  let logger = addChainIdLabel cid logger'
  let resetDb = False
  withSqliteDb cid logger path resetDb (f logger)

-- | Get all Pact table names in the database.
getPactTableNames :: Database -> Stream (Of Utf8) IO ()
getPactTableNames db = S.each =<< liftIO (do
  let sortedTableNames :: [[SType]] -> [Utf8]
      sortedTableNames rows = List.sortOn (Text.toLower . fromUtf8) $ flip List.map rows $ \case
        [SText u] -> u
        _ -> error "getPactTableNames.sortedTableNames: expected text"

  fmap sortedTableNames $ do
    let qryText =
          "SELECT name FROM sqlite_schema \
          \WHERE \
          \  type = 'table' \
          \AND \
          \  name NOT LIKE 'sqlite_%'"
    Pact.qry db qryText [] [RText])

-- | Get all of the rows for each table. The tables will be appear sorted
--   lexicographically by table name.
getPactTables :: Database -> Stream (Of Table) IO ()
getPactTables db = do
  let fmtTable x = "\"" <> x <> "\""

  getPactTableNames db
    & S.filter (\tbl -> tbl `notElem` excludedTables)
    & S.mapM (\tbl -> do
        let qryText = "SELECT rowkey, rowdata, txid FROM "
              <> fmtTable tbl
        userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
        shapedRows <- forM userRows $ \case
          [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
            pure $ PactRow {..}
          _ -> error "getPactTableNames: unexpected shape of user table row"
        pure $ Table (fromUtf8 tbl) shapedRows
      )

-- streaming SQLite step; see Pact SQLite module
stepStatement :: Direct.Statement -> [RType] -> Stream (Of [SType]) IO (Either Direct.Error ())
stepStatement stmt rts = runExceptT $ do
  -- todo: rename from acc
  let acc :: Direct.StepResult -> ExceptT Direct.Error (Stream (Of [SType]) IO) ()
      acc = \case
        Direct.Done -> do
          pure ()
        Direct.Row -> do
          as <- forM (List.zip [0..] rts) $ \(colIx, expectedColType) -> do
            liftIO $ case expectedColType of
              RInt -> SInt <$> Direct.columnInt64 stmt colIx
              RDouble -> SDouble <$> Direct.columnDouble stmt colIx
              RText -> SText <$> Direct.columnText stmt colIx
              RBlob -> SBlob <$> Direct.columnBlob stmt colIx
          lift $ S.yield as
          liftIO (Direct.step stmt) >>= \case
            Left err -> do
              throwError err
            Right sr -> do
              acc sr

  -- maybe use stepNoCB
  ExceptT (liftIO (Direct.step stmt)) >>= acc

-- | Prepare/execute query with params; stream the results
qryStream :: ()
  => Database
  -> Utf8
  -> [SType]
  -> [RType]
  -> (Stream (Of [SType]) IO (Either Direct.Error ()) -> IO x)
  -> IO x
qryStream db qryText args returnTypes k = do
  bracket (SQL.prepareUtf8 db qryText) Direct.finalize $ \stmt -> do
    bindParams stmt args
    k (stepStatement stmt returnTypes)
  where
    bindParams :: Direct.Statement -> [SType] -> IO ()
    bindParams s as = forM_ (List.zip [1..] as) $ \(argIndex, arg) -> do
      case arg of
        SInt a -> Direct.bindInt64 s argIndex a
        SDouble a -> Direct.bindDouble s argIndex a
        SText a -> Direct.bindText s argIndex a
        SBlob a -> Direct.bindBlob s argIndex a

-- | Get the latest Pact state (in a ready-to-diff form).
getLatestPactStateDiffable :: Database -> Stream (Of TableDiffable) IO ()
getLatestPactStateDiffable db = do
  bh <- liftIO $ getLatestBlockHeight db
  getLatestPactStateAtDiffable db bh

-- | Get the Pact state (in a ready-to-diff form) at the given height.
getLatestPactStateAtDiffable :: ()
  => Database
  -> BlockHeight
  -> Stream (Of TableDiffable) IO ()
getLatestPactStateAtDiffable db bh = do
  flip S.map (getLatestPactStateAt db bh) $ \(tblName, state) ->
    TableDiffable tblName (M.map (\prc -> prc.rowData) state)

getEndingTxId :: ()
  => Database
  -> BlockHeight
  -> IO Int64
getEndingTxId db bh = do
  r <- liftIO $ Pact.qry db
         "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
         [SInt (int bh)]
         [RInt]
  case r of
    [[SInt txId]] -> pure txId
    _ -> error "getEndingTxId: expected int"

-- | Get the Pact state at the given height.
getLatestPactStateAt :: ()
  => Database
  -> BlockHeight
  -> Stream (Of (Text, Map ByteString PactRowContents)) IO ()
getLatestPactStateAt db bh = do
  endingTxId <- liftIO $ getEndingTxId db bh

  getLatestPactTableNamesAt db bh
    & S.mapM (\tbl -> do
        let qryText = "SELECT rowkey, rowdata, txid FROM "
              <> "\"" <> tbl <> "\""
              <> " WHERE txid<?"
        latestState <- liftIO $ qryStream db qryText [SInt endingTxId] [RText, RBlob, RInt] $ \rows -> do
          let go :: Map ByteString PactRowContents -> [SType] -> Map ByteString PactRowContents
              go m = \case
                [SText (Utf8 rowKey), SBlob rowData, SInt txId] ->
                  M.insertWith (\prc1 prc2 -> if prc1.txId > prc2.txId then prc1 else prc2) rowKey (PactRowContents rowData txId) m
                _ -> error "getLatestPactState: unexpected shape of user table row"
          S.fold_ go M.empty id rows
        pure (fromUtf8 tbl, latestState)
      )

getLatestPactTableNamesAt :: ()
  => Database
  -> BlockHeight
  -> Stream (Of Utf8) IO ()
getLatestPactTableNamesAt db bh = do
  tablesCreatedAfter <- liftIO $ do
    let qryText = "SELECT tablename FROM VersionedTableCreation WHERE createBlockheight > ?1"
    rows <- Pact.qry db qryText [SInt (int bh)] [RText]
    forM rows $ \case
      [SText tbl] -> pure tbl
      _ -> error "getLatestPactStateAt.tablesCreatedAfter: expected text"

  let excludeThese = excludedTables ++ tablesCreatedAfter
  getPactTableNames db
    & S.filter (\tbl -> tbl `notElem` excludeThese)

-- | Use the Pact State at the given height.
withLatestPactStateAt :: ()
  => Database
  -> BlockHeight
  -> (forall r. Text -> Stream (Of (T2 Int64 PactRow)) IO r -> IO ())
  -> IO ()
withLatestPactStateAt db bh withTable = do
  endingTxId <- liftIO $ getEndingTxId db bh

  getLatestPactTableNamesAt db bh
    & S.mapM_ (\tbl -> do
        -- ❯ sqlite3 pact-v1-chain-0.sqlite 'EXPLAIN QUERY PLAN SELECT rowid, rowkey, rowdata, txid FROM [coin_coin-table] WHERE txid < 100 ORDER BY rowid DESC'
        -- QUERY PLAN
        -- `--SCAN coin_coin-table

        let qryText = "SELECT rowid, rowkey, txid, rowdata FROM "
              <>  "[" <> tbl <> "]"
              <> " WHERE txid < ?1"
              <> " ORDER BY rowid DESC"

        qryStream db qryText [SInt endingTxId] [RInt, RText, RInt, RBlob] $ \rows -> do
          let go :: ()
                => Set ByteString
                -> Stream (Of [SType]) IO (Either Direct.Error ())
                -> Stream (Of (T2 Int64 PactRow)) IO ()
              go !seen s = do
                e <- liftIO (S.next s)
                case e of
                  Left (Left sqlErr) -> do
                    error $ "withLatestPactStateAt: Encountered SQLite error: " <> show sqlErr
                  Left (Right ()) -> do
                    pure ()
                  Right (row, rest) -> do
                    case row of
                      [SInt rowid, SText (Utf8 rk), SInt tid, SBlob rd] -> do
                        let element = T2 rowid $ PactRow
                              { rowKey = rk
                              , rowData = rd
                              , txId = tid
                              }
                        if | tbl == "SYS:Pacts" && rd == "null" -> do
                               S.yield element
                               go (Set.insert rk seen) rest
                           | rk `Set.member` seen -> do
                               go seen rest
                           | otherwise -> do
                               S.yield element
                               go (Set.insert rk seen) rest
                      _ -> do
                        error "getLatestPactState: invalid query"

          withTable (fromUtf8 tbl) (go Set.empty rows)
      )

-- | A pact table - just its name and its rows.
data Table = Table
  { name :: Text
  , rows :: [PactRow]
  }
  deriving stock (Eq, Show)

-- | A diffable pact table - its name and the _active_ pact state
--   as a Map from rowkey to rowdata.
data TableDiffable = TableDiffable
  { name :: Text
  , rows :: Map ByteString ByteString
  }
  deriving stock (Eq, Ord, Show)

data PactRow = PactRow
  { rowKey :: ByteString
  , rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Show)

instance Ord PactRow where
  compare pr1 pr2 =
    compare pr1.txId pr2.txId
    <> compare pr1.rowKey pr2.rowKey
    <> compare pr1.rowData pr2.rowData

instance ToJSON PactRow where
  toJSON pr = Aeson.object
    [ "row_key" .= Text.decodeUtf8 pr.rowKey
    , "row_data" .= Text.decodeUtf8 pr.rowData
    , "tx_id" .= pr.txId
    ]

data PactRowContents = PactRowContents
  { rowData :: ByteString
  , txId :: Int64
  }
  deriving stock (Eq, Show)

-- | Given a pact database directory, check to see if it
--   contains the pact db for the given ChainId.
doesPactDbExist :: ChainId -> FilePath -> IO Bool
doesPactDbExist cid dbDir = do
  doesFileExist (chainDbFileName cid dbDir)

-- | Given a pact database directory, return the SQLite
--   path chainweb uses for the given ChainId.
chainDbFileName :: ChainId -> FilePath -> FilePath
chainDbFileName cid dbDir = dbDir </> mconcat
  [ "pact-v1-chain-"
  , Text.unpack (chainIdToText cid)
  , ".sqlite"
  ]


addChainIdLabel :: (Logger logger)
  => ChainId
  -> logger
  -> logger
addChainIdLabel cid = addLabel ("chainId", chainIdToText cid)

allChains :: ChainwebVersion -> [ChainId]
allChains v = List.sort $ F.toList $ chainIdsAt v (BlockHeight maxBound)
