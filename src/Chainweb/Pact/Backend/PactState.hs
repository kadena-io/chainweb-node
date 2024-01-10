{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

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
  , getLatestPactState
  , getLatestPactStateDiffable
  , getLatestPactStateAt
  , getLatestPactStateAtDiffable
  , getLatestBlockHeight
  , getEarliestBlockHeight
  , ensureBlockHeightExists
  , withChainDb
  , doesPactDbExist

  , PactRow(..)
  , PactRowContents(..)
  , Table(..)
  , TableDiffable(..)
  )
  where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Data.Aeson (ToJSON(..), (.=))
import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite3.Direct (Utf8(..), Database)
import Database.SQLite3.Direct qualified as SQL

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (Logger, addLabel)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, withSqliteDb)
import Chainweb.Utils (int)
import Chainweb.Version (ChainId, chainIdToText)

import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Pact.Types.SQLite (SType(..), RType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

excludedTables :: [Utf8]
excludedTables = checkpointerTables ++ compactionTables
  where
    checkpointerTables = ["BlockHistory", "VersionedTableCreation", "VersionedTableMutation", "TransactionIndex"]
    compactionTables = ["CompactGrandHash", "CompactActiveRow"]

getLatestBlockHeight :: Database -> IO BlockHeight
getLatestBlockHeight db = do
  let qryText = "SELECT MAX(blockheight) FROM BlockHistory"
  Pact.qry db qryText [] [RInt] >>= \case
    [[SInt bh]] -> pure (BlockHeight (int bh))
    _ -> error "getLatestBlockHeight: expected int"

-- | Get the earliest blockheight on chain.
getEarliestBlockHeight :: Database -> IO BlockHeight
getEarliestBlockHeight db = do
  r <- Pact.qry db "SELECT blockheight FROM BlockHistory ORDER BY blockheight ASC LIMIT 1" [] [RInt]
  case r of
    [[SInt bh]] -> do
      pure (fromIntegral bh)
    _ -> do
      error "getEarliestBlockHeight: no earliest blockheight"

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

-- | Wrapper around 'withSqliteDb' that adds the chainId label to the logger
--   and sets resetDb to False.
withChainDb :: (Logger logger)
  => ChainId
  -> logger
  -> FilePath
  -> (SQLiteEnv -> IO x)
  -> IO x
withChainDb cid logger' path f = do
  let logger = addLabel ("chainId", chainIdToText cid) logger'
  let resetDb = False
  withSqliteDb cid logger path resetDb f

getPactTableNames :: Database -> IO (Vector Utf8)
getPactTableNames db = do
  let sortedTableNames :: [[SType]] -> [Utf8]
      sortedTableNames rows = List.sortOn fromUtf8 $ flip List.map rows $ \case
        [SText u] -> u
        _ -> error "getPactTableNames.sortedTableNames: expected text"

  tables <- fmap sortedTableNames $ do
    let qryText =
          "SELECT name FROM sqlite_schema \
          \WHERE \
          \  type = 'table' \
          \AND \
          \  name NOT LIKE 'sqlite_%'"
    Pact.qry db qryText [] [RText]

  pure (Vector.fromList tables)

-- | Get all of the rows for each table. The tables will be sorted
--   lexicographically by name.
getPactTables :: Database -> Stream (Of Table) IO ()
getPactTables db = do
  let fmtTable x = "\"" <> x <> "\""

  tables <- liftIO $ getPactTableNames db

  forM_ tables $ \tbl -> do
    if tbl `notElem` excludedTables
    then do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> fmtTable tbl
      userRows <- liftIO $ Pact.qry db qryText [] [RText, RBlob, RInt]
      shapedRows <- forM userRows $ \case
        [SText (Utf8 rowKey), SBlob rowData, SInt txId] -> do
          pure $ PactRow {..}
        _ -> error "getPactTableNames: unexpected shape of user table row"
      S.yield $ Table (fromUtf8 tbl) shapedRows
    else do
      pure ()

-- streaming SQLite step; see Pact SQLite module
stepStatement :: SQL.Statement -> [RType] -> Stream (Of [SType]) IO (Either SQL.Error ())
stepStatement stmt rts = runExceptT $ do
  -- todo: rename from acc
  let acc :: SQL.StepResult -> ExceptT SQL.Error (Stream (Of [SType]) IO) ()
      acc = \case
        SQL.Done -> do
          pure ()
        SQL.Row -> do
          as <- forM (List.zip [0..] rts) $ \(colIx, expectedColType) -> do
            liftIO $ case expectedColType of
              RInt -> SInt <$> SQL.columnInt64 stmt colIx
              RDouble -> SDouble <$> SQL.columnDouble stmt colIx
              RText -> SText <$> SQL.columnText stmt colIx
              RBlob -> SBlob <$> SQL.columnBlob stmt colIx
          lift $ S.yield as
          liftIO (SQL.step stmt) >>= \case
            Left err -> do
              throwError err
            Right sr -> do
              acc sr

  -- maybe use stepNoCB
  ExceptT (liftIO (SQL.step stmt)) >>= acc

-- | Prepare/execute query with params; stream the results
qry :: ()
  => Database
  -> Utf8
  -> [SType]
  -> [RType]
  -> (Stream (Of [SType]) IO (Either SQL.Error ()) -> IO x)
  -> IO x
qry db qryText args returnTypes k = do
  bracket (Pact.prepStmt db qryText) SQL.finalize $ \stmt -> do
    Pact.bindParams stmt args
    k (stepStatement stmt returnTypes)

getLatestPactStateDiffable :: Database -> Stream (Of TableDiffable) IO ()
getLatestPactStateDiffable db = do
  bh <- liftIO $ getLatestBlockHeight db
  getLatestPactStateAtDiffable db bh

getLatestPactState :: Database -> Stream (Of (Text, Map ByteString PactRowContents)) IO ()
getLatestPactState db = do
  bh <- liftIO $ getLatestBlockHeight db
  getLatestPactStateAt db bh

getLatestPactStateAtDiffable :: ()
  => Database
  -> BlockHeight
  -> Stream (Of TableDiffable) IO ()
getLatestPactStateAtDiffable db bh = do
  flip S.map (getLatestPactStateAt db bh) $ \(tblName, state) ->
    TableDiffable tblName (M.map (\prc -> prc.rowData) state)

getLatestPactStateAt :: ()
  => Database
  -> BlockHeight
  -> Stream (Of (Text, Map ByteString PactRowContents)) IO ()
getLatestPactStateAt db bh = do
  endingTxId <- do
    r <- liftIO $ Pact.qry db
           "SELECT endingtxid FROM BlockHistory WHERE blockheight=?"
           [SInt (int bh)]
           [RInt]
    case r of
      [[SInt txId]] -> pure txId
      _ -> error "expected int"

  tables <- liftIO $ getPactTableNames db

  forM_ tables $ \tbl -> do
    when (tbl `notElem` excludedTables) $ do
      let qryText = "SELECT rowkey, rowdata, txid FROM "
            <> "\"" <> tbl <> "\""
            <> " WHERE txid<?"
      latestState <- liftIO $ qry db qryText [SInt endingTxId] [RText, RBlob, RInt] $ \rows -> do
        let go :: Map ByteString PactRowContents -> [SType] -> Map ByteString PactRowContents
            go m = \case
              [SText (Utf8 rowKey), SBlob rowData, SInt txId] ->
                M.insertWith (\prc1 prc2 -> if prc1.txId > prc2.txId then prc1 else prc2) rowKey (PactRowContents rowData txId) m
              _ -> error "getLatestPactState: unexpected shape of user table row"
        S.fold_ go M.empty id rows
      S.yield (fromUtf8 tbl, latestState)

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
  let chainDbFileName = mconcat
        [ "pact-v1-chain-"
        , Text.unpack (chainIdToText cid)
        , ".sqlite"
        ]
  let file = dbDir </> chainDbFileName
  doesFileExist file
