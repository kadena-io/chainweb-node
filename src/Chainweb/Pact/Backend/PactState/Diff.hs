{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.Backend.PactState.Diff
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: see LICENSE.md
--
-- Diff Pact state between two databases.
module Chainweb.Pact.Backend.PactState.Diff
  ( pactDiffMain
  )
  where

import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Logger (logFunctionText, logFunctionJson)
import Chainweb.Utils (HasTextRepresentation, fromText, toText)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName, ChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (TableDiffable(..), getLatestPactStateDiffable, doesPactDbExist, withChainDb)

import System.Exit (exitFailure)
import System.LogLevel (LogLevel(..))

import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S

data PactDiffConfig = PactDiffConfig
  { firstDbDir :: FilePath
  , secondDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , logDir :: FilePath
  }

data Diffy = Difference | NoDifference
  deriving stock (Eq)

instance Semigroup Diffy where
  Difference <> _ = Difference
  _ <> Difference = Difference
  _ <> _          = NoDifference

instance Monoid Diffy where
  mempty = NoDifference

pactDiffMain :: IO ()
pactDiffMain = do
  cfg <- execParser opts

  when (cfg.firstDbDir == cfg.secondDbDir) $ do
    Text.putStrLn "Source and target Pact database directories cannot be the same."
    exitFailure

  let cids = List.sort $ F.toList $ chainIdsAt cfg.chainwebVersion (BlockHeight maxBound)

  diffyRef <- newIORef @(Map ChainId Diffy) M.empty

  forM_ cids $ \cid -> do
    C.withPerChainFileLogger cfg.logDir cid Info $ \logger -> do
      let logText = logFunctionText logger

      sqliteFileExists1 <- doesPactDbExist cid cfg.firstDbDir
      sqliteFileExists2 <- doesPactDbExist cid cfg.secondDbDir

      if | not sqliteFileExists1 -> do
             logText Warn $ "[SQLite for chain in " <> Text.pack cfg.firstDbDir <> " doesn't exist. Skipping]"
         | not sqliteFileExists2 -> do
             logText Warn $ "[SQLite for chain in " <> Text.pack cfg.secondDbDir <> " doesn't exist. Skipping]"
         | otherwise -> do
             withChainDb cid logger cfg.firstDbDir $ \_ (SQLiteEnv db1 _) -> do
               withChainDb cid logger cfg.secondDbDir $ \_ (SQLiteEnv db2 _) -> do
                 logText Info "[Starting diff]"
                 let diff = diffLatestPactState (getLatestPactStateDiffable db1) (getLatestPactStateDiffable db2)
                 diffy <- S.foldMap_ id $ flip S.mapM diff $ \(tblName, tblDiff) -> do
                   logText Info $ "[Starting table " <> tblName <> "]"
                   d <- S.foldMap_ id $ flip S.mapM tblDiff $ \d -> do
                     logFunctionJson logger Warn $ rowKeyDiffExistsToObject d
                     pure Difference
                   logText Info $ "[Finished table " <> tblName <> "]"
                   pure d

                 logText Info $ case diffy of
                   Difference -> "[Non-empty diff]"
                   NoDifference -> "[Empty diff]"
                 logText Info $ "[Finished chain " <> chainIdToText cid <> "]"

                 atomicModifyIORef' diffyRef $ \m -> (M.insert cid diffy m, ())

  diffy <- readIORef diffyRef
  case M.foldMapWithKey (\_ d -> d) diffy of
    Difference -> do
      Text.putStrLn "Diff complete. Differences found."
      exitFailure
    NoDifference -> do
      Text.putStrLn "Diff complete. No differences found."
  where
    opts :: ParserInfo PactDiffConfig
    opts = info (parser <**> helper)
      (fullDesc <> progDesc "Compare two Pact databases")

    parser :: Parser PactDiffConfig
    parser = PactDiffConfig
      <$> strOption
           (long "first-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "First Pact database directory")
      <*> strOption
           (long "second-database-dir"
            <> metavar "PACT_DB_DIRECTORY"
            <> help "Second Pact database directory")
      <*> (fmap (lookupVersionByName . fromTextSilly @ChainwebVersionName) $ strOption
           (long "graph-version"
            <> metavar "CHAINWEB_VERSION"
            <> help "Chainweb version for graph. Only needed for non-standard graphs."
            <> value (toText (_versionName mainnet))
            <> showDefault))
      <*> strOption
           (long "log-dir"
            <> metavar "LOG_DIRECTORY"
            <> help "Directory where logs will be placed"
            <> value ".")

fromTextSilly :: HasTextRepresentation a => Text -> a
fromTextSilly t = case fromText t of
  Just a -> a
  Nothing -> error "fromText failed"

-- | We don't include the entire rowdata in the diff, only the rowkey.
--   This is just a space-saving measure.
data RowKeyDiffExists
  = Old ByteString
    -- ^ The rowkey exists in the same table of the first db, but not the second.
  | New ByteString
    -- ^ The rowkey exists in the same table of the second db, but not the first.
  | Delta ByteString
    -- ^ The rowkey exists in the same table of both dbs, but the rowdata
    --   differs.

rowKeyDiffExistsToObject :: RowKeyDiffExists -> Aeson.Value
rowKeyDiffExistsToObject = \case
  Old rk -> Aeson.object
    [ "old" .= Text.decodeUtf8 rk
    ]
  New rk -> Aeson.object
    [ "new" .= Text.decodeUtf8 rk
    ]
  Delta rk -> Aeson.object
    [ "delta" .= Text.decodeUtf8 rk
    ]

diffTables :: TableDiffable -> TableDiffable -> Stream (Of RowKeyDiffExists) IO ()
diffTables t1 t2 = do
  void $ Merge.mergeA
    (Merge.traverseMaybeMissing $ \rk _rd -> do
      S.yield (Old rk)
      pure Nothing
    )
    (Merge.traverseMaybeMissing $ \rk _rd -> do
      S.yield (New rk)
      pure Nothing
    )
    (Merge.zipWithMaybeAMatched $ \rk rd1 rd2 -> do
      when (rd1 /= rd2) $ do
        S.yield (Delta rk)
      pure Nothing
    )
    t1.rows
    t2.rows

-- This assumes the same tables (essentially zipWith).
--   Note that this assumes we got the state from `getLatestPactState`,
--   because `getPactTableNames` sorts the table names, and `getLatestPactState`
--   sorts the [PactRow] by rowKey.
--
-- If we ever step across two tables that do not have the same name, we throw an error.
--
-- This diminishes the utility of comparing two pact states that are known to be
-- at different heights, but that hurts our ability to perform the diff in
-- constant memory.
diffLatestPactState :: ()
  => Stream (Of TableDiffable) IO ()
  -> Stream (Of TableDiffable) IO ()
  -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
diffLatestPactState = go
  where
  go :: Stream (Of TableDiffable) IO () -> Stream (Of TableDiffable) IO () -> Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
  go s1 s2 = do
    e1 <- liftIO $ S.next s1
    e2 <- liftIO $ S.next s2

    case (e1, e2) of
      (Left (), Left ()) -> do
        pure ()
      (Right _, Left ()) -> do
        error "left stream longer than right"
      (Left (), Right _) -> do
        error "right stream longer than left"
      (Right (t1, next1), Right (t2, next2)) -> do
        when (t1.name /= t2.name) $ do
          error "diffLatestPactState: mismatched table names"
        S.yield (t1.name, diffTables t1 t2)
        go next1 next2

