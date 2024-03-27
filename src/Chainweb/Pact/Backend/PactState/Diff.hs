{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
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
  ( main
  )
  where

import Streaming.Prelude qualified as S
import Chainweb.BlockHeight (BlockHeight)
import Chainweb.Logger (logFunctionText, logFunctionJson)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (TableDiffable(..), getLatestPactStateAtDiffable, doesPactDbExist, withChainDb, allChains)
import Chainweb.Utils (fromText, toText)
import Chainweb.Version (ChainwebVersion(..), ChainId, chainIdToText)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Map (Map)
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import Streaming.Prelude (Stream, Of)
import System.Exit (exitFailure)
import System.LogLevel (LogLevel(..))

data PactDiffConfig = PactDiffConfig
  { firstDbDir :: FilePath
  , secondDbDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , target :: BlockHeight
  , logDir :: FilePath
  }

data IsDifferent = Difference | NoDifference
  deriving stock (Eq)

instance Semigroup IsDifferent where
  Difference <> _ = Difference
  _ <> Difference = Difference
  _ <> _          = NoDifference

instance Monoid IsDifferent where
  mempty = NoDifference

main :: IO ()
main = do
  cfg <- execParser opts

  when (cfg.firstDbDir == cfg.secondDbDir) $ do
    Text.putStrLn "Source and target Pact database directories cannot be the same."
    exitFailure

  let cids = allChains cfg.chainwebVersion

  isDifferentRef <- newIORef @(Map ChainId IsDifferent) M.empty

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
             withChainDb cid logger cfg.firstDbDir $ \_ db1 -> do
               withChainDb cid logger cfg.secondDbDir $ \_ db2 -> do
                 logText Info "[Starting diff]"
                 let getPactState db = getLatestPactStateAtDiffable db cfg.target
                 let diff :: Stream (Of (Text, Stream (Of RowKeyDiffExists) IO ())) IO ()
                     diff = diffLatestPactState (getPactState db1) (getPactState db2)
                 isDifferent <- S.foldMap_ id $ flip S.mapM diff $ \(tblName, tblDiff) -> do
                   logText Info $ "[Starting table " <> tblName <> "]"
                   d <- S.foldMap_ id $ flip S.mapM tblDiff $ \d -> do
                     logFunctionJson logger Warn $ rowKeyDiffExistsToObject d
                     pure Difference
                   logText Info $ "[Finished table " <> tblName <> "]"
                   pure d

                 logText Info $ case isDifferent of
                   Difference -> "[Non-empty diff]"
                   NoDifference -> "[Empty diff]"
                 logText Info $ "[Finished chain " <> chainIdToText cid <> "]"

                 atomicModifyIORef' isDifferentRef $ \m -> (M.insert cid isDifferent m, ())

  isDifferent <- readIORef isDifferentRef
  case M.foldMapWithKey (\_ d -> d) isDifferent of
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
      <$> strOption (long "first-database-dir" <> help "First Pact database directory")
      <*> strOption (long "second-database-dir" <> help "Second Pact database directory")
      <*> fmap parseChainwebVersion (strOption (long "graph-version" <> help "Chainweb version for graph. Only needed for non-standard graphs." <> value (toText (_versionName mainnet)) <> showDefault))
      <*> fmap (fromIntegral @Int) (option auto (long "target-blockheight" <> metavar "BLOCKHEIGHT" <> help "Target Blockheight"))
      <*> strOption (long "log-dir" <> help "Directory where logs will be placed" <> value ".")

    parseChainwebVersion :: Text -> ChainwebVersion
    parseChainwebVersion = lookupVersionByName . fromMaybe (error "ChainwebVersion parse failed") . fromText

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
--   Note that this assumes we got the state from `getLatestPactStateDiffable`,
--   because `getPactTableNames` sorts the table names, and
--   `getLatestPactStateDiffable` sorts the [PactRow] by rowKey.
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
          error $ "diffLatestPactState: mismatched table names: " <> Text.unpack t1.name <> " vs. " <> Text.unpack t2.name
        S.yield (t1.name, diffTables t1 t2)
        go next1 next2
