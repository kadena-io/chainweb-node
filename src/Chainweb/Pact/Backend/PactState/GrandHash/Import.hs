{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the pact-import tool. The tool is designed so
--   that users can import a cryptographically verified pact database
--   into their node. The design is a bit roundabout, because care is taken
--   to make it hard for users to footgun themselves.
--
--   Some quick background knowledge:
--     chainweb-node has 'Snapshot's embedded into it.
--     Snapshots are tuples of 'ChainGrandHash' and 'BlockHeader'. They are
--     indexed by 'BlockHeight' and then 'ChainId'. In other words,
--     for an arbitrary number of unique 'BlockHeight's, there is a
--     'Map' 'ChainId' 'Snapshot'. This means that the database has
--     'Snapshot's across each chain for each 'BlockHeight'. These embedded
--     snapshots are computed by the `pact-calc` tool.
--
--   The tool works like this:
--     - See what the latest common (i.e, across all chains) blockheight in RocksDb is.
--       E.g. if all chains are at blockheight 1_000 but one chain is at 999 and
--       another chain is at 998, then the latest common blockheight across all
--       chains is 998. Call this the latestCommonBlockHeight.
--     - Find the first embedded snapshot associated with
--       a blockheight <= latestCommonBlockHeight. Call this
--       'snapshotBlockHeight'.
--       E.g. say the db has latest height 3_980_000, but the latest embedded
--       snapshot is at 3_800_000. This means that we will pick the embedded
--       snapshot at blockheight 3_800_000.
--     - Sets the environment variable `SNAPSHOTview blockHeight`, which is useful
--       for debugging and/or consumption by other tools, such as pact-diff.
--     - Go into the db and compute the blockheader and pact grandhash
--       ('Snapshot') at 'snapshotBlockHeight'.
--     - Compare the computed snapshot against what's embedded in the
--       node. If there is a mismatch, we fail.
--     - If the user specified a target database directory, we copy the
--       sqlite files in the directory we've been operating on over to it.
--     - Then, drop anything in the target directory after the verified state.
--       This is accomplished via Checkpointer rewind.
--
module Chainweb.Pact.Backend.PactState.GrandHash.Import
  (
    pactImportMain

  , pactVerify
  , pactDropPostVerified
  )
  where

import Chainweb.BlockHeader (ParentHeader(..), blockHash)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (addChainIdLabel, allChains)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot.Mainnet qualified as MainnetSnapshots
import Chainweb.Pact.Backend.PactState.GrandHash.Utils (resolveLatestCutHeaders, resolveCutHeadersAtHeight, computeGrandHashesAt, exitLog, withConnections, chainwebDbFilePath, rocksParser, cwvParser)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Utils (sshow)
import Chainweb.Version (ChainwebVersion(..))
import Control.Applicative (optional)
import Control.Lens ((^?!), ix, view)
import Control.Monad (forM_, when)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..))
import Data.Text qualified as Text
import Options.Applicative (ParserInfo, Parser, (<**>))
import Options.Applicative qualified as O
import Patience.Map qualified as P
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Environment (setEnv)
import System.LogLevel (LogLevel(..))
import qualified Chainweb.Pact.PactService.Checkpointer.Internal as Checkpointer.Internal

-- | Verifies that the hashes and headers match @grands@.
--
--   Returns the latest (highest) blockheight along with the snapshot
--   thereat.
pactVerify :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
     -- ^ pact connections
  -> RocksDb
     -- ^ rocksDb
  -> [(BlockHeight, HashMap ChainId Snapshot)]
     -- ^ grands
  -> IO (BlockHeight, HashMap ChainId Snapshot)
pactVerify logger v pactConns rocksDb grands = do
  logFunctionText logger Debug "Starting pact-verify"

  -- Get the highest common blockheight across all chains.
  latestBlockHeight <- fst <$> resolveLatestCutHeaders logger v pactConns rocksDb

  snapshot@(snapshotBlockHeight, expectedChainHashes) <- do
    -- Find the first element of 'grands' such that
    -- the blockheight therein is less than or equal to
    -- the argument latest common blockheight.
    case List.find (\g -> latestBlockHeight >= fst g) (List.sortOn (Down . fst) grands) of
      Nothing -> do
        exitLog logger "No snapshot older than latest block"
      Just s -> do
        pure s

  chainHashes <- do
    chainTargets <- resolveCutHeadersAtHeight logger v pactConns rocksDb snapshotBlockHeight
    computeGrandHashesAt pactConns chainTargets

  let deltas = Map.filter (not . P.isSame)
        $ P.diff (hashMapToMap expectedChainHashes) (hashMapToMap chainHashes)

  forM_ (Map.toAscList deltas) $ \(cid, delta) -> do
    let logger' = addChainIdLabel cid logger
    case delta of
      P.Same _ -> pure ()
      P.Old _ -> exitLog logger' "pact-import: internal logic error: chain mismatch"
      P.New _ -> exitLog logger' "pact-import: internal logic error: chain mismatch"
      P.Delta (Snapshot eHash eHeader) (Snapshot hash header) -> do
        when (header /= eHeader) $ do
          logFunctionText logger' Error $ Text.unlines
            [ "Chain " <> chainIdToText cid
            , "Block Header mismatch"
            , "  Expected: " <> sshow (view blockHash eHeader)
            , "  Actual:   " <> sshow (view blockHash header)
            ]

        when (hash /= eHash) $ do
          logFunctionText logger' Error $ Text.unlines
            [ "Chain " <> chainIdToText cid
            , "Grand Hash mismatch"
            , "  Expected: " <> sshow eHash
            , "  Actual:   " <> sshow hash
            ]
  when (Map.size deltas > 0) $ do
    exitLog logger $ "Hashes did not align at BlockHeight " <> sshow snapshotBlockHeight

  logFunctionText logger Info $ "Hashes aligned at BlockHeight " <> sshow snapshotBlockHeight
  pure snapshot

pactDropPostVerified :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath
     -- ^ source pact dir
  -> FilePath
     -- ^ target pact dir
  -> BlockHeight
     -- ^ highest verified blockheight
  -> HashMap ChainId Snapshot
     -- ^ Grand Hashes & BlockHeaders at this blockheight
  -> IO ()
pactDropPostVerified logger v srcDir tgtDir snapshotBlockHeight snapshotChainHashes = do
  logFunctionText logger Info $ "Creating " <> Text.pack tgtDir
  createDirectoryIfMissing True tgtDir

  let chains = HM.keys snapshotChainHashes

  forM_ chains $ \cid -> do
    let srcDb = chainwebDbFilePath cid srcDir
    let tgtDb = chainwebDbFilePath cid tgtDir
    let logger' = addChainIdLabel cid logger

    logFunctionText logger' Info
      $ "Copying contents of "
        <> Text.pack srcDb
        <> " to "
        <> Text.pack tgtDb
    copyFile srcDb tgtDb

  withConnections logger tgtDir chains $ \tgtConns -> do
    forM_ chains $ \cid -> do
      let sqliteEnv = tgtConns ^?! ix cid
      let logger' = addChainIdLabel cid logger
      logFunctionText logger' Info
        $ "Dropping anything post verified state (BlockHeight " <> sshow snapshotBlockHeight <> ")"
      Checkpointer.Internal.withCheckpointerResources logger defaultModuleCacheLimit sqliteEnv DoNotPersistIntraBlockWrites v cid $ \cp -> do
        Checkpointer.Internal.rewindTo cp (Just $ ParentHeader $ blockHeader $ snapshotChainHashes ^?! ix cid)

data PactImportConfig = PactImportConfig
  { sourcePactDir :: FilePath
  , targetPactDir :: Maybe FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  }

pactImportMain :: IO ()
pactImportMain = do
  cfg <- O.execParser opts

  let chains = allChains cfg.chainwebVersion

  C.withDefaultLogger Info $ \logger -> do
    withConnections logger cfg.sourcePactDir chains $ \srcConns -> do
      withReadOnlyRocksDb cfg.rocksDir modernDefaultOptions $ \rocksDb -> do
        (snapshotBlockHeight, snapshotChainHashes) <- pactVerify logger cfg.chainwebVersion srcConns rocksDb MainnetSnapshots.grands

        -- Set this before pactDropPostVerified, in case there's a failure, so
        -- it's still recoverable.
        --
        -- pact-import doesn't use this environment variable; it's for
        -- debugging and/or consumption by other tools.
        setEnv "SNAPSHOT_BLOCKHEIGHT" (show snapshotBlockHeight)

        forM_ cfg.targetPactDir $ \targetDir -> do
          pactDropPostVerified logger cfg.chainwebVersion cfg.sourcePactDir targetDir snapshotBlockHeight snapshotChainHashes
  where
    opts :: ParserInfo PactImportConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compare the grand hash of a Pact database to an expected value."
      , "If the hash matches, and a target directory is specificied, the"
      , "database will be copied to the target directory, and any state"
      , "later than what is cryptographically verifiable will be dropped."
      , "This tool sets the environment variable `SNAPSHOT_BLOCKHEIGHT` which"
      , "can be useful for debugging, or if you want to use the blockheight"
      , "for your own queries."
      ]

    parser :: Parser PactImportConfig
    parser = PactImportConfig
      <$> O.strOption
            (O.long "source-database-dir"
             <> O.short 's'
             <> O.metavar "PACT DBDIR"
             <> O.help "Source Pact database directory, which you wish to verify"
            )
      <*> optional (O.strOption
            (O.long "target-database-dir"
             <> O.short 't'
             <> O.metavar "PACT DBDIR"
             <> O.help "Target directory to copy the verified Pact database into"
            ))
      <*> rocksParser
      <*> cwvParser

hashMapToMap :: (Hashable k, Ord k) => HashMap k a -> Map k a
hashMapToMap = Map.fromList . HM.toList
