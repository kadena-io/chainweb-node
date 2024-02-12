{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Pact.Backend.PactState.GrandHash.Import
  (
    pactImportMain

  , pactVerify
  , pactDropPostVerified
  )
  where

import Chainweb.BlockHeader (BlockHeader(..), ParentHeader(..), genesisHeight)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (addChainIdLabel, allChains)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot.Mainnet qualified as MainnetSnapshots
import Chainweb.Pact.Backend.PactState.GrandHash.Utils (resolveLatestCutHeaders, resolveCutHeadersAtHeight, computeGrandHashesAt, exitLog, withConnections, chainwebDbFilePath, rocksParser, cwvParser)
import Chainweb.Pact.Backend.RelationalCheckpointer (withProdRelationalCheckpointer)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..), initBlockState, _cpRewindTo)
import Chainweb.Pact.Service.Types (PersistIntraBlockWrites(..))
import Chainweb.Pact.Types (defaultModuleCacheLimit)
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Utils (sshow)
import Chainweb.Version (ChainwebVersion(..))
import Control.Applicative (optional)
import Control.Lens ((^?!), ix)
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
import System.LogLevel (LogLevel(..))

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
            , "  Expected: " <> sshow (_blockHash eHeader)
            , "  Actual:   " <> sshow (_blockHash header)
            ]

        when (hash /= eHash) $ do
          logFunctionText logger' Error $ Text.unlines
            [ "Chain " <> chainIdToText cid
            , "Grand Hash mismatch"
            , "  Expected: " <> sshow eHash
            , "  Actual:   " <> sshow hash
            ]
  when (Map.size deltas > 0) $ do
    exitLog logger "Hashes did not align."

  logFunctionText logger Info "Hashes aligned"
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
      let bState = initBlockState defaultModuleCacheLimit (genesisHeight v cid)
      withProdRelationalCheckpointer logger bState sqliteEnv PersistIntraBlockWrites v cid $ \cp -> do
        _cpRewindTo cp (Just $ ParentHeader $ blockHeader $ snapshotChainHashes ^?! ix cid)

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

        forM_ cfg.targetPactDir $ \targetDir -> do
          pactDropPostVerified logger cfg.chainwebVersion cfg.sourcePactDir targetDir snapshotBlockHeight snapshotChainHashes
  where
    opts :: ParserInfo PactImportConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compare the grand hash of a Pact database to an expected value."
      , "If the hash matches, optionally import the database into a target directory"
      , "and delete any newer state not included in the hash."
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

{-
versionFromText :: (HasCallStack) => Text -> ChainwebVersion
versionFromText t = case fromText @ChainwebVersionName t of
  Just a -> lookupVersionByName a
  Nothing -> error $ "Invalid chainweb version name: " ++ Text.unpack t

checkPactDbsExist :: FilePath -> [ChainId] -> IO ()
checkPactDbsExist dbDir cids = pooledFor_ cids $ \cid -> do
  e <- doesFileExist (chainwebDbFilePath cid dbDir)
  when (not e) $ do
    error $ "Pact database doesn't exist for expected chain id " <> Text.unpack (chainIdToText cid)

chainwebDbFilePath :: ChainId -> FilePath -> FilePath
chainwebDbFilePath cid dbDir =
  let fileName = mconcat
        [ "pact-v1-chain-"
        , Text.unpack (chainIdToText cid)
        , ".sqlite"
        ]
  in dbDir </> fileName
-}

hashMapToMap :: (Hashable k, Ord k) => HashMap k a -> Map k a
hashMapToMap = Map.fromList . HM.toList
