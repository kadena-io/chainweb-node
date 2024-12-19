{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash.Utils
  (

    withConnections
  , resolveLatestCutHeaders
  , resolveCutHeadersAtHeight
  , resolveCutHeadersAtHeights
  , computeGrandHashesAt
  , exitLog
  , hex
  , chainwebDbFilePath
  , rocksParser
  , cwvParser
  )
  where

import Chainweb.BlockHeader (BlockHeader, blockHeight)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.PactState (getLatestPactStateAt, getLatestBlockHeight, addChainIdLabel)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (computeGrandHash)
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils (startSqliteDb, stopSqliteDb)
import Chainweb.Storage.Table.RocksDB (RocksDb)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils (fromText, toText, sshow)
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.WebBlockHeaderDB (WebBlockHeaderDb, getWebBlockHeaderDb, initWebBlockHeaderDb)
import Control.Exception (bracket)
import Control.Lens ((^?!), ix, view)
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative qualified as O
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.LogLevel (LogLevel(..))
import UnliftIO.Async (pooledForConcurrently, pooledForConcurrently_)

-- | Get the cut headers at the specified blockheight.
--
--   This works by taking the latest cut headers (from 'readLatestCutHeaders')
--   and calling 'seekAncestor' to find the 'BlockHeader's associated with the
--   specified blockheight at each chain (this is the cut header).
limitCut :: (Logger logger)
  => logger
  -> WebBlockHeaderDb
  -> HashMap ChainId BlockHeader -- ^ latest cut headers
  -> HashMap ChainId SQLiteEnv
  -> BlockHeight
  -> IO (HashMap ChainId BlockHeader)
limitCut logger wbhdb latestCutHeaders pactConns bHeight = do
  fmap (HM.mapMaybe id) $ flip HM.traverseWithKey latestCutHeaders $ \cid latestCutHeader -> do
    let logger' = addChainIdLabel cid logger
    bdb <- getWebBlockHeaderDb wbhdb cid
    seekAncestor bdb latestCutHeader (fromIntegral bHeight) >>= \case
      -- Block exists on that chain
      Just h -> do
        -- Sanity check, should absolutely never happen
        when (view blockHeight h /= bHeight) $ do
          exitLog logger' "expected seekAncestor behaviour is broken"

        -- Confirm that PactDB is not behind RocksDB (it can be ahead though)
        let db = pactConns ^?! ix cid
        latestPactHeight <- getLatestBlockHeight db
        when (latestPactHeight < bHeight) $ do
          exitLog logger' "Pact State is behind RocksDB. This should never happen."

        pure (Just h)

      -- Block does not exist on that chain
      Nothing -> do
        logFunctionText logger' Debug $ "Block " <> sshow bHeight <> " is not accessible on this chain."
        pure Nothing

-- | Get the latest cut headers.
--   Note that the latest cut headers reflects the latest state of consensus.
--
--   Also returns a 'WebBlockHeaderDb' for convenience to callers.
getLatestCutHeaders :: ()
  => ChainwebVersion
  -> RocksDb
  -> IO (WebBlockHeaderDb, HashMap ChainId BlockHeader)
getLatestCutHeaders v rocksDb = do
  wbhdb <- initWebBlockHeaderDb rocksDb v
  let cutHashes = cutHashesTable rocksDb
  latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
  pure (wbhdb, latestCutHeaders)

-- | Take the latest cut headers, and find the minimum 'BlockHeight' across
--   the 'BlockHeader's of each chain, i.e, the highest 'BlockHeight' shared
--   amongst all chains. Then return the cut headers associated with that
--   latest common 'BlockHeight'.
resolveLatestCutHeaders :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> IO (BlockHeight, HashMap ChainId BlockHeader)
resolveLatestCutHeaders logger v pactConns rocksDb = do
  (wbhdb, latestCutHeaders) <- getLatestCutHeaders v rocksDb
  let latestCommonBlockHeight = minimum $ fmap (view blockHeight) latestCutHeaders
  headers <- limitCut logger wbhdb latestCutHeaders pactConns latestCommonBlockHeight
  pure (latestCommonBlockHeight, headers)

-- | Take the latest cut headers, and return the cut headers associated with
--   the specified 'BlockHeight'.
resolveCutHeadersAtHeight :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> BlockHeight
  -> IO (HashMap ChainId BlockHeader)
resolveCutHeadersAtHeight logger v pactConns rocksDb target = do
  (wbhdb, latestCutHeaders) <- getLatestCutHeaders v rocksDb
  limitCut logger wbhdb latestCutHeaders pactConns target

-- | Take the latest cut headers, and, for each specified 'BlockHeight',
--   return the cut headers associated with that 'BlockHeight'.
--
--   The list returned pairs the input 'BlockHeight's with the found headers.
resolveCutHeadersAtHeights :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> [BlockHeight] -- ^ targets
  -> IO [(BlockHeight, HashMap ChainId BlockHeader)]
resolveCutHeadersAtHeights logger v pactConns rocksDb targets = do
  (wbhdb, latestCutHeaders) <- getLatestCutHeaders v rocksDb
  forM targets $ \target -> do
    fmap (target, ) $ limitCut logger wbhdb latestCutHeaders pactConns target

-- | Compute the 'ChainGrandHash'es for each Chain. A 'Snapshot' just pairs
--   a 'BlockHeader' with the computed 'ChainGrandHash' at the header's
--   'BlockHeight'.
computeGrandHashesAt :: ()
  => HashMap ChainId SQLiteEnv
     -- ^ pact connections
  -> HashMap ChainId BlockHeader
     -- ^ Resolved targets, i.e, blockheights that are accessible per each
     --   chain.
  -> IO (HashMap ChainId Snapshot)
computeGrandHashesAt pactConns cutHeader = do
  fmap HM.fromList $ pooledForConcurrently (HM.toList cutHeader) $ \(cid, bHeader) -> do
    let db = pactConns ^?! ix cid
    (hash, ()) <- computeGrandHash (getLatestPactStateAt db (view blockHeight bHeader))
    pure (cid, Snapshot hash bHeader)

checkPactDbsExist :: FilePath -> [ChainId] -> IO ()
checkPactDbsExist dbDir cids = pooledForConcurrently_ cids $ \cid -> do
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

exitLog :: (Logger logger) => logger -> Text -> IO a
exitLog logger msg = do
  logFunctionText logger Error msg
  exitFailure

-- | Provide SQLite connections to all chains.
--
--   N.B. This is not at all exception-safe. This is not intended to be
--   used in production code paths.
--
--   This should either use ResourceT or some other mechanism for exception
--   safety.
withConnections :: (Logger logger)
  => logger
  -> FilePath
  -> [ChainId]
  -> (HashMap ChainId SQLiteEnv -> IO x)
  -> IO x
withConnections logger pactDir cids f = do
  checkPactDbsExist pactDir cids
  bracket openConnections closeConnections f
  where
    openConnections :: IO (HashMap ChainId SQLiteEnv)
    openConnections = fmap HM.fromList $ forM cids $ \cid -> do
      (cid, ) <$> startSqliteDb cid logger pactDir False

    closeConnections :: HashMap ChainId SQLiteEnv -> IO ()
    closeConnections = mapM_ stopSqliteDb

hex :: ByteString -> Text
hex = Text.decodeUtf8 . Base16.encode

cwvParser :: O.Parser ChainwebVersion
cwvParser = fmap (lookupVersionByName . fromMaybe (error "ChainwebVersion parse failed") . fromText)
  $ O.strOption
      (O.long "graph-version"
        <> O.short 'v'
        <> O.metavar "CHAINWEB_VERSION"
        <> O.help "Chainweb version for graph. Only needed for non-standard graphs."
        <> O.value (toText (_versionName mainnet))
        <> O.showDefault
      )

rocksParser :: O.Parser FilePath
rocksParser = O.strOption
  (O.long "rocksdb-dir"
   <> O.metavar "ROCKSDB DIR"
   <> O.help "Path to RocksDB directory"
  )
