{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( pactImportMain
  , pactCalcMain

  , computeGrandHash
  , pactCalc
  , pactVerify
  , pactDropPostVerified
  , BlockHeightTargets(..)
  )
  where

import Chainweb.Pact.Backend.RelationalCheckpointer (withProdRelationalCheckpointer)
import Chainweb.BlockHeader (BlockHeader(..), genesisHeight)
import Chainweb.Pact.Types (defaultModuleCacheLimit)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), PactRowContents(..), getLatestPactStateAt, getLatestBlockHeight, addChainIdLabel)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.Types (Checkpointer(..), SQLiteEnv(..), initBlockState)
import Chainweb.Pact.Backend.Utils (startSqliteDb, stopSqliteDb)
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils (fromText, toText, sshow)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName(..))
import Chainweb.Version.Development (devnet)
import Chainweb.Version.FastDevelopment (fastDevnet)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Testnet (testnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.WebBlockHeaderDB
import Control.Applicative ((<|>), many, optional)
import Control.Exception (bracket)
import Control.Lens ((^?!), ix)
import Control.Monad (forM, forM_, when, void)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Data.ByteArray qualified as Memory
import Data.Char qualified as Char
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.SQLite3.Direct (Database)
import GHC.Stack (HasCallStack)
import Options.Applicative (ParserInfo, Parser, (<**>))
import Options.Applicative qualified as O
import Pact.JSON.Encode qualified as J
import Patience.Map qualified as P
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.LogLevel (LogLevel(..))
import UnliftIO.Async (pooledForConcurrentlyN, pooledForConcurrentlyN_)

-- | Compute the "Grand Hash" of a given chain.
computeGrandHash :: (Logger logger) => logger -> Database -> BlockHeight -> IO ByteString
computeGrandHash logger db bh = do
  -- We map over the state of the chain (tables) at the given blockheight.
  -- For each table, we sort the rows by rowKey, lexicographically.
  -- Then we feed the sorted rows into the incremental 'hashAggregate' function.
  let hashStream :: Stream (Of ByteString) IO ()
      hashStream = flip S.mapMaybeM (getLatestPactStateAt db bh) $ \(tblName, state) -> do
        let rows =
              Vector.fromList
              $ List.sortOn (\pr -> pr.rowKey)
              $ List.map (\(rowKey, PactRowContents{..}) -> PactRow{..})
              $ Map.toList state
        let tableHash = hashTable tblName rows
        forM_ tableHash $ \h -> do
          logFunctionText logger Debug $ "Table Hash of " <> tblName <> " is " <> hex h
        pure tableHash

  -- This is a simple incremental hash over all of the table hashes.
  -- This is well-defined by its order because 'getLatestPactStateAt'
  -- guarantees that the stream of tables is ordered lexicographically by table
  -- name.
  grandHash <- S.fold_
    (\ctx hash -> hashUpdate ctx (hashWith SHA3_256 hash))
    (hashInitWith SHA3_256)
    (Memory.convert . hashFinalize)
    hashStream
  logFunctionText logger Debug $ "Grand Hash is " <> hex grandHash
  pure grandHash

-- | This is the grand hash of a table
--
--   Precondition: The PactRows must be in ordered by rowkey ASC
hashTable :: Text -> Vector PactRow -> Maybe ByteString
hashTable tblName rows
  | Vector.length rows == 0 = Nothing
  | otherwise = Just
      $ Memory.convert
      $ hashFinalize
      $ Vector.foldl'
          (\ctx row -> hashUpdate ctx (hashWith alg (rowToHashInput row)))
          (hashUpdate (hashInitWith alg) (hashWith alg tableInput))
          rows
  where
    alg = SHA3_256

    tableInput =
      let
        tbl = Text.encodeUtf8 (Text.toLower tblName)
      in
      BL.toStrict
      $ BB.toLazyByteString
      $ BB.charUtf8 'T'
        <> BB.word64LE (fromIntegral @Int @Word64 (BS.length tbl))
        <> BB.byteString tbl

-- | Turn a (TableName, PactRow) into the arguments for a hash function.
--
--   The format is:
--
--   char 'K' (1 byte)
--   <length rowkey> (8 bytes)
--   <rowkey> (variable, maximum size TBD)
--
--   char 'I' (1 byte)
--   <txId> (8 bytes)
--
--   char 'D' (1 byte)
--   <rowdata> (variable, maximum size TBD)
--
--   And thus the size of this will always be 28 bytes + the total size of
--   all variable sized inputs. None of the variable-sized inputs can
--   be empty (other than maybe rowdata?).
rowToHashInput :: PactRow -> ByteString
rowToHashInput pr =
  BL.toStrict
  $ BB.toLazyByteString
  $
       BB.charUtf8 'K'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length pr.rowKey))
    <> BB.byteString pr.rowKey

    <> BB.charUtf8 'I'
    <> BB.word64LE (fromIntegral @Int64 @Word64 pr.txId)

    <> BB.charUtf8 'D'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length pr.rowData))
    <> BB.byteString pr.rowData

limitCut :: (Logger logger)
  => logger
  -> WebBlockHeaderDb
  -> HashMap ChainId BlockHeader -- ^ latest cut headers
  -> HashMap ChainId SQLiteEnv
  -> BlockHeight
  -> IO (HashMap ChainId BlockHeader)
limitCut logger wbhdb latestCutHeaders pactConns blockHeight = do
  fmap (HM.mapMaybe id) $ flip HM.traverseWithKey latestCutHeaders $ \cid latestCutHeader -> do
    bdb <- getWebBlockHeaderDb wbhdb cid
    seekAncestor bdb latestCutHeader (fromIntegral blockHeight) >>= \case
      -- Block exists on that chain
      Just h -> do
        -- Sanity check, should absolutely never happen
        when (_blockHeight h /= blockHeight) $ do
          exitLog logger "expected seekAncestor behaviour is broken"

        -- Confirm that PactDB is not behind RocksDB (it can be ahead though)
        let SQLiteEnv db _ = pactConns ^?! ix cid
        latestPactHeight <- getLatestBlockHeight db
        when (latestPactHeight < blockHeight) $ do
          exitLog logger "Pact State is behind RocksDB. This should never happen."

        pure (Just h)

      -- Block does not exist on that chain
      Nothing -> do
        pure Nothing

resolveLatestCutHeaders :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> IO (BlockHeight, HashMap ChainId BlockHeader)
resolveLatestCutHeaders logger v pactConns rocksDb = do
  wbhdb <- initWebBlockHeaderDb rocksDb v
  let cutHashes = cutHashesTable rocksDb
  latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
  let latestCommonBlockHeight = minimum $ fmap _blockHeight latestCutHeaders
  headers <- limitCut logger wbhdb latestCutHeaders pactConns latestCommonBlockHeight
  pure (latestCommonBlockHeight, headers)

resolveCutHeadersAtHeights :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> [BlockHeight] -- ^ targets
  -> IO [(BlockHeight, HashMap ChainId BlockHeader)]
resolveCutHeadersAtHeights logger v pactConns rocksDb targets = do
  wbhdb <- initWebBlockHeaderDb rocksDb v
  let cutHashes = cutHashesTable rocksDb
  latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
  forM targets $ \target -> do
    fmap (target, ) $ limitCut logger wbhdb latestCutHeaders pactConns target

-- | Compute the GrandHashes at the specified targets.
--
--   The targets can either be the latest BlockHeight only,
--   or a set of specified targets (for computing many grand hashes per chain at
--   once).
--
--   The list this returns is in descending order by the BlockHeight.
--   This is to facilitate easier searching in the style of 'findGrandHash'.
computeGrandHashesAt :: (Logger logger)
  => logger
  -> HashMap ChainId SQLiteEnv
     -- ^ pact connections
  -> [(a, HashMap ChainId BlockHeader)]
     -- ^ Resolved targets, i.e, blockheights that are accessible per each
     --   chain.
     --
     --   The 'a' is polymorphic to show that it is unused by this function,
     --   but we keep them paired up.
  -> IO [(a, HashMap ChainId (ByteString, BlockHeader))]
computeGrandHashesAt logger pactConns chainTargets = do
  pooledFor chainTargets $ \(x, cutHeader) -> do
    fmap ((x, ) . HM.fromList) $ pooledFor (HM.toList cutHeader) $ \(cid, blockHeader) -> do
      let SQLiteEnv db _ = pactConns ^?! ix cid
      hash <- computeGrandHash (addChainIdLabel cid logger) db (_blockHeight blockHeader)
      pure (cid, (hash, blockHeader))

-- | Like 'TargetBlockHeight', but supports multiple targets in the non-'Latest'
--   case.
data BlockHeightTargets
  = LatestAll
  | TargetAll (Set BlockHeight)

data PactCalcConfig = PactCalcConfig
  { pactDir :: FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , target :: BlockHeightTargets
  , writeModule :: Bool
  }

pactCalc :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
     -- ^ pact database dir
  -> RocksDb
     -- ^ rocksdb dir
  -> BlockHeightTargets
     -- ^ target for calculation
  -> IO [(BlockHeight, HashMap ChainId (ByteString, BlockHeader))]
pactCalc logger v pactConns rocksDb target = do
  chainTargets <- case target of
    LatestAll -> do
      List.singleton <$> resolveLatestCutHeaders logger v pactConns rocksDb
    TargetAll ts -> do
      resolveCutHeadersAtHeights logger v pactConns rocksDb (Set.toDescList ts)
  computeGrandHashesAt logger pactConns chainTargets

-- | Calculate the hash at every provided blockheight across all chains.
--
--   Note that for some chains, one or more of the requested blockheights
--   won't be accessible. This could be due to compaction, or the blockheight
--   predating the genesis of the chain. In this case, a warning will be
--   emitted, not an error.
pactCalcMain :: IO ()
pactCalcMain = do
  cfg <- O.execParser opts
  C.withDefaultLogger Debug $ \logger -> do
    withConnections logger cfg.pactDir (allChains cfg.chainwebVersion) $ \pactConns -> do
      withReadOnlyRocksDb cfg.rocksDir modernDefaultOptions $ \rocksDb -> do
        chainHashes <- pactCalc logger cfg.chainwebVersion pactConns rocksDb cfg.target
        when cfg.writeModule $ do
          let modulePath = "src/Chainweb/Pact/Backend/PactState/EmbeddedSnapshot/" <> versionModuleName cfg.chainwebVersion <> ".hs"
          writeFile modulePath (chainHashesToModule cfg.chainwebVersion chainHashes)
        BLC8.putStrLn $ grandsToJson chainHashes
  where
    opts :: ParserInfo PactCalcConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compute the grand hash of a Pact database at a particular height(s)."
      , "If no height is specified, defaults to the latest state of consensus."
      ]

    parser :: Parser PactCalcConfig
    parser = PactCalcConfig
      <$> O.strOption
            (O.long "pact-database-dir"
             <> O.short 'd'
             <> O.metavar "PACT DBDIR"
             <> O.help "Source Pact database, which you wish to verify"
            )
      <*> rocksParser
      <*> cwvParser
      <*> targetsParser
      <*> O.switch (O.long "write-module" <> O.help "Write the hashes and headers out as a Haskell module to be used in chainweb-node")

data PactImportConfig = PactImportConfig
  { sourcePactDir :: FilePath
  , targetPactDir :: Maybe FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  }

pactVerify :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
  -> RocksDb
  -> [(BlockHeight, HashMap ChainId Snapshot)]
  -> IO (BlockHeight, HashMap ChainId (ByteString, BlockHeader))
pactVerify logger v pactConns rocksDb grands = do
  -- Get the highest common blockheight across all chains.
  latestBlockHeight <- fst <$> resolveLatestCutHeaders logger v pactConns rocksDb

  snapshot@(snapshotBlockHeight, expectedChainHashes) <- do
    -- Find the first element of 'grands' such that
    -- the blockheight therein is less than or equal to
    -- the argument latest common blockheight.
    case List.find (\g -> latestBlockHeight >= fst g) grands of
      Nothing -> do
        exitLog logger "No snapshot older than latest block"
      Just (b, s) -> do
        pure (b, HM.map (\snapshot -> (snapshot.pactHash, snapshot.blockHeader)) s)

  chainHashes <- do
    chainTargets <- resolveCutHeadersAtHeights logger v pactConns rocksDb [snapshotBlockHeight]
    computeGrandHashesAt logger pactConns chainTargets >>= \case
      [(_, chainHashes)] -> do
        pure chainHashes
      [] -> do
        exitLog logger "computeGrandHashesAt unexpectedly returned 0 blocks"
      _ -> do
        exitLog logger "computeGrandHashesAt unexpectedly returned multiple blocks"

  let deltas = P.diff (hashMapToMap expectedChainHashes) (hashMapToMap chainHashes)

  forM_ (Map.toAscList deltas) $ \(cid, delta) -> do
    let logger' = addChainIdLabel cid logger
    case delta of
      P.Same _ -> pure ()
      P.Old _ -> exitLog logger' "pact-import: internal logic error: chain mismatch"
      P.New _ -> exitLog logger' "pact-import: internal logic error: chain mismatch"
      P.Delta (eHash, eHeader) (hash, header) -> do
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
            , "  Expected: " <> hex eHash
            , "  Actual:   " <> hex hash
            ]
  when (Map.size deltas > 0) exitFailure

  logFunctionText logger Info "Hashes aligned"
  pure snapshot

pactDropPostVerified :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath
     -- ^ source pact dir
  -> FilePath
     -- ^ target pact dir
  -> HashMap ChainId SQLiteEnv
     -- ^ Pact SQLite Connections
  -> BlockHeight
     -- ^ highest verified blockheight
  -> HashMap ChainId (ByteString, BlockHeader)
     -- ^ Grand Hashes & BlockHeaders at this blockheight
  -> IO ()
pactDropPostVerified logger v srcDir tgtDir pactConns snapshotBlockHeight snapshotChainHashes = do
  logFunctionText logger Info $ "Creating " <> Text.pack tgtDir
  createDirectoryIfMissing False tgtDir

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

  forM_ chains $ \cid -> do
    let sqliteEnv = pactConns ^?! ix cid
    let logger' = addChainIdLabel cid logger
    logFunctionText logger' Info
      $ "Dropping anything post verified state (BlockHeight " <> sshow snapshotBlockHeight <> ")"
    withProdRelationalCheckpointer logger (initBlockState defaultModuleCacheLimit (genesisHeight v cid)) sqliteEnv v cid $ \cp -> do
      let blockHash = _blockHash $ snd $ snapshotChainHashes ^?! ix cid
      void $ _cpRestore cp (Just (snapshotBlockHeight + 1, blockHash))
      _cpSave cp blockHash

pactImportMain :: IO ()
pactImportMain = do
  cfg <- O.execParser opts

  C.withDefaultLogger Info $ \logger -> do
    withConnections logger cfg.sourcePactDir (allChains cfg.chainwebVersion) $ \pactConns -> do
      withReadOnlyRocksDb cfg.rocksDir modernDefaultOptions $ \rocksDb -> do
        (snapshotBlockHeight, snapshotChainHashes) <- pactVerify logger cfg.chainwebVersion pactConns rocksDb (error "replace me")

        forM_ cfg.targetPactDir $ \targetDir -> do
          pactDropPostVerified logger cfg.chainwebVersion cfg.sourcePactDir targetDir pactConns snapshotBlockHeight snapshotChainHashes
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

cwvParser :: Parser ChainwebVersion
cwvParser = fmap versionFromText $ O.strOption
  (O.long "graph-version"
    <> O.short 'v'
    <> O.metavar "CHAINWEB_VERSION"
    <> O.help "Chainweb version for graph. Only needed for non-standard graphs."
    <> O.value (toText (_versionName mainnet))
    <> O.showDefault
  )

rocksParser :: Parser FilePath
rocksParser = O.strOption
  (O.long "rocksdb-dir"
   <> O.metavar "ROCKSDB DIR"
   <> O.help "Path to RocksDB directory"
  )

targetsParser :: Parser BlockHeightTargets
targetsParser =
  let
    makeTargeted = \case
      [] -> LatestAll
      ts -> TargetAll (Set.fromList ts)
    p = fmap makeTargeted $ many $ fmap BlockHeight $ O.option O.auto
          (O.long "target-blockheight"
            <> O.short 'b'
            <> O.metavar "BLOCKHEIGHT"
            <> O.help "BlockHeight to verify."
          )
  in
  p <|> pure LatestAll

hex :: ByteString -> Text
hex = Text.decodeUtf8 . Base16.encode

pooledFor :: (Traversable t) => t a -> (a -> IO b) -> IO (t b)
pooledFor = pooledForConcurrentlyN 4

pooledFor_ :: (Foldable t) => t a -> (a -> IO b) -> IO ()
pooledFor_ = pooledForConcurrentlyN_ 4

allChains :: ChainwebVersion -> [ChainId]
allChains v = List.sort $ F.toList $ chainIdsAt v (BlockHeight maxBound)

grandsToJson :: [(BlockHeight, HashMap ChainId (ByteString, BlockHeader))] -> BL.ByteString
grandsToJson chainHashes =
  J.encode $ J.Object $ flip List.map chainHashes $ \(height, hashes) ->
    let sortedHashes = List.sortOn fst $ HM.toList hashes
        key = Text.pack $ show height
        val = J.Object $ flip List.map sortedHashes $ \(cid, (hash, header)) ->
                let o = J.Object
                      [ "hash" J..= hex hash
                      , "header" J..= J.encodeWithAeson header
                      ]
                in (chainIdToText cid, o)
    in (key J..= val)

-- | Output a Haskell module with the embedded hashes. This module produced
--   pact-calc, and embedded into the chainweb-node library.
--
--   The implementation is a little janky, but it works.
chainHashesToModule :: ChainwebVersion -> [(BlockHeight, HashMap ChainId (ByteString, BlockHeader))] -> String
chainHashesToModule chainwebVersion input = prefix
  where
    indent :: Int -> String -> String
    indent n s = List.replicate n ' ' ++ s

    onHead :: (a -> a) -> [a] -> [a]
    onHead f = \case { [] -> []; x : xs -> f x : xs; }

    onTail :: (a -> a) -> [a] -> [a]
    onTail f = \case { [] -> []; x : xs -> x : List.map f xs; }

    inQuotes :: String -> String
    inQuotes s = "\"" ++ s ++ "\""

    embedQuotes :: String -> String
    embedQuotes = \case
      [] -> []
      c : cs -> "\"\\" ++ [c] ++ go cs
      where
        go = \case
          [] -> error "mis-use of embedQuotes"
          [_] -> error "mis-use of embedQuotes"
          xs -> List.init xs ++ "\\\"" ++ [List.last xs]

    prepend :: String -> (String -> String)
    prepend p = \s -> p ++ s

    formatUnderscores :: (Integral a, Show a) => a -> String
    formatUnderscores n = reverse $ List.intercalate "_" $ chunksOf 3 $ reverse $ show n
      where
        chunksOf k =
          let
            go = \case
              [] -> []
              xs -> take k xs : go (drop k xs)
          in
          go

    makeEntries :: [(BlockHeight, HashMap ChainId (ByteString, BlockHeader))] -> [String]
    makeEntries =
      List.concatMap (List.map (indent 4))
      . onTail (onTail (indent 2) . onHead (prepend ", "))
      . List.map (uncurry makeEntry)

    makeEntry :: BlockHeight -> HashMap ChainId (ByteString, BlockHeader) -> [String]
    makeEntry height chainMap =
      [ "( BlockHeight " ++ formatUnderscores height
      , ", HM.fromList"
      , "    ["
      ]
      ++ onHead ("  " ++) (List.map (indent 4) $ onTail (prepend ", ") (makeChainMap chainMap))
      ++
      [ "    ]"
      , ")"
      ]

    makeChainMap :: HashMap ChainId (ByteString, BlockHeader) -> [String]
    makeChainMap = map (uncurry makeChainEntry) . List.sortOn fst . HM.toList

    makeChainEntry :: ChainId -> (ByteString, BlockHeader) -> String
    makeChainEntry cid (hash, header) =
      let
        jsonDecode j = "unsafeDecodeBlockHeader " ++ j
        fromHex b = "unsafeBase16Decode " ++ b
        sCid = Text.unpack (chainIdToText cid)
        sHash = inQuotes $ Text.unpack (hex hash)
        sHeader = embedQuotes $ Text.unpack (J.encodeText (J.encodeWithAeson header))
      in
      concat
        [ "(unsafeChainId " ++ sCid ++ ", "
        , "Snapshot (" ++ fromHex sHash ++ ") (" ++ jsonDecode sHeader ++ ")"
        , ")"
        ]

    prefix = List.unlines
      [ "-- NOTE: This module has been auto-generated."
      , "-- Do not edit it."
      , ""
      , "{-# LANGUAGE ImportQualifiedPost #-}"
      , "{-# LANGUAGE NumericUnderscores #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , ""
      , "module Chainweb.Pact.Backend.PactState.EmbeddedSnapshot." <> versionModuleName chainwebVersion
      , "  ( grands"
      , "  )"
      , "  where"
      , ""
      , "import Chainweb.BlockHeight (BlockHeight(..))"
      , "import Chainweb.ChainId (ChainId, unsafeChainId)"
      , "import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..), unsafeDecodeBlockHeader, unsafeBase16Decode)"
      , "import Data.HashMap.Strict (HashMap)"
      , "import Data.HashMap.Strict qualified as HM"
      , "import Data.List qualified as List"
      , "import Data.Ord (Down(..))"
      , ""
      , "-- | sorted in descending order."
      , "grands :: [(BlockHeight, HashMap ChainId Snapshot)]"
      , "grands = List.sortOn (Down . fst)"
      , "  ["
      , unlines (makeEntries input)
      , "  ]"
      ]

exitLog :: (Logger logger) => logger -> Text -> IO a
exitLog logger msg = do
  logFunctionText logger Error msg
  exitFailure

hashMapToMap :: (Hashable k, Ord k) => HashMap k a -> Map k a
hashMapToMap = Map.fromList . HM.toList

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

versionModuleName :: ChainwebVersion -> String
versionModuleName v
  | v == mainnet = "Mainnet"
  | v == testnet = "Testnet"
  | v == devnet = "Devnet"
  | v == fastDevnet = "FastDevnet"
  | otherwise = case Text.unpack (getChainwebVersionName (_versionName v)) of
      [] -> []
      c : cs -> Char.toUpper c : cs

