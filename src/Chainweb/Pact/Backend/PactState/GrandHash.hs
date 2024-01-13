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
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( pactImportMain
  , pactCalcMain

  , computeGrandHash

  , test
  )
  where

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText, unsafeChainId)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger (Logger, logFunctionText, addLabel)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), PactRowContents(..), getLatestPactStateAt, withChainDb, getLatestBlockHeight)
import Chainweb.Pact.Backend.PactState.EmbeddedHashes (EncodedSnapshot(..), grands)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Storage.Table.RocksDB (withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils (fromText, toText, sshow)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.WebBlockHeaderDB
import Control.Applicative ((<|>), many, optional)
import Control.Lens ((^?!), at, _Just)
import Control.Monad (forM, forM_, when)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Data.Bifunctor (second)
import Data.ByteArray qualified as Memory
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.Foldable qualified as F
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
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
import UnliftIO.Async (pooledForConcurrentlyN_)

-- | Compute the "Grand Hash" of a given chain.
computeGrandHash :: Database -> BlockHeight -> IO ByteString
computeGrandHash db bh = do
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
        pure $ hashRows (Text.encodeUtf8 (Text.toLower tblName)) rows

  -- This is a simple incremental hash over all of the table hashes.
  -- This is well-defined by its order because 'getLatestPactStateAt'
  -- guarantees that the stream of tables is ordered lexicographically by table
  -- name.
  S.fold_
    (\ctx hash -> hashUpdate ctx (hashWith SHA3_256 hash))
    (hashInitWith SHA3_256)
    (Memory.convert . hashFinalize)
    hashStream

-- | Incremental hash over a vector
hashAggregate :: (a -> ByteString) -> Vector a -> Maybe ByteString
hashAggregate f v
  | Vector.length v == 0 = Nothing
  | otherwise = Just
      $ Memory.convert
      $ hashFinalize
      $ Vector.foldl'
          (\ctx a -> hashUpdate ctx (hashWith SHA3_256 (f a)))
          (hashInitWith SHA3_256)
          v

-- | This is the grand hash of a table
--
--   Precondition: The PactRows must be in ordered by rowkey ASC
hashRows :: ByteString -> Vector PactRow -> Maybe ByteString
hashRows tblName = hashAggregate (rowToHashArgs tblName)

-- | Turn a (TableName, PactRow) into the arguments for a hash function.
--
--   The format is:
--
--   char 'T' (1 byte)
--   <length tablename> (8 bytes)
--   <tablename> (variable, maximum TBD)
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
rowToHashArgs :: ByteString -> PactRow -> ByteString
rowToHashArgs tblName pr =
  BL.toStrict
  $ BB.toLazyByteString
  $ BB.charUtf8 'T'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length tblName))
    <> BB.byteString tblName

    <> BB.charUtf8 'K'
    <> BB.word64LE (fromIntegral @Int @Word64 (BS.length pr.rowKey))
    <> BB.byteString pr.rowKey

    <> BB.charUtf8 'I'
    <> BB.word64LE (fromIntegral @Int64 @Word64 pr.txId)

    <> BB.charUtf8 'D'
    <> BB.byteString pr.rowData

test :: IO ()
test = do
  let rocksDir = "/home/chessai/rocksDb"
  let v = mainnet
  let cid = unsafeChainId 0

  C.withDefaultLogger Debug $ \logger -> do
    withReadOnlyRocksDb rocksDir modernDefaultOptions $ \rocksDb -> do
      wbhdb <- initWebBlockHeaderDb rocksDb v
      let cutHashes = cutHashesTable rocksDb
      -- Get the latest cut
      latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
      let bh = BlockHeight 3998134
      header <- getBlockHeaderAt logger wbhdb cid latestCutHeaders bh
      print $ _blockHash header

-- | Get the BlockHeader at a particular height.
getBlockHeaderAt :: (Logger logger)
  => logger
  -> WebBlockHeaderDb
  -> ChainId
  -> HashMap ChainId BlockHeader
     -- ^ Cut Headers
  -> BlockHeight
  -> IO BlockHeader
getBlockHeaderAt logger wbhdb cid latestCutHeaders bh = do
  bdb <- getWebBlockHeaderDb wbhdb cid
  -- Get the block in the latest cut
  let latestCutHeader = latestCutHeaders ^?! at cid . _Just
  debugLog logger $ "Latest Cut Header: " <> sshow latestCutHeader
  seekAncestor bdb latestCutHeader (fromIntegral bh) >>= \case
    Just h -> do
      -- Sanity check, should absolutely never happen
      when (_blockHeight h /= bh) $ do
        exitLog logger "getBlockHeadersAt: expected seekAncestor behaviour is broken"
      pure h
    Nothing -> do
      exitLog logger $
        "getBlockHeadersAt: no ancestor found for " <>
        "BlockHeight " <> sshow bh <> " on " <>
        "Chain " <> chainIdToText cid

limitCut :: (Logger logger)
  => logger
  -> WebBlockHeaderDb
  -> HashMap ChainId BlockHeader -- ^ latest cut headers
  -> FilePath -- ^ pact dir
  -> BlockHeight
  -> IO (HashMap ChainId BlockHeader)
limitCut logger wbhdb latestCutHeaders pactDir blockHeight = do
  flip HM.traverseWithKey latestCutHeaders $ \cid latestCutHeader -> do
    bdb <- getWebBlockHeaderDb wbhdb cid
    seekAncestor bdb latestCutHeader (fromIntegral blockHeight) >>= \case
      Just h -> do
        -- Sanity check, should absolutely never happen
        when (_blockHeight h /= blockHeight) $ do
          exitLog logger "expected seekAncestor behaviour is broken"

        -- Confirm that PactDB is not behind RocksDB (it can be ahead though)
        withChainDb cid logger pactDir $ \_ (SQLiteEnv db _) -> do
          latestPactHeight <- getLatestBlockHeight db
          when (latestPactHeight < blockHeight) $ do
            exitLog logger "Pact State is behind RocksDB. This should never happen."

        pure h
      Nothing -> do
        exitLog logger $
          "getBlockHeadersAt: no ancestor found for " <>
          "BlockHeight " <> sshow blockHeight <> " on " <>
          "Chain " <> chainIdToText cid

resolveLatest :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath
     -- ^ pact dir. we need to check that the pactdb is not behind rocksdb
  -> FilePath
     -- ^ rocksdb dir
  -> IO (BlockHeight, HashMap ChainId BlockHeader)
resolveLatest logger v pactDir rocksDir = do
  withReadOnlyRocksDb rocksDir modernDefaultOptions $ \rocksDb -> do
    wbhdb <- initWebBlockHeaderDb rocksDb v
    let cutHashes = cutHashesTable rocksDb
    latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
    let latestCommonBlockHeight = minimum $ fmap _blockHeight latestCutHeaders
    headers <- limitCut logger wbhdb latestCutHeaders pactDir latestCommonBlockHeight
    pure (latestCommonBlockHeight, headers)

resolveTargets' :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> FilePath -- ^ pact dir
  -> FilePath -- ^ rocksdb dir
  -> [BlockHeight] -- ^ targets
  -> IO [(BlockHeight, HashMap ChainId BlockHeader)]
resolveTargets' logger v pactDir rocksDir targets = do
  withReadOnlyRocksDb rocksDir modernDefaultOptions $ \rocksDb -> do
    wbhdb <- initWebBlockHeaderDb rocksDb v
    let cutHashes = cutHashesTable rocksDb
    latestCutHeaders <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
    forM targets $ \target -> do
      fmap (target, ) $ limitCut logger wbhdb latestCutHeaders pactDir target

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
  -> FilePath
     -- ^ pact dir
  -> [(a, HashMap ChainId BlockHeader)]
     -- ^ Resolved targets, i.e, blockheights that are accessible per each
     --   chain.
  -> IO [(a, HashMap ChainId (ByteString, BlockHeader))]
computeGrandHashesAt logger pactDir chainTargets = do
  forM chainTargets $ \(x, cutHeader) -> do
    fmap (x, ) $ flip HM.traverseWithKey cutHeader $ \cid blockHeader -> do
      withChainDb cid logger pactDir $ \_ (SQLiteEnv db _) -> do
        hash <- computeGrandHash db (_blockHeight blockHeader)
        pure (hash, blockHeader)

-- | Like 'TargetBlockHeight', but supports multiple targets in the non-'Latest'
--   case.
data BlockHeightTargets
  = LatestAll
  | TargetAll (Set BlockHeight)

data PactCalcConfig = PactCalcConfig
  { pactDir :: FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , targetBlockHeight :: BlockHeightTargets
  , writeModule :: Bool
  }

-- | Calculate the hash at every provided blockheight across all chains.
--
--   Note that for some chains, one or more of the requested blockheights
--   won't be accessible. This could be due to compaction, or the blockheight
--   predating the genesis of the chain. In this case, you will receive a
--   warning.
pactCalcMain :: IO ()
pactCalcMain = do
  cfg <- O.execParser opts
  let cids = allChains cfg.chainwebVersion
  checkPactDbsExist cfg.pactDir cids

  C.withDefaultLogger Info $ \logger -> do
    chainTargets <- case cfg.targetBlockHeight of
      LatestAll -> do
        List.singleton <$> resolveLatest logger cfg.chainwebVersion cfg.pactDir cfg.rocksDir
      TargetAll ts -> do
        resolveTargets' logger cfg.chainwebVersion cfg.pactDir cfg.rocksDir (Set.toDescList ts)
    chainHashes <- fmap (List.map (second hashMapToMap)) $ computeGrandHashesAt logger cfg.pactDir chainTargets
    when cfg.writeModule $ do
      writeFile "src/Chainweb/Pact/Backend/PactState/EmbeddedHashes.hs" (chainHashesToModule chainHashes)
    BLC8.putStrLn $ grandsToJson chainHashes
  where
    opts :: ParserInfo PactCalcConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compute the grand hash of a Pact database at a particular height."
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

pactImportMain :: IO ()
pactImportMain = do
  cfg <- O.execParser opts

  let cids = allChains cfg.chainwebVersion
  checkPactDbsExist cfg.sourcePactDir cids

  C.withDefaultLogger Info $ \logger -> do
    -- Get the highest common blockheight across all chains.
    latestBlockHeight <- fst <$> resolveLatest logger cfg.chainwebVersion cfg.sourcePactDir cfg.rocksDir

    let (recordedBlockHeight, expectedChainHashes) =
          -- Find the first element of 'grands' such that
          -- the blockheight therein is less than or equal to
          -- the argument latest common blockheight.
          case List.find (\g -> latestBlockHeight >= fst g) grands of
            Nothing -> error "pact-import: no snapshot found"
            Just (b, s) -> (b, Map.map (\es -> (es.pactHash, es.blockHeader)) s)

    chainHashes <- do
      chainTargets <- resolveTargets' logger cfg.chainwebVersion cfg.sourcePactDir cfg.rocksDir [recordedBlockHeight]
      computeGrandHashesAt logger cfg.sourcePactDir chainTargets >>= \case
        [(_, chainHashes)] -> do
          pure chainHashes
        [] -> do
          error "pact-import: computeGrandHashesAt unexpectedly returned 0 blocks"
        _ -> do
          error "pact-import: computeGrandHashesAt unexpectedly returned multiple blocks"

    let deltas = P.diff expectedChainHashes (hashMapToMap chainHashes)

    forM_ (Map.toAscList deltas) $ \(cid, delta) -> do
      case delta of
        P.Same _ -> pure ()
        P.Old _ -> error "pact-import: internal logic error: chain mismatch"
        P.New _ -> error "pact-import: internal logic error: chain mismatch"
        P.Delta (eHash, eHeader) (hash, header) -> do
          when (header /= eHeader) $ do
            putStrLn $ unlines
              [ "Chain " <> Text.unpack (chainIdToText cid)
              , "Block Header mismatch"
              , "  Expected: " <> show (_blockHash eHeader)
              , "  Actual:   " <> show (_blockHash header)
              ]

          when (hash /= eHash) $ do
            putStrLn $ unlines
              [ "Chain " <> Text.unpack (chainIdToText cid)
              , "Grand Hash mismatch"
              , "  Expected: " <> Text.unpack (hex eHash)
              , "  Actual:   " <> Text.unpack (hex hash)
              ]
    when (Map.size deltas > 0) exitFailure

    logFunctionText logger Info "Hashes aligned"

    -- TODO: drop things after the verified height?
    case cfg.targetPactDir of
      Nothing -> do
        pure ()
      Just targetDir -> do
        createDirectoryIfMissing False targetDir
        forM_ (HM.keys chainHashes) $ \cid -> do
          copyFile
            (chainwebDbFilePath cid cfg.sourcePactDir)
            (chainwebDbFilePath cid targetDir)
  where
    opts :: ParserInfo PactImportConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compare the grand hash of a Pact database to an expected value."
      , "If the hash matches, optionally import the database into a target directory."
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
checkPactDbsExist dbDir cids = pooledFor cids $ \cid -> do
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

pooledFor :: (Foldable t) => t a -> (a -> IO b) -> IO ()
pooledFor = pooledForConcurrentlyN_ 4

allChains :: ChainwebVersion -> [ChainId]
allChains v = List.sort $ F.toList $ chainIdsAt v (BlockHeight maxBound)

grandsToJson :: [(BlockHeight, Map ChainId (ByteString, BlockHeader))] -> BL.ByteString
grandsToJson chainHashes =
  J.encode $ J.Object $ flip List.map chainHashes $ \(height, hashes) ->
    let key = Text.pack $ show height
        val = J.Object $ flip List.map (Map.toAscList hashes) $ \(cid, (hash, header)) ->
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
chainHashesToModule :: [(BlockHeight, Map ChainId (ByteString, BlockHeader))] -> String
chainHashesToModule input = prefix
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

    makeEntries :: [(BlockHeight, Map ChainId (ByteString, BlockHeader))] -> [String]
    makeEntries =
      List.concatMap (List.map (indent 4))
      . onTail (onTail (indent 2) . onHead (prepend ", "))
      . List.map (uncurry makeEntry)

    makeEntry :: BlockHeight -> Map ChainId (ByteString, BlockHeader) -> [String]
    makeEntry height chainMap =
      [ "( BlockHeight " ++ show height
      , ", Map.fromList"
      , "    ["
      ]
      ++ onHead ("  " ++) (List.map (indent 4) $ onTail (prepend ", ") (makeChainMap chainMap))
      ++
      [ "    ]"
      , ")"
      ]

    makeChainMap :: Map ChainId (ByteString, BlockHeader) -> [String]
    makeChainMap = map (uncurry makeChainEntry) . Map.toList

    makeChainEntry :: ChainId -> (ByteString, BlockHeader) -> String
    makeChainEntry cid (hash, header) =
      let
        jsonDecode j = "unsafeJsonDecode @BlockHeader " ++ j
        fromHex b = "unsafeFromHex " ++ b
        sCid = Text.unpack (chainIdToText cid)
        sHash = inQuotes $ Text.unpack (hex hash)
        sHeader = embedQuotes $ Text.unpack (J.encodeText (J.encodeWithAeson header))
      in
      concat
        [ "(unsafeChainId " ++ sCid ++ ", "
        , "EncodedSnapshot (" ++ fromHex sHash ++ ") (" ++ jsonDecode sHeader ++ ")"
        , ")"
        ]

    prefix = List.unlines
      [ "-- NOTE: This module has been auto-generated."
      , "-- Do not edit it."
      , ""
      , "{-# LANGUAGE ImportQualifiedPost #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE TypeApplications #-}"
      , ""
      , "module Chainweb.Pact.Backend.PactState.EmbeddedHashes"
      , "  ( EncodedSnapshot(..)"
      , "  , grands"
      , "  )"
      , "  where"
      , ""
      , "import Chainweb.BlockHeader (BlockHeader)"
      , "import Chainweb.BlockHeight (BlockHeight(..))"
      , "import Chainweb.ChainId (ChainId, unsafeChainId)"
      , "import Data.Aeson qualified as A"
      , "import Data.ByteString (ByteString)"
      , "import Data.ByteString.Base16 qualified as Base16"
      , "import Data.List qualified as List"
      , "import Data.Map (Map)"
      , "import Data.Map qualified as Map"
      , "import Data.Ord (Down(..))"
      , "import Data.Text (Text)"
      , "import Data.Text.Encoding qualified as Text"
      , ""
      , "data EncodedSnapshot = EncodedSnapshot"
      , "  { pactHash :: ByteString"
      , "  , blockHeader :: BlockHeader"
      , "  }"
      , ""
      , "unsafeJsonDecode :: (A.FromJSON a) => Text -> a"
      , "unsafeJsonDecode t = case A.decodeStrict (Text.encodeUtf8 t) of"
      , "  Just a -> a"
      , "  Nothing -> error \"EmbeddedHashes: invalid json construction\""
      , ""
      , "unsafeFromHex :: Text -> ByteString"
      , "unsafeFromHex t = case Base16.decode (Text.encodeUtf8 t) of"
      , "  Right a -> a"
      , "  Left err -> error $ \"EmbeddedHashes: unsafeFromHex failed: \" ++ show err"
      , ""
      , "-- | sorted in descending order."
      , "grands :: [(BlockHeight, Map ChainId EncodedSnapshot)]"
      , "grands = List.sortOn (Down . fst)"
      , "  ["
      , unlines (makeEntries input)
      , "  ]"
      ]

infoLog :: (Logger logger) => logger -> Text -> IO ()
infoLog logger msg = do
  logFunctionText logger Info msg

debugLog :: (Logger logger) => logger -> Text -> IO ()
debugLog logger msg = do
  logFunctionText logger Debug msg

exitLog :: (Logger logger) => logger -> Text -> IO a
exitLog logger msg = do
  logFunctionText logger Error msg
  exitFailure

hashMapToMap :: (Hashable k, Ord k) => HashMap k a -> Map k a
hashMapToMap = Map.fromList . HM.toList
