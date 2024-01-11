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
  )
  where

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), PactRowContents(..), getLatestPactStateAt, withChainDb, getLatestBlockHeight, getEarliestBlockHeight, ensureBlockHeightExists)
import Chainweb.Pact.Backend.PactState.EmbeddedHashes (EncodedSnapshot(..), grands)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils (fromText, toText, sshow)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.WebBlockHeaderDB (initWebBlockHeaderDb, webBlockHeaderDb)
import Control.Applicative ((<|>), many, optional)
import Control.Lens ((^.))
import Control.Monad (forM, forM_, when)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Data.ByteArray qualified as Memory
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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
import UnliftIO.Async (pooledMapConcurrentlyN_)

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
        pure $ hashRows (Text.encodeUtf8 tblName) rows

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

-- | Get the BlockHeaders for each chain at the specified BlockHeight.
getBlockHeadersAt :: ()
  => RocksDb
     -- ^ RocksDB handle
  -> Map ChainId (Set BlockHeight)
     -- ^ Map from ChainId to available blockheights on that chain.
     --   This is most probably going to be the output of 'resolveTargets'.
  -> BlockHeight
     -- ^ The blockheight that you want to look up the header for.
  -> ChainwebVersion
     -- ^ chainweb version
  -> IO (Map ChainId BlockHeader)
getBlockHeadersAt rdb resolvedTargets bh v = do
  wbhdb <- initWebBlockHeaderDb rdb v
  let cutHashes = cutHashesTable rdb
  -- Get the latest cut
  latestCut <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
  fmap (Map.fromList . catMaybes) $ forM (HM.toList latestCut) $ \(cid, latestCutHeader) -> do
    if bh `Set.member` Map.findWithDefault Set.empty cid resolvedTargets
    then do
      if _blockHeight latestCutHeader == bh
      then do
        -- If we're already there, great
        pure $ Just (cid, latestCutHeader)
      else do
        -- Otherwise, we need to do an ancestral lookup
        case HM.lookup cid (wbhdb ^. webBlockHeaderDb) of
          Nothing -> error "getBlockHeadersAt: Malformed WebBlockHeaderDb"
          Just bdb -> do
            seekAncestor bdb latestCutHeader (fromIntegral bh) >>= \case
              Just h -> do
                -- Sanity check, should absolutely never happen
                when (_blockHeight h /= bh) $ do
                  error "getBlockHeadersAt: expected seekAncestor behaviour is broken"
                pure $ Just (cid, h)
              Nothing -> error "getBlockHeadersAt: no ancestor found!"
    else do
      pure Nothing

-- | Like 'TargetBlockHeight', but supports multiple targets in the non-'Latest'
--   case.
data BlockHeightTargets
  = LatestAll
  | TargetAll (Set BlockHeight)

--   Resolve the latest common blockheight across all chains.
--   Not all chains may be at the same tip, so we have to get
--   the highest block that they each have in their history,
--   and take the minimum of all of those. This operates under
--   the assumption that all chains are moving along at roughly
--   the same pace, and none are lagging behind considerably.
resolveLatest :: (Logger logger)
  => logger
  -> [ChainId]
  -> FilePath
  -> IO BlockHeight
resolveLatest logger cids pactDir = do
  -- Get the highest common blockheight across all chains.
  -- This assumes that all chains are moving along close enough
  -- to one another.
  maxCommonRef <- newIORef @BlockHeight (BlockHeight maxBound)
  pooledFor cids $ \cid -> do
    withChainDb cid logger pactDir $ \(SQLiteEnv db _) -> do
      top <- getLatestBlockHeight db
      atomicModifyIORef' maxCommonRef $ \x -> (min x top, ())
  readIORef maxCommonRef

-- | Resolve the requested targets across all chains.
--
--   Returns the accessible subset of those blockheights for each chain.
--
--   There are a couple of reasons that a blockheight might
--   not be accessible on a chain:
--     - the height is below the genesisHeight for that chain
--     - the height has been compacted away
resolveTargets :: (Logger logger)
  => logger
  -> [ChainId]
  -> FilePath
  -> Set BlockHeight
  -> IO (Map ChainId (Set BlockHeight))
resolveTargets logger cids pactDir targets = do
  targetsRef <- newIORef @(Map ChainId (Set BlockHeight)) $ Map.empty
  pooledFor cids $ \cid -> do
    withChainDb cid logger pactDir $ \(SQLiteEnv db _) -> do
      pooledFor targets $ \target -> do
        earliest <- getEarliestBlockHeight db
        if target < earliest
        then do
          logFunctionText logger Warn
            $ "BlockHeight " <> sshow target <> " doesn't exist on Chain " <> chainIdToText cid
        else do
          ensureBlockHeightExists db target
          atomicModifyIORef' targetsRef $ \m -> (Map.insertWith Set.union cid (Set.singleton target) m, ())
  readIORef targetsRef

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
  -> [ChainId]
  -> FilePath
  -> FilePath
  -> Map ChainId (Set BlockHeight)
     -- ^ Resolved targets, i.e, blockheights that are accessible per each
     --   chain.
  -> ChainwebVersion
  -> IO [(BlockHeight, Map ChainId (ByteString, BlockHeader))]
computeGrandHashesAt logger cids pactDir rocksDir chainTargets chainwebVersion = do

  -- We do this in two phases;
  --   1) We gather all of the grand hashes.
  --   2) We pair all of the grand hashes with the associated block headers.

  -- Phase 1:
  --   Gather all of the grand hashes

  -- This holds the Grand Hashes for each chain, at each BlockHeight of
  -- interest.
  chainHashesRef <- newIORef @(Map BlockHeight (Map ChainId ByteString)) Map.empty

  pooledFor cids $ \cid -> do
    withChainDb cid logger pactDir $ \(SQLiteEnv db _) -> do
      case Map.lookup cid chainTargets of
        Nothing -> do
          pure ()
        Just targets -> do
          -- Compute the grand hash for the chain at every target height
          pooledFor targets $ \target -> do
            hash <- computeGrandHash db target
            atomicModifyIORef' chainHashesRef $ \m ->
              (Map.insertWith Map.union target (Map.singleton cid hash) m, ())

  -- Note the toDescList. This is so that pact-import's top-down search
  -- is faster, and correct.
  chainHashes <- Map.toDescList <$> readIORef chainHashesRef

  -- Phase 2:
  -- Grab the headers corresponding to every blockheight from RocksDB
  withReadOnlyRocksDb rocksDir modernDefaultOptions $ \rocksDb -> do
    forM chainHashes $ \(height, hashes) -> do
      headers <- getBlockHeadersAt rocksDb chainTargets height chainwebVersion
      let missingIn cid m = error $ "missing entry for chain " <> Text.unpack (chainIdToText cid) <> " in " <> m
      pure $ (height,)
        $ Merge.merge
            (Merge.mapMissing (\cid _ -> missingIn cid "headers"))
            (Merge.mapMissing (\cid _ -> missingIn cid "hashes"))
            (Merge.zipWithMatched (\_ hash header -> (hash, header)))
            hashes
            headers

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
        undefined
      TargetAll ts -> do
        resolveTargets logger cids cfg.pactDir ts
    chainHashes <- computeGrandHashesAt logger cids cfg.pactDir cfg.rocksDir chainTargets cfg.chainwebVersion
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
    latest <- resolveLatest logger cids cfg.sourcePactDir

    let (recordedBlockHeight, expectedChainHashes) =
          -- Find the first element of 'grands' such that
          -- the blockheight therein is less than or equal to
          -- the argument latest common blockheight.
          case List.find (\g -> latest >= fst g) grands of
            Nothing -> error "pact-import: no snapshot found"
            Just (b, s) -> (b, Map.map (\es -> (es.pactHash, es.blockHeader)) s)

    chainHashes <- do
      let chainTargets = Map.fromList $ List.map (, Set.singleton recordedBlockHeight) cids
      computeGrandHashesAt logger cids cfg.sourcePactDir cfg.rocksDir chainTargets cfg.chainwebVersion >>= \case
        [(_, chainHashes)] -> do
          pure chainHashes
        [] -> do
          error "pact-import: computeGrandHashesAt unexpectedly returned 0 blocks"
        _ -> do
          error "pact-import: computeGrandHashesAt unexpectedly returned multiple blocks"

    let deltas = P.diff expectedChainHashes chainHashes

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
        forM_ (Map.keys chainHashes) $ \cid -> do
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
pooledFor stuff f = pooledMapConcurrentlyN_ 4 f stuff

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

