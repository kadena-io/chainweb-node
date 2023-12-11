{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( pactImportMain
  , pactCalcMain
  )
  where

import Pact.JSON.Encode qualified as J
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger (addLabel)
import Chainweb.Pact.Backend.Compaction (TargetBlockHeight(..))
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), getLatestPactStateUpperBound', PactRowContents(..))
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (withSqliteDb)
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils (fromText, toText)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Chainweb.WebBlockHeaderDB (initWebBlockHeaderDb)
import Chainweb.WebBlockHeaderDB (webBlockHeaderDb)
import Control.Applicative ((<|>), many)
import Control.Applicative (optional)
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
import Data.Ord (Down(..))
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
import Pact.Types.SQLite (RType(..), SType(..))
import Pact.Types.SQLite qualified as Pact
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Logger.Types (LogLevel(..))
import UnliftIO.Async (pooledMapConcurrentlyN_)

-- | Compute the "Grand Hash" of a given chain.
computeGrandHash :: Database -> BlockHeight -> IO ByteString
computeGrandHash db bh = do
  -- We map over the state of the chain (tables) at the given blockheight.
  -- For each table, we sort the rows by rowKey, lexicographically.
  -- Then we feed the sorted rows into the incremental 'hashAggregate' function.
  let hashStream :: Stream (Of ByteString) IO ()
      hashStream = flip S.mapMaybeM (getLatestPactStateUpperBound' db bh) $ \(tblName, state) -> do
        let rows =
              Vector.fromList
              $ List.sortOn (\pr -> pr.rowKey)
              $ List.map (\(rowKey, PactRowContents{..}) -> PactRow{..})
              $ Map.toList state
        let hash = hashRows (Text.encodeUtf8 tblName) rows
        pure hash

  -- This is a simple incremental hash over all of the table hashes.
  -- This is well-defined by its order because 'getLatestPactStateUpperBound''
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

-- sorted in descending order.
-- this is currently bogus data.
grands :: [(BlockHeight, Map ChainId (ByteString, BlockHeader))]
grands = List.sortOn (Down . fst) -- insurance
  [ --(4000, Map.fromList [(unsafeChainId 4, ("4k", _))])
  --, (3000, Map.fromList [(unsafeChainId 4, ("3k", _))])
  --, (2000, Map.fromList [(unsafeChainId 4, ("2k", _))])
  ]

-- | Finds the first element of `grandHashes` such that
--   the blockheight therein is less than or equal to
--   the argument blockheight. We then look up the GrandHash
--   for the cid, and if we get a value, we return it. Otherwise
--   we keep looking.
--
findGrandHash :: BlockHeight -> ChainId -> Maybe (ByteString, BlockHeader)
findGrandHash bh0 cid = go grands
  where
    go = \case
      [] -> Nothing
      (bh, hashes) : rest ->
        if bh0 >= bh
        then case Map.lookup cid hashes of
          Just hash -> Just hash
          Nothing -> go rest
        else
          go rest

-- | Get the BlockHeaders for each chain at the specified BlockHeight.
getBlockHeadersAt :: RocksDb -> BlockHeight -> ChainwebVersion -> IO (Map ChainId BlockHeader)
getBlockHeadersAt rdb bh v = do
  wbhdb <- initWebBlockHeaderDb rdb v
  let cutHashes = cutHashesTable rdb
  -- Get the latest cut
  highestCuts <- readHighestCutHeaders v (\_ _ -> pure ()) wbhdb cutHashes
  fmap Map.fromList $ forM (HM.toList highestCuts) $ \(cid, header) -> do
    if _blockHeight header == bh
    then do
      -- If we're already there, great
      pure (cid, header)
    else do
      -- Otherwise, we need to do an ancestral lookup
      case HM.lookup cid (wbhdb ^. webBlockHeaderDb) of
        Nothing -> error "getBlockHeadersAt: Malformed WebBlockHeaderDb"
        Just bdb -> do
          seekAncestor bdb header (fromIntegral bh) >>= \case
            Just h -> do
              -- Sanity check, should absolutely never happen
              when (_blockHeight h /= bh) $ do
                error "getBlockHeadersAt: internal error"
              pure (cid, h)
            Nothing -> error "getBlockHeadersAt: no ancestor found!"

ensureBlockHeightExists :: Database -> BlockHeight -> IO ()
ensureBlockHeightExists db bh = do
  r <- Pact.qry db "SELECT blockheight FROM BlockHistory WHERE blockheight = ?1" [SInt (fromIntegral bh)] [RInt]
  case r of
    [[SInt rBH]] -> do
      when (fromIntegral bh /= rBH) $ do
        error "ensureBlockHeightExists: malformed query"
    _ -> do
      error $ "ensureBlockHeightExists: empty BlockHistory: height=" ++ show bh

getLatestBlockHeight :: Database -> IO BlockHeight
getLatestBlockHeight db = do
  r <- Pact.qry db "SELECT blockheight FROM BlockHistory ORDER BY blockheight DESC LIMIT 1" [] [RInt]
  case r of
    [[SInt bh]] -> do
      pure (fromIntegral bh)
    _ -> do
      error "getLatestBlockHeight: no latest blockheight"

data BlockHeightTargets
  = LatestAll
  | TargetAll (Set BlockHeight)

-- | Compute the GrandHashes at the specified targets.
--
--   The targets can either be the latest BlockHeight only,
--   or a set of specified targets (for computing many grand hashes per chain at
--   once).
--
--   The list this returns is in descending order by the BlockHeight.
--   This is to facilitate easier searching in the style of 'findGrandHashes'.
computeGrandHashesAt :: ()
  => FilePath
  -> FilePath
  -> BlockHeightTargets
  -> ChainwebVersion
  -> IO [(BlockHeight, Map ChainId (ByteString, BlockHeader))]
computeGrandHashesAt pactDir rocksDir targets chainwebVersion = do
  let cids = List.sort $ F.toList $ chainIdsAt chainwebVersion (BlockHeight maxBound)
  let pooledFor stuff f = pooledMapConcurrentlyN_ 4 f stuff

  -- We do this in two phases;
  --   - In the first phase, we check that the db matches
  --     our expectations (all files exist, blockheights we intend to use are
  --     available, etc)
  --   - In the second phase, we actually gather all of the hashes.
  C.withDefaultLogger Info $ \logger' -> do
    let resetDb = False

    -- Highest blockheight in common between all chains.
    -- Only used in the 'Latest' case.
    minBHRef <- newIORef @BlockHeight (BlockHeight maxBound)

    pooledFor cids $ \cid -> do
      -- We check db existence as a precondition, because any missing chainIds are
      -- unacceptable, so we can abort.
      e <- doesPactDbExist cid pactDir
      when (not e) $ do
        error $ "Pact database doesn't exist for expected chain id " <> Text.unpack (chainIdToText cid)

      -- We also check the blockheight validity of each chain
      let logger = addLabel ("chainId", chainIdToText cid) logger'
      withSqliteDb cid logger pactDir resetDb $ \(SQLiteEnv db _) -> do
        case targets of
          -- if we have an explicit target(s), we just need to make sure that
          -- every chain contains each target.
          TargetAll s -> do
            pooledFor s $ \target -> do
              ensureBlockHeightExists db target
          -- we need to get the minimum blockheight that every chain has in
          -- common. this operates under the assumption that each chain has
          -- the same "floor", so to speak.
          LatestAll -> do
            top <- getLatestBlockHeight db
            atomicModifyIORef' minBHRef $ \x -> (min x top, ())
            pure ()

        pure ()

    -- This holds the Grand Hashes for each chain, at each BlockHeight of
    -- interest.
    chainHashesRef <- newIORef @(Map BlockHeight (Map ChainId ByteString)) Map.empty
    case targets of
      LatestAll -> do
        bh <- readIORef minBHRef
        -- It's simpler to instantiate the outer key ahead of time
        writeIORef chainHashesRef (Map.singleton bh Map.empty)
        pooledFor cids $ \cid -> do
          let logger = addLabel ("chainId", chainIdToText cid) logger'
          withSqliteDb cid logger pactDir resetDb $ \(SQLiteEnv db _) -> do
            hash <- computeGrandHash db bh
            atomicModifyIORef' chainHashesRef $ \m ->
              (Map.insertWith Map.union bh (Map.singleton cid hash) m, ())
      TargetAll s -> do
        -- It's simpler to instantiate the outer keys ahead of time
        forM_ s $ \target -> do
          modifyIORef' chainHashesRef $ \m -> Map.insert target Map.empty m
        pooledFor cids $ \cid -> do
          let logger = addLabel ("chainId", chainIdToText cid) logger'
          withSqliteDb cid logger pactDir resetDb $ \(SQLiteEnv db _) -> do
            pooledFor s $ \target -> do
              hash <- computeGrandHash db target
              atomicModifyIORef' chainHashesRef $ \m ->
                (Map.insertWith Map.union target (Map.singleton cid hash) m, ())

    -- Note the toDescList
    chainHashes <- Map.toDescList <$> readIORef chainHashesRef

    -- Now that we have all of the hashes, we need to grab the corresponding
    -- headers from RocksDB
    withReadOnlyRocksDb rocksDir modernDefaultOptions $ \rocksDb -> do
      forM chainHashes $ \(height, hashes) -> do
        headers <- getBlockHeadersAt rocksDb height chainwebVersion
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
  }

pactCalcMain :: IO ()
pactCalcMain = do
  cfg <- O.execParser opts
  chainHashes <- computeGrandHashesAt cfg.pactDir cfg.rocksDir cfg.targetBlockHeight cfg.chainwebVersion
  BLC8.putStrLn $ J.encode $ J.Object $ flip List.map chainHashes $ \(height, hashes) ->
    let key = Text.pack $ show height
        val = J.Object $ flip List.map (Map.toDescList hashes) $ \(cid, (hash, header)) ->
                let o = J.Object
                      [ "hash" J..= hex hash
                      , "header" J..= J.encodeWithAeson header
                      ]
                in (chainIdToText cid, o)
    in (key J..= val)
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

data PactImportConfig = PactImportConfig
  { sourcePactDir :: FilePath
  , targetPactDir :: Maybe FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , targetBlockHeight :: TargetBlockHeight
  }

pactImportMain :: IO ()
pactImportMain = do
  cfg <- O.execParser opts

  let targets = case cfg.targetBlockHeight of
        Latest -> LatestAll
        Target bh -> TargetAll (Set.singleton bh)
  (bh, chainHashes) <- do
    computeGrandHashesAt cfg.sourcePactDir cfg.rocksDir targets cfg.chainwebVersion >>= \case
      [(bh, chainHashes)] -> do
        pure (bh, chainHashes)
      _ -> do
        error "pact-import: computeGrandHashesAt unexpectedly returned multiple blocks"

  flip Map.foldMapWithKey chainHashes $ \cid (grandHash, header) -> do
    case findGrandHash bh cid of
      Nothing -> do
        error "pact-import: findGrandHash failed. This should be impossible."
      Just (expectedGrandHash, expectedBlockHeader) -> do
        when (header /= expectedBlockHeader) $ do
          error $ unlines
            [ "Block Header mismatch"
            , "  Expected: " <> show (_blockHash expectedBlockHeader)
            , "  Actual:   " <> show (_blockHash header)
            ]

        when (expectedGrandHash /= grandHash) $ do
          error $ unlines
            [ "Grand Hash mismatch"
            , "  Expected: " <> Text.unpack (hex expectedGrandHash)
            , "  Actual:   " <> Text.unpack (hex grandHash)
            ]

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
      <*> targetParser

versionFromText :: (HasCallStack) => Text -> ChainwebVersion
versionFromText t = case fromText @ChainwebVersionName t of
  Just a -> lookupVersionByName a
  Nothing -> error $ "Invalid chainweb version name: " ++ Text.unpack t

doesPactDbExist :: ChainId -> FilePath -> IO Bool
doesPactDbExist cid dbDir = do
  doesFileExist (chainwebDbFilePath cid dbDir)

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

targetParser :: Parser TargetBlockHeight
targetParser =
  let
    p = fmap (Target . BlockHeight) $ O.option O.auto
          (O.long "target-blockheight"
            <> O.short 'b'
            <> O.metavar "BLOCKHEIGHT"
            <> O.help "BlockHeight to verify."
          )
  in
  p <|> pure Latest

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
