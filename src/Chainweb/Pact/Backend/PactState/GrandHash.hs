{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# options_ghc -fno-warn-unused-imports #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( test
  , main
  )
  where

import Data.Maybe (fromMaybe)

import Data.Word (Word8)
import Data.Int (Int64)
import Chainweb.WebBlockHeaderDB (initWebBlockHeaderDb)
import Chainweb.Storage.Table.RocksDB (RocksDb)
import Data.Ord (Down(..))
import Control.Applicative (optional)
import Control.Monad (forM, forM_, when)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Chainweb.BlockHeader (BlockHeader)
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText, unsafeChainId)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), getLatestPactStateUpperBound', PactRowContents(..))
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, toUtf8, withSqliteDb)
import Chainweb.Utils (fromText, toText)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BSS
import Data.ByteArray qualified as Memory
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.Char (isHexDigit)
import Data.Foldable qualified as F
import Data.Hash.Class.Mutable (Context)
import Data.Hash.Class.Mutable qualified as H
import Data.Hash.SHA3 (Sha3_256(..))
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.SQLite3.Direct (Database, Utf8(..))
import GHC.Stack (HasCallStack)
import Options.Applicative (ParserInfo, Parser, (<**>))
import Options.Applicative qualified as O
import Pact.Types.SQLite qualified as Pact
import Pact.Types.SQLite (RType(..), SType(..))
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Logger.Types (LogLevel(..))

computeGrandHash :: Database -> BlockHeight -> IO ByteString
computeGrandHash db bh = do
  let hashStream :: Stream (Of ByteString) IO ()
      hashStream = flip S.mapMaybeM (getLatestPactStateUpperBound' db bh) $ \(tblName, state) -> do
        let rows =
              Vector.fromList
              $ List.sortOn (\pr -> pr.rowKey)
              $ List.map (\(rowKey, PactRowContents{..}) -> PactRow{..})
              $ Map.toList state
        let hash = hashRows (Text.encodeUtf8 tblName) rows
        pure hash

  S.fold_
    (\ctx hash -> hashUpdate ctx (hashWith SHA3_256 hash))
    (hashInitWith SHA3_256)
    (Memory.convert . hashFinalize)
    hashStream

test :: IO ()
test = do

  do
    let assert :: (HasCallStack) => Bool -> IO ()
        assert b = if b then pure () else error "oh brother"
    let cid = unsafeChainId 4
    assert $ findGrandHash 5000 cid == Just "4k"
    assert $ findGrandHash 4000 cid == Just "4k"
    assert $ findGrandHash 3999 cid == Just "3k"
    assert $ findGrandHash 3000 cid == Just "3k"
    assert $ findGrandHash 2999 cid == Just "2k"
    assert $ findGrandHash 2000 cid == Just "2k"
    assert $ findGrandHash 1999 cid == Nothing

  C.withDefaultLogger Info $ \logger -> do
    let resetDb = False
    let cid = unsafeChainId 4
    withSqliteDb cid logger "/home/chessai/sqlite-compacted/sqlite/" resetDb $ \(SQLiteEnv db _) -> do
      ourHash <- computeGrandHash db (BlockHeight maxBound)
      Text.putStrLn (Text.decodeUtf8 $ Base16.encode ourHash)

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

hashRow :: ByteString -> PactRow -> ByteString
hashRow tblName pr =
  Memory.convert
  $ hashWith SHA3_256
  $ rowToHashArgs tblName pr

rowToHashArgs :: ByteString -> PactRow -> ByteString
rowToHashArgs tblName pr =
  BL.toStrict
  $ BB.toLazyByteString
  $ BB.charUtf8 'T'
    <> BB.intDec (BS.length tblName)
    <> BB.byteString tblName

    <> BB.charUtf8 'K'
    <> BB.intDec (BS.length pr.rowKey)
    <> BB.byteString pr.rowKey

    <> BB.charUtf8 'I'
    <> BB.word8Dec (countDigits pr.txId)
    <> BB.int64Dec pr.txId

    <> BB.charUtf8 'D'
    <> BB.intDec (BS.length pr.rowData)
    <> BB.byteString pr.rowData

-- silly
countDigits :: Int64 -> Word8
countDigits i
  | i >= 1_000_000_000_000_000_000 = 19
  | i >=   100_000_000_000_000_000 = 18
  | i >=    10_000_000_000_000_000 = 17
  | i >=     1_000_000_000_000_000 = 16
  | i >=       100_000_000_000_000 = 15
  | i >=        10_000_000_000_000 = 14
  | i >=         1_000_000_000_000 = 13
  | i >=           100_000_000_000 = 12
  | i >=            10_000_000_000 = 11
  | i >=             1_000_000_000 = 10
  | i >=               100_000_000 =  9
  | i >=                10_000_000 =  8
  | i >=                 1_000_000 =  7
  | i >=                   100_000 =  6
  | i >=                    10_000 =  5
  | i >=                     1_000 =  4
  | i >=                       100 =  3
  | i >=                        10 =  2
  | otherwise                      =  1

data Config = Config
  { sourcePactDir :: FilePath
  , targetPactDir :: Maybe FilePath
  , chainwebVersion :: ChainwebVersion
  , targetBlockHeight :: Maybe BlockHeight
  }

-- sorted in descending order.
-- this is currently bogus data.
grandHashes :: [(BlockHeight, Map ChainId ByteString)]
grandHashes = List.sortOn (Down . fst) -- insurance
  [ (4000, Map.fromList [(unsafeChainId 4, "4k")])
  , (3000, Map.fromList [(unsafeChainId 4, "3k")])
  , (2000, Map.fromList [(unsafeChainId 4, "2k")])
  ]

-- | Finds the first element of `grandHashes` such that
--   the blockheight therein is less than or equal to
--   the argument blockheight. We then look up the GrandHash
--   for the cid, and if we get a value, we return it. Otherwise
--   we keep looking.
--
findGrandHash :: BlockHeight -> ChainId -> Maybe ByteString
findGrandHash bh0 cid = go grandHashes
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

--withReadOnlyRocksDb pathToRocksDb modernDefaultOptions $ \rocksDb -> do
{-
getBlockHeader :: RocksDb -> BlockHeight -> IO BlockHeader
getBlockHeader rdb bh = do
  wbhdb <- initWebBlockHeaderDb rocksDb chainwebVersion
  let cutHashes = cutHashesTable rocksDb
  highestCuts <- readHighestCutHeaders chainwebVersion someLogFunc wbhdb cutHashes
  pure undefined
-}
main :: IO ()
main = do
  cfg <- O.execParser opts

  let target = fromMaybe (BlockHeight maxBound) cfg.targetBlockHeight
  let cids = List.sort $ F.toList $ chainIdsAt cfg.chainwebVersion target

  -- We check this as a precondition, because any missing chainIds are
  -- unacceptable, so we can abort.
  forM_ cids $ \cid -> do
    e <- doesPactDbExist cid cfg.sourcePactDir
    when (not e) $ do
      error $ "Pact database doesn't exist for expected chain id " <> Text.unpack (chainIdToText cid)

  chainHashesRef <- newIORef @(Map ChainId ByteString) Map.empty

  -- Make this multi-threaded once per-chain grandhash is correct
  forM_ cids $ \cid -> do
    C.withDefaultLogger Info $ \logger -> do
      let resetDb = False
      withSqliteDb cid logger cfg.sourcePactDir resetDb $ \(SQLiteEnv db _) -> do
        hash <- computeGrandHash db target
        atomicModifyIORef' chainHashesRef $ \m -> (Map.insert cid hash m, ())

  chainHashes <- readIORef chainHashesRef
  flip Map.foldMapWithKey chainHashes $ \cid newlyComputedGrandHash -> do
    case findGrandHash (BlockHeight maxBound) cid of
      Nothing -> do
        error "impossible"
      Just preComputedGrandHash -> do
        let preHex = Text.decodeUtf8 (Base16.encode preComputedGrandHash)
        let newHex = Text.decodeUtf8 (Base16.encode newlyComputedGrandHash)
        when (preComputedGrandHash /= newlyComputedGrandHash) $ do
          error $ unlines
            [ "Grand Hash mismatch"
            , "  Expected: " <> Text.unpack preHex
            , "  Actual:   " <> Text.unpack newHex
            ]

  case cfg.targetPactDir of
    Nothing -> do
      pure ()
    Just targetDir -> do
      createDirectoryIfMissing False targetDir
      forM_ cids $ \cid -> do
        copyFile
          (chainwebDbFilePath cid cfg.sourcePactDir)
          (chainwebDbFilePath cid targetDir)
  where
    opts :: ParserInfo Config
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compare the grand hash of a Pact database to an expected value."
      , "If the hash matches, optionally import the database into a target directory."
      ]

    parser :: Parser Config
    parser = Config
      <$> O.strOption
            (O.long "source-database-dir"
             <> O.short 's'
             <> O.metavar "PACT DBDIR"
             <> O.help "Source Pact database, which you wish to verify"
            )
      <*> optional (O.strOption
            (O.long "target-database-dir"
             <> O.short 't'
             <> O.metavar "PACT DBDIR"
             <> O.help "Target directory to copy the verified Pact database into"
            ))
      <*> fmap versionFromText (O.strOption
            (O.long "graph-version"
             <> O.short 'v'
             <> O.metavar "CHAINWEB_VERSION"
             <> O.help "Chainweb version for graph. Only needed for non-standard graphs."
             <> O.value (toText (_versionName mainnet))
             <> O.showDefault
            ))
      <*> optional (fmap BlockHeight $ O.option O.auto
            (O.long "target-blockheight"
             <> O.short 'b'
             <> O.metavar "BLOCKHEIGHT"
             <> O.help "BlockHeight to verify."
            ))

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
