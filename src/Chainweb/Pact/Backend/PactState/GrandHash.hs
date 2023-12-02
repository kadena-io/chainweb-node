{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( test
  , main
  )
  where

import Control.Applicative (optional)
import Control.Monad (forM, forM_, when)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText, unsafeChainId)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), getLatestPactState', PactRowContents(..))
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, toUtf8, withSqliteDb)
import Chainweb.Utils (fromText, toText)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName)
import Chainweb.Version.Utils (chainIdsAt)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteArray qualified as Memory
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BB
import Data.Char (isHexDigit)
import Data.Foldable qualified as F
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

checkRowHashes :: Database -> Text -> Vector PactRow -> IO ()
checkRowHashes db tblName prs = do
  ref <- do
    let qryText = "SELECT rowkey, hash FROM CompactActiveRow WHERE tablename=\""
          <> toUtf8 tblName <> "\" ORDER BY rowkey"
    rs <- Pact.qry db qryText [] [RText, RBlob]
    fmap Vector.fromList $ forM rs $ \case
      [SText (Utf8 rowkey), SBlob hash] -> pure (rowkey, hash)
      _ -> error "checkRowHash: invalid query"

  when (Vector.length prs /= Vector.length ref) $ do
    error $ unlines
      [ "Length of rows mismatch on table " <> Text.unpack tblName
      , "  CompactActiveRow disagrees with computed rowkeys."
      ]

  forM_ (Vector.zip prs ref) $ \(pr, (refRk, refHash)) -> do
    when (refRk /= pr.rowKey) $ do
      error "checkRowHashes: ordering mismatch"

    let ourHash = hashRow (Text.encodeUtf8 tblName) pr
    when (ourHash /= refHash) $ do
      error $ unlines
        [ "Hash mismatch on table " <> Text.unpack tblName
        ]

compareHash :: Database -> Text -> Maybe ByteString -> IO ()
compareHash db tblName ourHash = do
  refHash <- do
    let qryText = "SELECT hash FROM CompactGrandHash WHERE tablename=\"" <> toUtf8 tblName <> "\""
    Pact.qry db qryText [] [RBlob] >>= \case
      [[SBlob atHash]] -> pure (Just atHash)
      _ -> pure Nothing

  when (ourHash /= refHash) $ do
    error $ unlines
      [ "Hash mismatch on table " ++ Text.unpack tblName ++ ":"
      , "  refHash: " ++ show refHash
      , "  ourHash: " ++ show ourHash
      ]

computeGrandHash :: Database -> IO ByteString
computeGrandHash db = do
  refTableNames <- do
    let qryText = "SELECT tablename FROM CompactGrandHash WHERE tablename IS NOT NULL ORDER BY tablename"
    rs <- Pact.qry db qryText [] [RText]
    forM rs $ \case
      [SText tblName] -> pure (fromUtf8 tblName)
      _ -> error "bad :)"

  ourTableNamesIO <- newIORef @[Text] []

  let hashStream :: Stream (Of ByteString) IO ()
      hashStream = flip S.mapMaybeM (getLatestPactState' db) $ \(tblName, state) -> do
        Text.putStrLn $ "Starting table " <> tblName
        let rows =
              Vector.fromList
              $ List.sortOn (\pr -> pr.rowKey)
              $ List.map (\(rowKey, PactRowContents{..}) -> PactRow{..})
              $ Map.toList state
        checkRowHashes db tblName rows
        let hash = hashRows (Text.encodeUtf8 tblName) rows
        compareHash db tblName hash
        when (isJust hash) $ do
          modifyIORef ourTableNamesIO (tblName :)
        pure hash

  grandHash <- S.foldM_
    (\ctx hash -> pure (hashUpdate ctx (hashWith SHA3_256 hash)))
    (pure (hashInitWith SHA3_256))
    (pure . Memory.convert . hashFinalize)
    hashStream

  ourTableNames <- List.reverse <$> readIORef ourTableNamesIO
  putStr "our table names match the reference table names: "
  print $ refTableNames == ourTableNames

  pure grandHash

test :: IO ()
test = do
  C.withDefaultLogger Info $ \logger -> do
    let resetDb = False
    let cid = unsafeChainId 4
    withSqliteDb cid logger "/home/chessai/sqlite-compacted/sqlite/" resetDb $ \(SQLiteEnv db _) -> do
      refHash <- do
        let qryText = "SELECT hash FROM CompactGrandHash WHERE tablename IS NULL"
        [[SBlob hash]] <- Pact.qry db qryText [] [RBlob]
        pure hash

      ourHash <- computeGrandHash db

      putStr "refHash: " >> print refHash
      putStr "ourHash: " >> print ourHash

      when (ourHash /= refHash) $ do
        error "GrandHash mismatch"

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
    <> BB.byteString tblName
    <> BB.charUtf8 'K'
    <> BB.byteString pr.rowKey
    <> BB.charUtf8 'I'
    <> BB.int64Dec pr.txId
    <> BB.charUtf8 'D'
    <> BB.byteString pr.rowData

data Config = Config
  { sourcePactDir :: FilePath
  , targetPactDir :: Maybe FilePath
  , chainwebVersion :: ChainwebVersion
  , grandHash :: Text
  }

main :: IO ()
main = do
  cfg <- O.execParser opts

  -- Check that the grandhash passed is hex-encoded
  when (not (Text.all isHexDigit cfg.grandHash)) $ do
    error "GrandHash provided is not hex-encoded."

  let cids = List.sort $ F.toList $ chainIdsAt cfg.chainwebVersion (BlockHeight maxBound)

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
        hash <- computeGrandHash db
        atomicModifyIORef' chainHashesRef $ \m -> (Map.insert cid hash m, ())

  chainHashes <- readIORef chainHashesRef
  case hashAggregate id (Vector.fromList (Map.elems chainHashes)) of
    Nothing -> error "impossible"
    Just ourGrandHash -> do
      let ourHexGrandHash = Text.decodeUtf8 (Base16.encode ourGrandHash)
      when (cfg.grandHash /= ourHexGrandHash) $ do
        error $ unlines
          [ "Grand Hash mismatch"
          , "  Expected: " <> Text.unpack cfg.grandHash
          , "  Actual:   " <> Text.unpack ourHexGrandHash
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
      <*> O.strOption
            (O.long "grand-hash"
             <> O.short 'g'
             <> O.metavar "HEX"
             <> O.help "Expected grand hash of database. This must be hex-encoded."
            )

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
