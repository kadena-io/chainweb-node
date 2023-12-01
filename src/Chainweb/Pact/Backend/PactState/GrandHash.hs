{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Pact.Backend.PactState.GrandHash
  ( test
  )
  where

import Control.Monad (forM, forM_, when)
import Crypto.Hash (hashWith, hashInitWith, hashUpdate, hashFinalize)
import Crypto.Hash.Algorithms (SHA3_256(..))
import Chainweb.ChainId (unsafeChainId)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (PactRow(..), getLatestPactState', PactRowContents(..))
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Pact.Backend.Utils (fromUtf8, toUtf8, withSqliteDb)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteArray qualified as Memory
import Data.ByteString.Builder qualified as BB
import Data.Coerce (coerce)
import Data.IORef
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.SQLite3.Direct (Database, Utf8(..))
import Pact.Types.SQLite qualified as Pact
import Pact.Types.SQLite (RType(..), SType(..))
import Streaming.Prelude (Stream, Of)
import Streaming.Prelude qualified as S
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

singleRowTest :: Database -> IO ()
singleRowTest db = do
  (tbl, rk, arHash) <- do
    let qryText = "SELECT tablename, rowkey, hash FROM CompactActiveRow"
          <> " ORDER BY rowkey LIMIT 1"
    [[SText tbl, SText rk, SBlob arHash]]  <- Pact.qry db qryText [] [RText, RText, RBlob]
    pure (tbl, rk, arHash)

  pr <- do
    let qryText = "SELECT rowkey, rowdata, txid FROM \"" <> tbl <> "\""
                  <> " WHERE rowkey=\"" <> rk <> "\""
                  <> " ORDER BY txid DESC LIMIT 1"
    [[SText (Utf8 rowKey), SBlob rowData, SInt txId]] <- Pact.qry db qryText [] [RText, RBlob, RInt]
    pure $ PactRow {..}

  print $ arHash == hashRow (coerce tbl) pr

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
  print $ refTableNames == ourTableNames

  pure grandHash

hashTablesTest :: Database -> IO ()
hashTablesTest db = do
  refHash <- do
    let qryText = "SELECT hash FROM CompactGrandHash WHERE tablename IS NULL"
    [[SBlob hash]] <- Pact.qry db qryText [] [RBlob]
    pure hash

  ourHash <- computeGrandHash db

  putStr "refHash: " >> print refHash
  putStr "ourHash: " >> print ourHash

  when (ourHash /= refHash) $ do
    error "GrandHash mismatch"

test :: IO ()
test = do
  C.withDefaultLogger Info $ \logger -> do
    let resetDb = False
    let cid = unsafeChainId 4
    withSqliteDb cid logger "/home/chessai/sqlite-compacted/sqlite/" resetDb $ \(SQLiteEnv db _) -> do
      singleRowTest db
      hashTablesTest db

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
