{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Test.Pact4.GrandHash
  ( tests
  )
  where

import Data.ByteArray qualified as Memory
import Data.Functor.Identity (Identity(..))
import Data.ByteString.Base16 qualified as Base16
import Data.Maybe (mapMaybe)
import Crypto.Hash (hashWith)
import Data.List qualified as List
import Chainweb.Pact.Backend.PactState (PactRow(..), Table(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (TableHash(..), rowToHashInput, hashTable, tableNameToHashInput, hashStream, hashAlgorithm)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as BS
import Data.Bytes qualified as Bytes
import Data.Bytes.Parser (Parser)
import Data.Bytes.Parser qualified as Smith
import Data.Bytes.Parser.Ascii qualified as SmithA
import Data.Bytes.Parser.LittleEndian qualified as SmithLE
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word8, Word32, Word64)
import Streaming.Prelude qualified as S
import Test.QuickCheck (Property, Arbitrary, Gen, Positive(..), (===), arbitrary, elements)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Chainweb.Test.Utils

tests :: TestTree
tests =
  independentSequentialTestGroup "Chainweb.Test.Pact4.GrandHash"
    [ testCase "PactRow hash input roundtrip - habibti ascii" (testPactRow habibtiAscii)
    , testCase "PactRow hash input roundtrip - habibti utf8" (testPactRow habibtiUtf8)
    , testProperty "PactRow hash input roundtrip - arbitrary utf8" propPactRowHashInputRoundtrip
    , testProperty "Table hash input roundtrip - arbitrary utf8" propTableHashInputRoundtrip
    , testProperty "Table hash: incremental equals non-incremental" propHashWholeTableEqualsIncremental
    , testProperty "Grand Hash of tables: incremental equals non-incremental" propHashWholeChainEqualsIncremental
    ]

habibtiAscii :: PactRow
habibtiAscii = PactRow
  { rowKey = Text.encodeUtf8 "Habibti"
  , rowData = Text.encodeUtf8 "Asophiel"
  , txId = 2100
  }

habibtiUtf8 :: PactRow
habibtiUtf8 = PactRow
  { rowKey = Text.encodeUtf8 "حبيبتي"
  , rowData = Text.encodeUtf8 "Kaspitell"
  , txId = 2300
  }

propPactRowHashInputRoundtrip :: PactRow -> Property
propPactRowHashInputRoundtrip row =
  Right row === parseRowHashInput (rowToHashInput row)

propTableHashInputRoundtrip :: Text -> Property
propTableHashInputRoundtrip tablename =
  Right (len, tblName) === parseTableHashInput (tableNameToHashInput tablename)
  where
    tblName = Text.encodeUtf8 (Text.toLower tablename)
    len = fromIntegral @Int @Word64 (BS.length tblName)

propHashWholeTableEqualsIncremental :: Table -> Property
propHashWholeTableEqualsIncremental tbl =
  let
    incrementalHash :: Maybe TableHash
    incrementalHash = hashTable tbl.name
      $ Vector.fromList
      $ List.sortOn (\pr -> pr.rowKey) tbl.rows

    wholeHash :: Maybe ByteString
    wholeHash = testHashTableNotIncremental tbl
  in
  fmap (hex . getTableHash) incrementalHash === fmap hex wholeHash

testHashTableNotIncremental :: Table -> Maybe ByteString
testHashTableNotIncremental tbl = if null tbl.rows
  then Nothing
  else Just
       $ Memory.convert
       $ hashWith hashAlgorithm
       $ BL.toStrict
       $ BB.toLazyByteString
       $ (BB.byteString (tableNameToHashInput tbl.name) <>)
       $ foldMap (BB.byteString . rowToHashInput)
       $ List.sortOn (\pr -> pr.rowKey) tbl.rows

propHashWholeChainEqualsIncremental :: [Table] -> Property
propHashWholeChainEqualsIncremental tbls =
  let
    sortedTables = List.sortOn (\tbl -> tbl.name) tbls
    tableHashes = mapMaybe testHashTableNotIncremental sortedTables

    incrementalHash = fst $ runIdentity $ hashStream @Identity (S.each tableHashes)

    wholeHash = Memory.convert $ hashWith hashAlgorithm $ BS.concat tableHashes
  in
  incrementalHash === wholeHash

instance Arbitrary PactRow where
  arbitrary = genPactRow

instance Arbitrary Table where
  arbitrary = genTable

genTable :: Gen Table
genTable = do
  tblNameLen <- elements @Int [3 .. 20]
  tblName <- Text.pack <$> replicateM tblNameLen (arbitrary @Char)

  numRows <- elements @Int [1 .. 10]
  tblRows <- replicateM numRows genPactRow

  pure $ Table
    { name = tblName
    , rows = tblRows
    }

genPactRow :: Gen PactRow
genPactRow = do
  txid <- arbitrary @Word32
  rkLen <- fmap (fromIntegral @_ @Int . getPositive) $ arbitrary @(Positive Word8)
  rdLen <- fmap (fromIntegral @_ @Int . getPositive) $ arbitrary @(Positive Word8)

  rk <- Text.pack <$> replicateM rkLen (arbitrary @Char)
  rd <- Text.pack <$> replicateM rdLen (arbitrary @Char)

  pure $ PactRow
    { rowKey = Text.encodeUtf8 rk
    , txId = fromIntegral @Word32 @Int64 txid
    , rowData = Text.encodeUtf8 rd
    }

testPactRow :: PactRow -> Assertion
testPactRow row = do
  Right row @?= parseRowHashInput (rowToHashInput row)

parseRowHashInput :: ByteString -> Either Text PactRow
parseRowHashInput b = Smith.parseBytesEither parser (Bytes.fromByteString b)
  where
    parser :: Parser Text s PactRow
    parser = do
      _ <- SmithA.char "rowkey tag" 'K'
      rkLen <- SmithLE.word64 "rowkey len"
      rk <- Smith.take "rowkey" (fromIntegral @Word64 @Int rkLen)

      _ <- SmithA.char "txid tag" 'I'
      txid <- SmithLE.word64 "txid"

      _ <- SmithA.char "rowdata tag" 'D'
      rdLen <- SmithLE.word64 "rowdata len"
      rd <- Smith.take "rowdata" (fromIntegral @Word64 @Int rdLen)

      pure $ PactRow
        { rowKey = Bytes.toByteString rk
        , txId = fromIntegral @Word64 @Int64 txid
        , rowData = Bytes.toByteString rd
        }

parseTableHashInput :: ByteString -> Either Text (Word64, ByteString)
parseTableHashInput b = Smith.parseBytesEither parser (Bytes.fromByteString b)
  where
    parser :: Parser Text s (Word64, ByteString)
    parser = do
      _ <- SmithA.char "tablename tag" 'T'
      len <- SmithLE.word64 "tablename len"
      tablename <- Smith.take "tablename" (fromIntegral @Word64 @Int len)
      pure (len, Bytes.toByteString tablename)

hex :: ByteString -> Text
hex = Text.decodeUtf8 . Base16.encode
