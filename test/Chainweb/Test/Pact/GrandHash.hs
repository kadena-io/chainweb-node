{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Test.Pact.GrandHash
  ( tests
  )
  where

import Chainweb.Pact.Backend.PactState (PactRow(..))
import Chainweb.Pact.Backend.PactState.GrandHash (rowToHashInput)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Bytes qualified as Bytes
import Data.Bytes.Parser (Parser)
import Data.Bytes.Parser qualified as Smith
import Data.Bytes.Parser.Ascii qualified as Smith
import Data.Bytes.Parser.LittleEndian qualified as SmithLE
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word8, Word32, Word64)
import Test.QuickCheck (Property, Arbitrary, Gen, (===), arbitrary, elements)
import Test.Tasty (TestTree, DependencyType(..), sequentialTestGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  sequentialTestGroup "Chainweb.Test.Pact.GrandHash" AllSucceed
    [ testCase "habibti ascii" (testPactRow habibtiAscii)
    , testCase "habibti utf8" (testPactRow habibtiUtf8)
    , testProperty "arbitrary utf8" arbitraryUtf8
    ]

habibtiAscii :: PactRow
habibtiAscii = PactRow
  { rowKey = Text.encodeUtf8 "Habibti"
  , rowData = Text.encodeUtf8 "Rice cakes"
  , txId = 0
  }

habibtiUtf8 :: PactRow
habibtiUtf8 = PactRow
  { rowKey = Text.encodeUtf8 "حبيبتي"
  , rowData = Text.encodeUtf8 ""
  , txId = 0
  }

arbitraryUtf8 :: PactRow -> Property
arbitraryUtf8 row =
  Right row === parseRowHashInput (rowToHashInput row)

instance Arbitrary PactRow where
  arbitrary = genPactRow

genPactRow :: Gen PactRow
genPactRow = do
  txid <- arbitrary @Word32
  rkLen <- arbitrary @Word8
  rdLen <- arbitrary @Word8

  rk <- genUtf8 (fromIntegral @Word8 @Word rkLen)
  rd <- genUtf8 (fromIntegral @Word8 @Word rdLen)

  pure $ PactRow
    { rowKey = rk
    , txId = fromIntegral @Word32 @Int64 txid
    , rowData = rd
    }

genUtf8 :: Word -> Gen ByteString
genUtf8 numCodepoints = do
  string <- replicateM (fromIntegral @Word @Int numCodepoints) $ do
    plane <- elements [Plane1, Plane2, Plane3, Plane4]
    genUtf8Codepoint plane
  pure $ BL.toStrict $ BB.toLazyByteString $ mconcat string

genUtf8Codepoint :: Plane -> Gen BB.Builder
genUtf8Codepoint p = case p of
  Plane1 -> do
    b1 <- elements [0b00000000 .. 0b01000000]
    pure $ BB.word8 b1
  Plane2 -> do
    b1 <- elements [0b11000001 .. 0b11011111]
    b2 <- nonInitial
    pure $ BB.word8 b1 <> BB.word8 b2
  Plane3 -> do
    b1 <- elements [0b11100000 .. 0b11101111]
    b2 <- nonInitial
    b3 <- nonInitial
    pure $ BB.word8 b1 <> BB.word8 b2 <> BB.word8 b3
  Plane4 -> do
    b1 <- elements [0b11110000 .. 0b11110111]
    b2 <- nonInitial
    b3 <- nonInitial
    b4 <- nonInitial
    pure $ BB.word8 b1 <> BB.word8 b2 <> BB.word8 b3 <> BB.word8 b4
  where
    nonInitial :: Gen Word8
    nonInitial = do
      elements [0b10000000 .. 0b10111111]

data Plane = Plane1 | Plane2 | Plane3 | Plane4

testPactRow :: PactRow -> Assertion
testPactRow row = do
  Right row @?= parseRowHashInput (rowToHashInput row)

parseRowHashInput :: ByteString -> Either Text PactRow
parseRowHashInput b = Smith.parseBytesEither parser (Bytes.fromByteString b)
  where
    parser :: Parser Text s PactRow
    parser = do
      _ <- Smith.char "rowkey tag" 'K'
      rkLen <- SmithLE.word64 "rowkey len"
      rk <- Smith.take "rowkey" (fromIntegral @Word64 @Int rkLen)

      _ <- Smith.char "txid tag" 'I'
      txid <- SmithLE.word64 "txid"

      _ <- Smith.char "rowdata tag" 'D'
      rdLen <- SmithLE.word64 "rowdata len"
      rd <- Smith.take "rowdata" (fromIntegral @Word64 @Int rdLen)

      pure $ PactRow
        { rowKey = Bytes.toByteString rk
        , txId = fromIntegral @Word64 @Int64 txid
        , rowData = Bytes.toByteString rd
        }

