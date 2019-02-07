{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Word.Encoding
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.Word.Encoding
( WordEncoding(..)
, properties
) where

import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import Data.DoubleWord(Word128(..), Word256(..))
import Data.Word

import Test.QuickCheck

class WordEncoding w where
    encodeWordLe :: MonadPut m => w -> m ()
    decodeWordLe :: MonadGet m => m w

    encodeWordBe :: MonadPut m => w -> m ()
    decodeWordBe :: MonadGet m => m w

instance WordEncoding Word8 where
    encodeWordLe = putWord8
    decodeWordLe = getWord8
    encodeWordBe = putWord8
    decodeWordBe = getWord8
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word16 where
    encodeWordLe = putWord16le
    decodeWordLe = getWord16le
    encodeWordBe = putWord16be
    decodeWordBe = getWord16be
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word32 where
    encodeWordLe = putWord32le
    decodeWordLe = getWord32le
    encodeWordBe = putWord32be
    decodeWordBe = getWord32be
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word64 where
    encodeWordLe = putWord64le
    decodeWordLe = getWord64le
    encodeWordBe = putWord64be
    decodeWordBe = getWord64be
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word128 where
    encodeWordLe (Word128 a b) = encodeWordLe b *> encodeWordLe a
    decodeWordLe = flip Word128 <$> decodeWordLe <*> decodeWordLe
    encodeWordBe (Word128 a b) = encodeWordBe a *> encodeWordBe b
    decodeWordBe = Word128 <$> decodeWordBe <*> decodeWordBe
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word256 where
    encodeWordLe (Word256 a b) = encodeWordLe b *> encodeWordLe a
    decodeWordLe = flip Word256 <$> decodeWordLe <*> decodeWordLe
    encodeWordBe (Word256 a b) = encodeWordBe a *> encodeWordBe b
    decodeWordBe = Word256 <$> decodeWordBe <*> decodeWordBe
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

-- -------------------------------------------------------------------------- --
-- Properties

prop_bigEndian
    :: forall a
    . Integral a
    => Bounded a
    => WordEncoding a
    => Bits a
    => Bool
prop_bigEndian = all run [1 .. (bitSize (undefined :: a) `div` 8 -  1)]
  where
    run i = (==) i
        $ length
        $ takeWhile (== 0x00)
        $ B.unpack
        $ runPutS
        $ encodeWordBe
        $ maxBound @a `div` 2^(8*i)

prop_littleEndian
    :: forall a
    . Integral a
    => Bounded a
    => WordEncoding a
    => Bits a
    => Bool
prop_littleEndian = all run [1 .. (bitSize (undefined :: a) `div` 8 - 1)]
  where
    run i = (==) i
        $ length
        $ takeWhile (== 0x00)
        $ reverse
        $ B.unpack
        $ runPutS
        $ encodeWordLe
        $ maxBound @a `div` 2^(8*i)

properties :: [(String, Property)]
properties =
    [ ("Word128 little endian encoding", property $ prop_littleEndian @Word128)
    , ("Word256 little endian encoding", property $ prop_littleEndian @Word128)
    , ("Word128 big endian encoding", property $ prop_bigEndian @Word128)
    , ("Word256 big endian encoding", property $ prop_bigEndian @Word128)
    ]

