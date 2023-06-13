{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.Test.Word.Encoding
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Data.Test.Word.Encoding
( properties
) where

import Data.Bits
import qualified Data.ByteString as B
import Data.DoubleWord (Word128(..))

import Test.QuickCheck

-- internal modules

import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- Properties

prop_bigEndian
    :: forall a
    . Integral a
    => Bounded a
    => WordEncoding a
    => FiniteBits a
    => Bool
prop_bigEndian = all run [1 .. (finiteBitSize (undefined :: a) `div` 8 -  1)]
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
    => FiniteBits a
    => Bool
prop_littleEndian = all run [1 .. (finiteBitSize (undefined :: a) `div` 8 - 1)]
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
