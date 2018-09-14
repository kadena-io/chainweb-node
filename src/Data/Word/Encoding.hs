{-# LANGUAGE ScopedTypeVariables #-}

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
) where

import Data.Bytes.Get
import Data.Bytes.Put
import Data.LargeWord
import Data.Word

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

instance (WordEncoding a, WordEncoding b) => WordEncoding (LargeKey a b) where
    encodeWordLe (LargeKey a b) = encodeWordLe a *> encodeWordLe b
    decodeWordLe = LargeKey <$> decodeWordLe <*> decodeWordLe
    encodeWordBe (LargeKey a b) = encodeWordBe b *> encodeWordBe a
    decodeWordBe = flip LargeKey <$> decodeWordBe <*> decodeWordBe
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

