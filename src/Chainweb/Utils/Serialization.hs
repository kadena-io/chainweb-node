{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

module Chainweb.Utils.Serialization
    -- opaque decoder and encoder interface
    ( Get
    , PutM
    , Put
    , runGetL
    , runGetEitherL
    , runGetS
    , runGetEitherS
    , runPutL
    , runPutS
    , label
    -- concrete encoders and decoders
    , putWord8
    , getWord8
    , putWord16le
    , getWord16le
    , putWord16be
    , getWord16be
    , putWord32le
    , getWord32le
    , putWord32be
    , getWord32be
    , putWord64le
    , getWord64le
    , putWord64be
    , getWord64be
    , putByteString
    , getByteString

    -- abstract encoders and decoders
    , WordEncoding(..)
    -- utilities
    , Signed
    , signed
    , Unsigned
    , unsigned
    )
    where

import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce
import Data.DoubleWord (Word128(..), Word256(..))
import Data.Int
import Data.Kind
import qualified Data.Text as T
import Data.Word

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

import Chainweb.Utils

newtype Get a = Get (Binary.Get a)
    deriving newtype (Functor, Applicative, Monad, MonadFail)
newtype PutM a = PutM (Binary.PutM a)
    deriving newtype (Functor, Applicative, Monad)
type Put = PutM ()

instance MonadThrow Get where
    throwM e = Get (fail (show e))

-- | Decode a value from a 'B.ByteString'. In case of a failure a
-- 'DecodeException' is thrown.
--
runGetL :: MonadThrow m => Get a -> BL.ByteString -> m a
runGetL g = fromEitherM . over _Left (DecodeException . T.pack) . runGetEitherL g
{-# INLINE runGetL #-}

-- | Decode a value from a 'B.ByteString' and return either the result or a
-- 'DecodeException'.
--
runGetEitherL :: Get a -> BL.ByteString -> Either String a
runGetEitherL (Get g) = over _Left (view _3) . over _Right (view _3) . Binary.runGetOrFail (g <* eof)
{-# INLINE runGetEitherL #-}

runGetS :: MonadThrow m => Get a -> B.ByteString -> m a
runGetS g = runGetL g . BL.fromStrict
{-# INLINE runGetS #-}

runGetEitherS :: Get a -> B.ByteString -> Either String a
runGetEitherS g = runGetEitherL g . BL.fromStrict
{-# INLINE runGetEitherS #-}

-- | Encode a value into a 'B.ByteString'.
--
runPutL :: Put -> BL.ByteString
runPutL (PutM x) = Binary.runPut x
{-# INLINE runPutL #-}

runPutS :: Put -> B.ByteString
runPutS x = BL.toStrict (runPutL x)
{-# INLINE runPutS #-}

eof :: Binary.Get ()
eof = unlessM Binary.isEmpty $ fail "pending bytes in input"
{-# INLINE eof #-}

label :: forall a. String -> Get a -> Get a
label = coerce (Binary.label :: String -> Binary.Get a -> Binary.Get a)

--------------------
-- Specific encoders/decoders
--------------------

putWord8 :: Word8 -> Put
putWord8 = coerce Binary.putWord8
getWord8 :: Get Word8
getWord8 = coerce Binary.getWord8
putWord16le :: Word16 -> Put
putWord16le = coerce Binary.putWord16le
getWord16le :: Get Word16
getWord16le = coerce Binary.getWord16le
putWord16be :: Word16 -> Put
putWord16be = coerce Binary.putWord16be
getWord16be :: Get Word16
getWord16be = coerce Binary.getWord16be
putWord32le :: Word32 -> Put
putWord32le = coerce Binary.putWord32le
getWord32le :: Get Word32
getWord32le = coerce Binary.getWord32le
putWord32be :: Word32 -> Put
putWord32be = coerce Binary.putWord32be
getWord32be :: Get Word32
getWord32be = coerce Binary.getWord32be
putWord64le :: Word64 -> Put
putWord64le = coerce Binary.putWord64le
getWord64le :: Get Word64
getWord64le = coerce Binary.getWord64le
putWord64be :: Word64 -> Put
putWord64be = coerce Binary.putWord64be
getWord64be :: Get Word64
getWord64be = coerce Binary.getWord64be
getByteString :: Int -> Get B.ByteString
getByteString = coerce Binary.getByteString
putByteString :: B.ByteString -> Put
putByteString = coerce Binary.putByteString

--------------------
-- Abstract encoders/decoders
--------------------

class WordEncoding w where
    encodeWordLe :: w -> Put
    decodeWordLe :: Get w

    encodeWordBe :: w -> Put
    decodeWordBe :: Get w


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
    decodeWordLe = flip Word128 <$!> decodeWordLe <*> decodeWordLe
    encodeWordBe (Word128 a b) = encodeWordBe a *> encodeWordBe b
    decodeWordBe = Word128 <$!> decodeWordBe <*> decodeWordBe
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

instance WordEncoding Word256 where
    encodeWordLe (Word256 a b) = encodeWordLe b *> encodeWordLe a
    decodeWordLe = flip Word256 <$!> decodeWordLe <*> decodeWordLe
    encodeWordBe (Word256 a b) = encodeWordBe a *> encodeWordBe b
    decodeWordBe = Word256 <$!> decodeWordBe <*> decodeWordBe
    {-# INLINE encodeWordLe #-}
    {-# INLINE decodeWordLe #-}
    {-# INLINE encodeWordBe #-}
    {-# INLINE decodeWordBe #-}

-- poached from Data.Bytes.Signed, but safer

type family Unsigned i :: Type

type instance Unsigned Int = Word
type instance Unsigned Int8 = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64

unsigned :: (Integral i, Num (Unsigned i)) => i -> Unsigned i
unsigned = fromIntegral

type family Signed i :: Type

type instance Signed Word   = Int
type instance Signed Word8  = Int8
type instance Signed Word16 = Int16
type instance Signed Word32 = Int32
type instance Signed Word64 = Int64

signed :: (Integral i, Num (Signed i)) => i -> Signed i
signed = fromIntegral