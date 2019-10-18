{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.Core
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental

module Chainweb.Miner.Core
  ( HeaderBytes(..)
  , TargetBytes(..)
  , ChainBytes(..)
  , WorkBytes(..), workBytes, unWorkBytes
  , usePowHash
  , mine
  ) where

import Crypto.Hash.Algorithms (Blake2s_256)
import Crypto.Hash.IO

import Data.Bifunctor (second)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))
import Data.Word (Word64, Word8)

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, poke)

import Servant.API

-- internal modules

import Chainweb.BlockHeader (Nonce(..))
import Chainweb.Version (ChainwebVersion(..))

---

-- | Encoding of @ChainId + HashTarget + BlockHeader@ to be consumed by a remote
-- Mining API.
--
newtype WorkBytes = WorkBytes { _workBytes :: B.ByteString }
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream)

-- | The encoded form of a `BlockHeader`.
--
newtype HeaderBytes = HeaderBytes { _headerBytes :: B.ByteString }
    deriving stock (Eq, Show)
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream)

-- | The encoded form of a `HashTarget`.
--
newtype TargetBytes = TargetBytes { _targetBytes :: B.ByteString }
    deriving stock (Eq, Show)

-- | The encoded form of a `ChainId`.
--
newtype ChainBytes = ChainBytes { _chainBytes :: B.ByteString }
    deriving stock (Eq, Show)
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream)

-- | Combine `ChainBytes`, `TargetBytes` and `HeaderBytes` in such a way that
-- can be later undone by `unWorkBytes`.
--
workBytes :: ChainBytes -> TargetBytes -> HeaderBytes -> WorkBytes
workBytes (ChainBytes c) (TargetBytes t) (HeaderBytes h) = WorkBytes $ c <> t <> h

-- | NOTE: This makes a low-level assumption about the encoded size of
-- `HashTarget` and `ChainId`!
--
unWorkBytes :: WorkBytes -> T3 ChainBytes TargetBytes HeaderBytes
unWorkBytes (WorkBytes w) = T3 (ChainBytes c) (TargetBytes t) (HeaderBytes h)
  where
    (c, (t, h)) = second (B.splitAt 32) $ B.splitAt 4 w

-- | Select a hashing algorithm.
--
usePowHash :: ChainwebVersion -> (forall a. HashAlgorithm a => Proxy a -> f) -> f
usePowHash Test{} f = f $ Proxy @Blake2s_256
usePowHash TimedConsensus{} f = f $ Proxy @Blake2s_256
usePowHash PowConsensus{} f = f $ Proxy @Blake2s_256
usePowHash TimedCPM{} f = f $ Proxy @Blake2s_256
usePowHash FastTimedCPM{} f = f $ Proxy @Blake2s_256
usePowHash Development f = f $ Proxy @Blake2s_256
usePowHash Testnet02 f = f $ Proxy @Blake2s_256

-- | This Miner makes low-level assumptions about the chainweb protocol. It may
-- break if the protocol changes.
--
-- TODO: Check the chainweb version to make sure this function can handle the
-- respective version.
--
-- TODO: Remove the `Proxy`?
--
mine
  :: forall a. HashAlgorithm a
  => Proxy a
  -> Nonce
  -> TargetBytes
  -> HeaderBytes
  -> IO (T2 HeaderBytes Word64)
mine _ orig@(Nonce o) (TargetBytes tbytes) (HeaderBytes hbytes) = do
    nonces <- newIORef 0
    BA.withByteArray tbytes $ \trgPtr -> do
      !ctx <- hashMutableInit @a
      new <- fmap HeaderBytes . BA.copy hbytes $ \buf ->
          allocaBytes (powSize :: Int) $ \pow -> do

              -- inner mining loop
              --
              let go !n@(Nonce nv) = do
                      -- Compute POW hash for the nonce
                      injectNonce n buf
                      hash ctx buf pow

                      -- check whether the nonce meets the target
                      fastCheckTarget trgPtr (castPtr pow) >>= \case
                          True -> writeIORef nonces (nv - o)
                          False -> go (Nonce $ nv + 1)

              -- Start inner mining loop
              go orig
      T2 new <$> readIORef nonces
  where
    bufSize :: Int
    !bufSize = B.length hbytes

    powSize :: Int
    !powSize = hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf $ fromIntegral bufSize
            hashInternalFinalize ctxPtr $ castPtr pow
    {-# INLINE hash #-}

    -- | `injectNonce` makes low-level assumptions about the byte layout of a
    -- hashed `BlockHeader`. If that layout changes, this functions need to be
    -- updated. The assumption allows us to iterate on new nonces quickly.
    --
    injectNonce :: Nonce -> Ptr Word8 -> IO ()
    injectNonce (Nonce n) buf = poke (castPtr buf) n
    {-# INLINE injectNonce #-}

    -- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers
    -- in little endian encoding, hence we compare against the target from the
    -- end of the bytes first, then move toward the front 8 bytes at a time.
    --
    fastCheckTarget :: Ptr Word64 -> Ptr Word64 -> IO Bool
    fastCheckTarget !trgPtr !powPtr =
        fastCheckTargetN 3 trgPtr powPtr >>= \case
            LT -> return False
            GT -> return True
            EQ -> fastCheckTargetN 2 trgPtr powPtr >>= \case
                LT -> return False
                GT -> return True
                EQ -> fastCheckTargetN 1 trgPtr powPtr >>= \case
                    LT -> return False
                    GT -> return True
                    EQ -> fastCheckTargetN 0 trgPtr powPtr >>= \case
                        LT -> return False
                        GT -> return True
                        EQ -> return True
    {-# INLINE fastCheckTarget #-}

    -- | Recall that `peekElemOff` acts like `drop` for the size of the type in
    -- question. Here, this is `Word64`. Since our hash is treated as a
    -- `Word256`, each @n@ knocks off a `Word64`'s worth of bytes, and there
    -- would be 4 such sections (64 * 4 = 256).
    --
    -- This must never be called for @n >= 4@.
    --
    fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
    fastCheckTargetN n trgPtr powPtr = compare
        <$> peekElemOff trgPtr n
        <*> peekElemOff powPtr n
    {-# INLINE fastCheckTargetN #-}
