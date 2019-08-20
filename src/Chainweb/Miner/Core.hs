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
  , NonceBytes(..)
  , usePowHash
  , mine
  ) where

import Crypto.Hash.Algorithms (SHA512t_256)
import Crypto.Hash.IO

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Proxy (Proxy(..))
import Data.Word (Word64, Word8)

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, poke)

import Servant.API

-- internal modules

import Chainweb.Version (ChainwebVersion(..))

---

-- | The encoded form of a `BlockHeader`.
--
newtype HeaderBytes = HeaderBytes { _headerBytes :: B.ByteString }
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream)

-- | The encoded form of a `HashTarget`.
--
newtype TargetBytes = TargetBytes { _targetBytes :: B.ByteString }

-- | The encoded form of a `Nonce`.
--
newtype NonceBytes = NonceBytes { _nonceBytes :: Word64 }
    deriving newtype (Enum)

-- | Select a hashing algorithm.
--
usePowHash :: ChainwebVersion -> (forall a. HashAlgorithm a => Proxy a -> f) -> f
usePowHash Test{} f = f $ Proxy @SHA512t_256
usePowHash TimedConsensus{} f = f $ Proxy @SHA512t_256
usePowHash PowConsensus{} f = f $ Proxy @SHA512t_256
usePowHash TimedCPM{} f = f $ Proxy @SHA512t_256
usePowHash Development f = f $ Proxy @SHA512t_256
usePowHash Testnet02 f = f $ Proxy @SHA512t_256

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
  -> TargetBytes
  -> NonceBytes
  -> HeaderBytes
  -> IO HeaderBytes
mine _ (TargetBytes tbytes) nonce (HeaderBytes hbytes) = BA.withByteArray tbytes $ \trgPtr -> do
    !ctx <- hashMutableInit @a
    fmap HeaderBytes . BA.copy hbytes $ \buf ->
        allocaBytes (powSize :: Int) $ \pow -> do

            -- inner mining loop
            --
            let go !n = do
                    -- Compute POW hash for the nonce
                    injectNonce n buf
                    hash ctx buf pow

                    -- check whether the nonce meets the target
                    fastCheckTarget trgPtr (castPtr pow) >>= \case
                        True -> pure ()
                        False -> go (succ n)

            -- Start inner mining loop
            go nonce
  where
    -- tbytes :: B.ByteString
    -- !tbytes = runPutS $ encodeHashTarget target

    bufSize :: Int
    !bufSize = B.length hbytes

    powSize :: Int
    !powSize = hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf (fromIntegral bufSize)
            hashInternalFinalize ctxPtr (castPtr pow)
    {-# INLINE hash #-}

    -- | `injectNonce` makes low-level assumptions about the byte layout of a
    -- hashed `BlockHeader`. If that layout changes, this functions need to be
    -- updated. The assumption allows us to iterate on new nonces quickly.
    --
    injectNonce :: NonceBytes -> Ptr Word8 -> IO ()
    injectNonce (NonceBytes n) buf = poke (castPtr buf) n
    {-# INLINE injectNonce #-}

    -- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers
    -- in little endian encoding.
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

    fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
    fastCheckTargetN n trgPtr powPtr = compare
        <$> peekElemOff trgPtr n
        <*> peekElemOff powPtr n
    {-# INLINE fastCheckTargetN #-}
