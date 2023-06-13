{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.Core
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental

module Chainweb.Miner.Core
  ( HeaderBytes(..)
  , TargetBytes(..)
  , ChainBytes(..)
  , WorkBytes(..)
  , usePowHash
  , mine
  ) where

import Control.Monad

import Crypto.Hash.Algorithms (Blake2s_256)
import Crypto.Hash.IO

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import Data.Proxy (Proxy(..))
import Data.Word (Word64, Word8)

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, pokeByteOff, sizeOf)

import Servant.API

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Cut.Create
import Chainweb.Difficulty
import Chainweb.Time hiding (second)
import Chainweb.Utils
import Chainweb.Utils.Serialization
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

-- | Select a hashing algorithm.
--
usePowHash :: ChainwebVersion -> (forall a. HashAlgorithm a => Proxy a -> f) -> f
usePowHash Test{} f = f $ Proxy @Blake2s_256
usePowHash TimedConsensus{} f = f $ Proxy @Blake2s_256
usePowHash PowConsensus{} f = f $ Proxy @Blake2s_256
usePowHash TimedCPM{} f = f $ Proxy @Blake2s_256
usePowHash FastTimedCPM{} f = f $ Proxy @Blake2s_256
usePowHash Development f = f $ Proxy @Blake2s_256
usePowHash Testnet04 f = f $ Proxy @Blake2s_256
usePowHash Mainnet01 f = f $ Proxy @Blake2s_256

-- -------------------------------------------------------------------------- --
-- CPU Mining
--
-- ONLY USE FOR TESTING. BE CAREFUL ABOUT ONLY USING IT WITH HEADERS IN THE
-- CORRECT FORMAT.

noncePosition :: Int
noncePosition = 278
{-# INLINE noncePosition #-}

timestampPosition :: Int
timestampPosition = 8
{-# INLINE timestampPosition #-}

-- | CPU POW mining for chainweb.
--
-- See <https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding>
-- for details about the encoding of 'WorkHeader'.
--
mine
  :: forall a
  . HashAlgorithm a
  => Nonce
  -> WorkHeader
  -> IO SolvedWork
mine orig work = do
    when (bufSize < noncePosition + sizeOf (0 :: Word64)) $
        error "Chainweb.Miner.Core.mine: Buffer is too small to receive the nonce"
    BA.withByteArray tbytes $ \trgPtr -> do
        !ctx <- hashMutableInit @a
        new <- BA.copy hbytes $ \buf ->
            allocaBytes (powSize :: Int) $ \pow -> do

                -- inner mining loop
                --
                let go1 0 n = return (Just n)
                    go1 !i n@(Nonce nv) = do
                        -- Compute POW hash for the nonce
                        injectNonce n buf
                        hash ctx buf pow

                        -- check whether the nonce meets the target
                        fastCheckTarget trgPtr (castPtr pow) >>= \case
                            True -> return Nothing
                            False -> go1 (i - 1) (Nonce $ nv + 1)

                -- outer loop
                -- Estimates how many iterations of the inner loop run in one second. It runs the inner loop
                -- that many times and injects an updated creation time in each cycle.
                let go0 :: Int -> Time Micros -> Nonce -> IO ()
                    go0 x t !n = do
                        injectTime t buf
                        go1 x n >>= \case
                            Nothing -> return ()
                            Just n' -> do
                                t' <- getCurrentTimeIntegral
                                let TimeSpan td = diff t' t
                                    x' = round @Double (int x * 1000000 / int td) -- target 1 second
                                go0 x' t' n'

                -- Start outer mining loop
                t <- getCurrentTimeIntegral
                go0 100000 t orig
        runGetS decodeSolvedWork new
  where
    tbytes = runPutS $ encodeHashTarget (_workHeaderTarget work)
    hbytes = BS.fromShort $ _workHeaderBytes work

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

-- | Inject a nonce value into mining work header.
--
-- Recall: `Nonce` contains a `Word64`, and is thus 8 bytes long.
--
-- See also: https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding
--
injectNonce :: Nonce -> Ptr Word8 -> IO ()
injectNonce (Nonce n) buf = pokeByteOff buf noncePosition n
{-# INLINE injectNonce #-}

-- | Inject a timestamp value into mining work header.
--
-- Updating the creation timestamp is optional for miners. More accurate block
-- creation times improve the accuracy of difficulty adjustment, which is
-- beneficial for miners. In particular it causes mining luck to more accurately
-- reflect the hash rate of a miner.
--
-- See also: https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding
--
injectTime :: Time Micros -> Ptr Word8 -> IO ()
injectTime t buf = pokeByteOff buf timestampPosition $ encodeTimeToWord64 t
{-# INLINE injectTime #-}

-- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers in
-- little endian encoding, hence we compare against the target from the end of
-- the bytes first, then move toward the front 8 bytes at a time.
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
-- question. Here, this is `Word64`. Since our hash is treated as a `Word256`,
-- each @n@ knocks off a `Word64`'s worth of bytes, and there would be 4 such
-- sections (64 * 4 = 256).
--
-- This must never be called for @n >= 4@.
--
fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
fastCheckTargetN n trgPtr powPtr = compare
    <$> peekElemOff trgPtr n
    <*> peekElemOff powPtr n
{-# INLINE fastCheckTargetN #-}

