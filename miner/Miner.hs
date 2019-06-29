{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Sits idle upon startup until
-- work is submitted.
--

module Main ( main ) where

import Crypto.Hash.IO
import qualified Data.ByteArray as BA
import Data.Bytes.Put
import qualified Data.ByteString as B
import Data.Proxy (Proxy)
import Data.Word (Word64, Word8)
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable

-- internal modules
import Chainweb.BlockHeader
import Chainweb.Difficulty (encodeHashTarget)
import Chainweb.Time (Micros, Time, encodeTimeToWord64, getCurrentTimeIntegral)
import Chainweb.Utils (int, runGet)

---

{- OPERATIONS

SUBMIT - Submit a hash to mine upon. Result is stored in a `TVar` (or queue?)
when finished. If the miner is currently running when SUBMIT is called, its old
job is cancelled.

POLL - Fetch the mining result of some given job (keyed by hash). Errors (with
Nothing?) if the given key has no stored result.

HALT - stop active mining. No-op if already stopped.

There's probably something elegant I can do with STM to hook in the Submit/Halt
logic. Just passing an `Async` around and killing it when new work comes in
seems dirty. Would there also be memory issues if certain low-level things are
freed correctly?

-}

main :: IO ()
main = pure ()

-- submit ::

-- | This Miner makes low-level assumptions about the chainweb protocol. It may
-- break if the protocol changes.
--
-- TODO: Check the chainweb version to make sure this function can handle the
-- respective version.
--
mine
    :: forall a
    . HashAlgorithm a
    => Proxy a
    -> BlockHeader
    -> Nonce
    -> IO BlockHeader
mine _ h nonce = BA.withByteArray initialTargetBytes $ \trgPtr -> do
    !ctx <- hashMutableInit @a
    bytes <- BA.copy initialBytes $ \buf ->
        allocaBytes (powSize :: Int) $ \pow -> do

            -- inner mining loop
            --
            -- We do 100000 hashes before we update the creation time.
            --
            let go 100000 !n = do
                    -- update the block creation time
                    ct <- getCurrentTimeIntegral
                    injectTime ct buf
                    go 0 n

                go !i !n = do
                    -- Compute POW hash for the nonce
                    injectNonce n buf
                    hash ctx buf pow

                    -- check whether the nonce meets the target
                    fastCheckTarget trgPtr (castPtr pow) >>= \case
                        True -> return ()
                        False -> go (succ i) (succ n)

            -- Start inner mining loop
            go (0 :: Int) nonce

    -- On success: deserialize and return the new BlockHeader
    runGet decodeBlockHeaderWithoutHash bytes
  where
    !initialBytes = runPutS $ encodeBlockHeaderWithoutHash h
    !bufSize = B.length initialBytes
    !target = _blockTarget h
    !initialTargetBytes = runPutS $ encodeHashTarget target
    !powSize = int $ hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf (int bufSize)
            hashInternalFinalize ctxPtr (castPtr pow)
    {-# INLINE hash #-}

    injectTime :: Time Micros -> Ptr Word8 -> IO ()
    injectTime t buf = pokeByteOff buf 8 $ encodeTimeToWord64 t
    {-# INLINE injectTime #-}

    injectNonce :: Nonce -> Ptr Word8 -> IO ()
    injectNonce n buf = poke (castPtr buf) $ encodeNonceToWord64 n
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
