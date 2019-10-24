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
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental

module Chainweb.Miner.Core
  ( HeaderBytes(..)
  , TargetBytes(..)
  , ChainBytes(..)
  , WorkBytes(..), workBytes, unWorkBytes
  , MiningResult(..)
  , usePowHash
  , mine
  , fastCheckTarget
  , injectNonce
  , callExternalMiner
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Crypto.Hash.Algorithms (Blake2s_256)
import Crypto.Hash.IO

import Data.Bifunctor (second)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Tuple.Strict (T2(..), T3(..))
import Data.Word (Word64, Word8)

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, poke)

import Servant.API

import System.Exit
import System.IO (hClose)
import System.Path
import qualified System.Process as P

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


-- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers in
-- little endian encoding, hence we compare against the target from the end of
-- the bytes first, then move toward the front 8 bytes at a time.
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
fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
fastCheckTargetN n trgPtr powPtr = compare
    <$> peekElemOff trgPtr n
    <*> peekElemOff powPtr n
{-# INLINE fastCheckTargetN #-}

data MiningResult = MiningResult
  { _mrNonceBytes :: !B.ByteString
  , _mrNumNoncesTried :: !Word64
  , _mrEstimatedHashesPerSec :: !Word64
  , _mrStderr :: B.ByteString
  }

callExternalMiner
    :: Path Absolute            -- ^ miner path
    -> [String]                 -- ^ miner extra args
    -> Bool                     -- ^ save stderr?
    -> B.ByteString             -- ^ target hash
    -> B.ByteString             -- ^ block bytes
    -> IO (Either String MiningResult)
callExternalMiner minerPath0 minerArgs saveStderr target blockBytes = do
    minerPath <- toAbsoluteFilePath minerPath0
    let args = minerArgs ++ [targetHashStr]
    P.withCreateProcess (createProcess minerPath args) go
  where
    createProcess minerPath args =
        (P.proc minerPath args) {
            P.std_in = P.CreatePipe,
            P.std_out = P.CreatePipe,
            P.std_err = P.CreatePipe
            }
    targetHashStr = B.unpack $ B16.encode target
    go (Just hstdin) (Just hstdout) (Just hstderr) ph = do
        B.hPut hstdin blockBytes
        hClose hstdin
        Async.withAsync (B.hGetContents hstdout) $ \stdoutThread ->
          Async.withAsync (errThread hstderr) $ \stderrThread ->
          runExceptT $ do
            code <- liftIO $ P.waitForProcess ph
            (outbytes, errbytes) <- liftIO ((,) <$> Async.wait stdoutThread
                                                <*> Async.wait stderrThread)
            if (code /= ExitSuccess)
              then let msg = concat [
                         "Got error from miner. Stderr was: ",
                         B.unpack errbytes
                         ]
                   in throwE msg
              else do
                let parts = B.splitWith isSpace outbytes
                nonceB16 <- case parts of
                              [] -> throwE ("expected nonce from miner, got: "
                                            ++ B.unpack outbytes)
                              (a:_) -> return a
                let (numHashes, rate) =
                      case parts of
                        (_:a:b:_) -> (read (B.unpack a), read (B.unpack b))
                        _ -> (0, 0)

                -- reverse -- we want little-endian
                let nonceBytes = B.reverse $ fst $ B16.decode nonceB16
                when (B.length nonceBytes /= 8) $ throwE "process returned short nonce"
                return $ MiningResult nonceBytes numHashes rate errbytes
    go _ _ _ _ = fail "impossible: process is opened with CreatePipe in/out/err"

    slurp h = act
      where
        act = do
            b <- B.hGet h 4000
            if B.null b then return "stderr not saved" else act

    errThread = if saveStderr
                  then B.hGetContents
                  else slurp

