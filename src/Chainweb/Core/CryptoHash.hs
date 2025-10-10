{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Core.CryptoHash
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Base type for cryptographic hashes in Chainweb
--
module Chainweb.Core.CryptoHash
( CryptoHash(..)
, unsafeMkCryptoHash
, encodeCryptoHash
, decodeCryptoHash
, cryptoHashBytes
, cryptoHashToText
, cryptoHashFromText
) where

import Data.ByteString.Short qualified as BS
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Control.DeepSeq (NFData, rnf)
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString qualified as B
import Data.Coerce
import Data.Hash.Class.Mutable
import Data.Hashable (Hashable(..))
import Data.String (IsString)
import Data.Text qualified as T
import GHC.Exts (Int(I#), indexIntArray#)
import GHC.Generics (Generic)
import GHC.Stack

-- -----------------------------------------------------------------------------
-- | Base Type for Cryptographic Hashes in Chainweb.
--
-- This is mostly useful for use with deriving Via.
--
-- It also provides a few convenience functions for working with cryptographic
-- hashes.
--
newtype CryptoHash a = CryptoHash a
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, IncrementalHash, Hash, ResetableHash, IsString)

unsafeMkCryptoHash
    :: HasCallStack
    => Coercible a BS.ShortByteString
    => IncrementalHash a
    => B.ByteString
    -> CryptoHash a
unsafeMkCryptoHash = fromJuste . runGetS decodeCryptoHash
{-# INLINE unsafeMkCryptoHash #-}

encodeCryptoHash :: Coercible a BS.ShortByteString  => CryptoHash a -> Put
encodeCryptoHash (CryptoHash w) = putByteString $ BS.fromShort $ coerce w
{-# INLINE encodeCryptoHash #-}

cryptoHashBytes
    :: Coercible a BS.ShortByteString
    => CryptoHash a
    -> BS.ShortByteString
cryptoHashBytes = coerce
{-# INLINE cryptoHashBytes #-}

decodeCryptoHash
    :: forall a
    . Coercible a BS.ShortByteString
    => IncrementalHash a
    => Get (CryptoHash a)
decodeCryptoHash = coerce . BS.toShort
    <$> getByteString (digestSize @(CryptoHash a))
{-# INLINE decodeCryptoHash #-}

cryptoHashToText
    :: Coercible a BS.ShortByteString
    => CryptoHash a
    -> T.Text
cryptoHashToText = encodeB64UrlNoPaddingText . runPutS . encodeCryptoHash
{-# INLINE cryptoHashToText #-}

cryptoHashFromText
    :: Coercible a BS.ShortByteString
    => IncrementalHash a
    => MonadThrow m
    => T.Text
    -> m (CryptoHash a)
cryptoHashFromText t = either (throwM . TextFormatException . sshow) return
    $ runGetS decodeCryptoHash =<< decodeB64UrlNoPaddingText t
{-# INLINE cryptoHashFromText #-}

instance Coercible a BS.ShortByteString => NFData (CryptoHash a) where
    rnf = rnf . cryptoHashBytes
    {-# INLINE rnf #-}

instance (Coercible a BS.ShortByteString, Eq a) => Hashable (CryptoHash a) where
    -- CryptoHashs are already cryptographically strong hashes
    -- that include the chain id.
    hashWithSalt s h =
        hashWithSalt s $ I# $ indexIntArray# arr# 0#
      where
        !(BS.SBS arr#) = coerce h
    {-# INLINE hashWithSalt #-}

instance Coercible a BS.ShortByteString => ToJSON (CryptoHash a) where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeCryptoHash
    toEncoding = b64UrlNoPaddingTextEncoding . runPutS . encodeCryptoHash
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance (Coercible a BS.ShortByteString, IncrementalHash a) => FromJSON (CryptoHash a) where
    parseJSON = withText "CryptoHash" $ \t ->
        either (fail . show) return
            $ runGetS decodeCryptoHash =<< decodeB64UrlNoPaddingText t
    {-# INLINE parseJSON #-}

instance (IncrementalHash a, Coercible a BS.ShortByteString) => HasTextRepresentation (CryptoHash a) where
    toText = encodeB64UrlNoPaddingText . runPutS . encodeCryptoHash
    fromText = fmap unsafeMkCryptoHash . decodeB64UrlNoPaddingText
    {-# INLINE toText #-}
    {-# INLINE fromText #-}

