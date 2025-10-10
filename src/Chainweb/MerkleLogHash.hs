{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.MerkleLogHash
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A hash that identifies a node in a Chainweb Merkle Tree. This type usually
-- isn't used directly but wrapped with a newtype for the type that it
-- represents in the tree. Example newtype wrappers are 'BlockHash' and
-- 'BlockPayloadHash'.
--
module Chainweb.MerkleLogHash
(
-- * MerkleLogHash
  MerkleLogHash(..)
, merkleLogHashBytes
, MerkleLogHashBytesCount
, merkleLogHashBytesCount
, merkleLogHash
, unsafeMerkleLogHash
, encodeMerkleLogHash
, decodeMerkleLogHash
, nullHashBytes
, oneHashBytes
) where

import Control.DeepSeq
import Control.Monad.Catch (MonadThrow, displayException, throwM)

import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..))
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import Data.Bits
import qualified Data.ByteString as B
import Data.Hashable (Hashable(..))
import Data.MerkleLog hiding (Expected, Actual)
import Data.Proxy
import qualified Data.Text as T

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeNats

-- internal imports

import Chainweb.Utils
import Chainweb.Utils.Serialization
import Data.Either
import Data.Array.Byte
import Data.Hash.Class.Mutable

-- -------------------------------------------------------------------------- --
-- MerkleLogHash

-- | TODO: consider parameterizing this value on the ChainwebVersion.
-- (e.g. for Test we may want to use just '()' or 'Int')
--

type MerkleLogHashBytesCount = 32

merkleLogHashBytesCount :: Natural
merkleLogHashBytesCount = natVal $ Proxy @MerkleLogHashBytesCount
{-# INLINE merkleLogHashBytesCount #-}

newtype MerkleLogHash a = MerkleLogHash (MerkleRoot a)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Bytes (MerkleLogHash a) where
   byteArray (MerkleLogHash r) = merkleRootBytes r
   {-# INLINE byteArray #-}

-- | Smart constructor
--
merkleLogHash
    :: MonadThrow m
    => IncrementalHash a
    => B.ByteString
    -> m (MerkleLogHash a)
merkleLogHash = fmap MerkleLogHash . decodeMerkleRoot
{-# INLINE merkleLogHash #-}

merkleLogHashBytes :: MerkleLogHash a -> ByteArray
merkleLogHashBytes (MerkleLogHash r) = merkleRootBytes r
{-# INLINE merkleLogHashBytes #-}

unsafeMerkleLogHash
    :: HasCallStack
    => IncrementalHash a
    => B.ByteString
    -> MerkleLogHash a
unsafeMerkleLogHash = MerkleLogHash
    . either (error . displayException) id
    . decodeMerkleRoot
{-# INLINE unsafeMerkleLogHash #-}

encodeMerkleLogHash :: MerkleHashAlgorithm a => MerkleLogHash a -> Put
encodeMerkleLogHash (MerkleLogHash bytes) = putByteString $ encodeMerkleRoot bytes
{-# INLINE encodeMerkleLogHash #-}

decodeMerkleLogHash
    :: IncrementalHash a
    => Get (MerkleLogHash a)
decodeMerkleLogHash = unsafeMerkleLogHash <$> getByteString (int merkleLogHashBytesCount)
{-# INLINE decodeMerkleLogHash #-}

instance Hashable (MerkleLogHash a) where
    hashWithSalt s = xor s . int . fromRight 0 . peekByteArray64
    -- BlockHashes are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

nullHashBytes :: IncrementalHash a => MerkleLogHash a
nullHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0x00
{-# NOINLINE nullHashBytes #-}

oneHashBytes :: IncrementalHash a => MerkleLogHash a
oneHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0xff
{-# NOINLINE oneHashBytes #-}

merkleLogHashToText :: MerkleHashAlgorithm a => MerkleLogHash a -> T.Text
merkleLogHashToText = encodeB64UrlNoPaddingText . runPutS . encodeMerkleLogHash
{-# INLINE merkleLogHashToText #-}

merkleLogHashFromText
    :: IncrementalHash a
    => MonadThrow m
    => T.Text
    -> m (MerkleLogHash a)
merkleLogHashFromText t = either (throwM . TextFormatException . sshow) return
        $ runGetS decodeMerkleLogHash =<< decodeB64UrlNoPaddingText t
{-# INLINE merkleLogHashFromText #-}

instance MerkleHashAlgorithm a => HasTextRepresentation (MerkleLogHash a) where
    toText = merkleLogHashToText
    {-# INLINE toText #-}
    fromText = merkleLogHashFromText
    {-# INLINE fromText #-}

instance MerkleHashAlgorithm a => ToJSON (MerkleLogHash a) where
    toJSON = toJSON . merkleLogHashToText
    toEncoding = toEncoding . merkleLogHashToText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => ToJSONKey (MerkleLogHash a) where
    toJSONKey = toJSONKeyText merkleLogHashToText
    {-# INLINE toJSONKey #-}

instance MerkleHashAlgorithm a => FromJSON (MerkleLogHash a) where
    parseJSON = parseJsonFromText "MerkleLogHash"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => FromJSONKey (MerkleLogHash a) where
    fromJSONKey = FromJSONKeyTextParser
        $ either (fail . displayException) return . fromText
    {-# INLINE fromJSONKey #-}
