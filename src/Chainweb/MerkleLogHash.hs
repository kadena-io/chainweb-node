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
  MerkleLogHash(..) -- FIXME import this only internally
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
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Hashable (Hashable(..))
import Data.MerkleLog hiding (Expected, Actual)
import Data.Proxy
import qualified Data.Text as T

import Foreign.Storable

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeNats

import Numeric.Natural

import System.IO.Unsafe

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.Utils
import Chainweb.Utils.Serialization

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
    deriving newtype (BA.ByteArrayAccess)
    deriving anyclass (NFData)

-- | Smart constructor
--
merkleLogHash
    :: MonadThrow m
    => MerkleHashAlgorithm a
    => B.ByteString
    -> m (MerkleLogHash a)
merkleLogHash = fmap MerkleLogHash . decodeMerkleRoot
{-# INLINE merkleLogHash #-}

unsafeMerkleLogHash
    :: HasCallStack
    => MerkleHashAlgorithm a
    => B.ByteString
    -> MerkleLogHash a
unsafeMerkleLogHash = MerkleLogHash
    . either (error . displayException) id
    . decodeMerkleRoot
{-# INLINE unsafeMerkleLogHash #-}

encodeMerkleLogHash :: MerkleLogHash a -> Put
encodeMerkleLogHash (MerkleLogHash bytes) = putByteString $ encodeMerkleRoot bytes
{-# INLINE encodeMerkleLogHash #-}

decodeMerkleLogHash
    :: MerkleHashAlgorithm a
    => Get (MerkleLogHash a)
decodeMerkleLogHash = unsafeMerkleLogHash <$> getByteString (int merkleLogHashBytesCount)
{-# INLINE decodeMerkleLogHash #-}

instance Hashable (MerkleLogHash a) where
    hashWithSalt s = xor s
        . unsafeDupablePerformIO . flip BA.withByteArray (peek @Int)
    -- BlockHashes are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

nullHashBytes :: MerkleHashAlgorithm a => MerkleLogHash a
nullHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0x00
{-# NOINLINE nullHashBytes #-}

oneHashBytes :: MerkleHashAlgorithm a => MerkleLogHash a
oneHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0xff
{-# NOINLINE oneHashBytes #-}

merkleLogHashToText :: MerkleHashAlgorithm a => MerkleLogHash a -> T.Text
merkleLogHashToText = encodeB64UrlNoPaddingText . runPutS . encodeMerkleLogHash
{-# INLINE merkleLogHashToText #-}

merkleLogHashFromText
    :: MerkleHashAlgorithm a
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
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance MerkleHashAlgorithm a => ToJSONKey (MerkleLogHash a) where
    toJSONKey = toJSONKeyText toText
    {-# INLINE toJSONKey #-}

instance MerkleHashAlgorithm a => FromJSON (MerkleLogHash a) where
    parseJSON = parseJsonFromText "MerkleLogHash"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => FromJSONKey (MerkleLogHash a) where
    fromJSONKey = FromJSONKeyTextParser
        $ either fail return . eitherFromText
    {-# INLINE fromJSONKey #-}
