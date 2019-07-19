{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.MerkleLogHash
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
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
, randomMerkleLogHash
) where

import Control.DeepSeq
import Control.Monad.Catch (MonadThrow, displayException)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Bits
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Random as BR
import Data.Hashable (Hashable(..))
import Data.MerkleLog hiding (Expected, Actual)
import Data.Proxy

import Foreign.Storable

import GHC.Generics
import GHC.TypeNats
import GHC.Stack (HasCallStack)

import Numeric.Natural

import System.IO.Unsafe

-- internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- MerkleLogHash

-- | TODO: consider parameterizing this value on the ChainwebVersion.
-- (e.g. for Test we may want to use just '()' or 'Int')
--
type MerkleLogHashBytesCount = 32

merkleLogHashBytesCount :: Natural
merkleLogHashBytesCount = natVal $ Proxy @MerkleLogHashBytesCount
{-# INLINE merkleLogHashBytesCount #-}

newtype MerkleLogHash = MerkleLogHash (MerkleRoot (HashAlg ChainwebHashTag))
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (BA.ByteArrayAccess)
    deriving anyclass (NFData)

-- | Smart constructor
--
merkleLogHash :: MonadThrow m => B.ByteString -> m MerkleLogHash
merkleLogHash = fmap MerkleLogHash . decodeMerkleRoot . B.copy
{-# INLINE merkleLogHash #-}

unsafeMerkleLogHash :: HasCallStack => B.ByteString -> MerkleLogHash
unsafeMerkleLogHash = MerkleLogHash
    . either (error . displayException) id
    . decodeMerkleRoot
{-# INLINE unsafeMerkleLogHash #-}

encodeMerkleLogHash :: MonadPut m => MerkleLogHash -> m ()
encodeMerkleLogHash (MerkleLogHash bytes) = putByteString $ encodeMerkleRoot bytes
{-# INLINE encodeMerkleLogHash #-}

decodeMerkleLogHash :: MonadGet m => m MerkleLogHash
decodeMerkleLogHash = unsafeMerkleLogHash <$> getBytes (int merkleLogHashBytesCount)
{-# INLINE decodeMerkleLogHash #-}

instance Hashable MerkleLogHash where
    hashWithSalt s = xor s
        . unsafePerformIO . flip BA.withByteArray (peek @Int)
    -- BlockHashes are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

nullHashBytes :: MerkleLogHash
nullHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0x00
{-# NOINLINE nullHashBytes #-}

oneHashBytes :: MerkleLogHash
oneHashBytes = unsafeMerkleLogHash $ B.replicate (int merkleLogHashBytesCount) 0xff
{-# NOINLINE oneHashBytes #-}

-- | This must be used only for testing. The result hash is uniformily
-- distributed, but not cryptographically safe.
--
randomMerkleLogHash :: MonadIO m => m MerkleLogHash
randomMerkleLogHash = unsafeMerkleLogHash <$> liftIO (BR.random merkleLogHashBytesCount)

instance ToJSON MerkleLogHash where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeMerkleLogHash
    {-# INLINE toJSON #-}

instance FromJSON MerkleLogHash where
    parseJSON = withText "MerkleLogHash" $ \t ->
        either (fail . show) return
            $ runGet decodeMerkleLogHash =<< decodeB64UrlNoPaddingText t
    {-# INLINE parseJSON #-}
