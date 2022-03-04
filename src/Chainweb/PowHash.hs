{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.PowHash
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Proof-Of-Work hash
--
module Chainweb.PowHash
( PowHash
, powHashBytes
, mkPowHash
, unsafeMkPowHash
, PowHashBytesCount
, powHashBytesCount
, encodePowHash
, decodePowHash
, powHash
) where

import Control.DeepSeq
import Control.Monad.Catch

import qualified Crypto.Hash as C (hash)
import Crypto.Hash.Algorithms

import Data.Aeson
import Data.Bits
import qualified Data.ByteArray as BA
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import Data.Hashable hiding (hash)
import Data.Proxy

import Foreign.Storable

import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeNats

import Numeric.Natural

import System.IO.Unsafe

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- PowHash

type PowHashBytesCount = 32

powHashBytesCount :: Natural
powHashBytesCount = natVal $ Proxy @PowHashBytesCount
{-# INLINE powHashBytesCount #-}

newtype PowHash = PowHash SB.ShortByteString
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Smart constructor
--
mkPowHash :: MonadThrow m => B.ByteString -> m PowHash
mkPowHash = runGet decodePowHash
{-# INLINE mkPowHash #-}

unsafeMkPowHash :: HasCallStack => B.ByteString -> PowHash
unsafeMkPowHash = fromJuste . runGet decodePowHash
{-# INLINE unsafeMkPowHash #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag PowHash where
    type Tag PowHash = 'PowHashTag
    toMerkleNode = encodeMerkleInputNode encodePowHash
    fromMerkleNode = decodeMerkleInputNode decodePowHash
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodePowHash :: MonadPut m => PowHash -> m ()
encodePowHash (PowHash w) = putByteString $ SB.fromShort w
{-# INLINE encodePowHash #-}

powHashBytes :: PowHash -> SB.ShortByteString
powHashBytes (PowHash bytes) = bytes
{-# INLINE powHashBytes #-}

decodePowHash :: MonadGet m => m PowHash
decodePowHash = PowHash . SB.toShort <$> getBytes (int powHashBytesCount)
{-# INLINE decodePowHash #-}

instance Hashable PowHash where
    hashWithSalt s (PowHash bytes) = xor s
        . unsafePerformIO
        $ BA.withByteArray (SB.fromShort bytes) (peek @Int)
    -- PowHashs are already cryptographically strong hashes
    -- that include the chain id.
    {-# INLINE hashWithSalt #-}

instance ToJSON PowHash where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodePowHash
    toEncoding = toEncoding . encodeB64UrlNoPaddingText . runPutS . encodePowHash
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON PowHash where
    parseJSON = withText "PowHash" $ \t ->
        either (fail . show) return
            $ runGet decodePowHash =<< decodeB64UrlNoPaddingText t
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Cryptographic Hash

powHash :: ChainwebVersion -> B.ByteString -> PowHash
powHash Test{} = cryptoHash @Blake2s_256
powHash TimedConsensus{} = cryptoHash @Blake2s_256
powHash PowConsensus{} = cryptoHash @Blake2s_256
powHash TimedCPM{} = cryptoHash @Blake2s_256
powHash FastTimedCPM{} = cryptoHash @Blake2s_256
powHash Development = cryptoHash @Blake2s_256
powHash Testnet04 = cryptoHash @Blake2s_256
powHash Mainnet01 = cryptoHash @Blake2s_256

cryptoHash :: forall a . HashAlgorithm a => B.ByteString -> PowHash
cryptoHash = PowHash . SB.toShort . BA.convert . C.hash @_ @a
