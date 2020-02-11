{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.BlockHeight
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeight
(
-- * Block Height
  BlockHeight(..)
, encodeBlockHeight
, decodeBlockHeight
, encodeBlockHeightBe
, decodeBlockHeightBe
) where

import Control.DeepSeq

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable
import Data.Word

import GHC.Generics (Generic)

-- Internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- | BlockHeight
--
newtype BlockHeight = BlockHeight { _height :: Word64 }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum
        )
instance Show BlockHeight where show (BlockHeight b) = show b

instance IsMerkleLogEntry ChainwebHashTag BlockHeight where
    type Tag BlockHeight = 'BlockHeightTag
    toMerkleNode = encodeMerkleInputNode encodeBlockHeight
    fromMerkleNode = decodeMerkleInputNode decodeBlockHeight
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- | Little endian encoding of block height. This the default encoding for
-- exchanging chainweb data.
--
encodeBlockHeight :: MonadPut m => BlockHeight -> m ()
encodeBlockHeight (BlockHeight h) = putWord64le h

-- | Little endian encoding of block height. This the default encoding for
-- exchanging chainweb data.
--
decodeBlockHeight :: MonadGet m => m BlockHeight
decodeBlockHeight = BlockHeight <$> getWord64le

-- | Encodings for data exchange use little endian by default. Big endian
-- encodings are provided for use in internal storage when a bytewise
-- lexicographcial ordering is required.
--
encodeBlockHeightBe :: MonadPut m => BlockHeight -> m ()
encodeBlockHeightBe (BlockHeight r) = putWord64be r

-- | Encodings for data exchange use little endian by default. Big endian
-- encodings are provided for use in internal storage when a bytewise
-- lexicographcial ordering is required.
--
decodeBlockHeightBe :: MonadGet m => m BlockHeight
decodeBlockHeightBe = BlockHeight <$> getWord64be

