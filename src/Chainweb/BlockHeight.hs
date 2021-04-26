{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Chainweb Block heights. Note the height of genesis blocks can be strictly
-- larger than zero, if the chain was added to the network during a graph
-- transition.
--
module Chainweb.BlockHeight
(
-- * Block Height
  BlockHeight(..)
, encodeBlockHeight
, decodeBlockHeight
, encodeBlockHeightBe
, decodeBlockHeightBe

-- * Cut Height
, CutHeight(..)
, encodeCutHeight
, decodeCutHeight
, encodeCutHeightBe
, decodeCutHeightBe
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
        , Num, Integral, Real, Enum, Bounded
        )
instance Show BlockHeight where show (BlockHeight b) = show b

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BlockHeight where
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

-- -------------------------------------------------------------------------- --
-- Cut Height

newtype CutHeight = CutHeight Word64
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum, Bounded
        )

instance Show CutHeight where show (CutHeight b) = show b

-- | Little endian encoding of block height. This the default encoding for
-- exchanging chainweb data.
--
encodeCutHeight :: MonadPut m => CutHeight -> m ()
encodeCutHeight (CutHeight h) = putWord64le h

-- | Little endian encoding of block height. This the default encoding for
-- exchanging chainweb data.
--
decodeCutHeight :: MonadGet m => m CutHeight
decodeCutHeight = CutHeight <$> getWord64le

-- | Encodings for data exchange use little endian by default. Big endian
-- encodings are provided for use in internal storage when a bytewise
-- lexicographcial ordering is required.
--
encodeCutHeightBe :: MonadPut m => CutHeight -> m ()
encodeCutHeightBe (CutHeight r) = putWord64be r

-- | Encodings for data exchange use little endian by default. Big endian
-- encodings are provided for use in internal storage when a bytewise
-- lexicographcial ordering is required.
--
decodeCutHeightBe :: MonadGet m => m CutHeight
decodeCutHeightBe = CutHeight <$> getWord64be

