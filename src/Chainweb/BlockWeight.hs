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
-- Module: Chainweb.BlockWeight
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The weight of a block. This is the accumulated difficulty of all predecessors
-- of a block including the block itself.
--
module Chainweb.BlockWeight
(
-- * Block Weight
  BlockWeight(..)
, encodeBlockWeight
, decodeBlockWeight
, encodeBlockWeightBe
, decodeBlockWeightBe
) where

import Control.DeepSeq

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable

import GHC.Generics (Generic)

-- Internal imports

import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.MerkleUniverse

import Numeric.Additive

-- -------------------------------------------------------------------------- --
-- Block Weight
--
-- This is the accumulated Hash difficulty
--
newtype BlockWeight = BlockWeight HashDifficulty
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Hashable
        , ToJSON, FromJSON, ToJSONKey, FromJSONKey
        , AdditiveSemigroup, AdditiveAbelianSemigroup
        , Num
        )

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BlockWeight where
    type Tag BlockWeight = 'BlockWeightTag
    toMerkleNode = encodeMerkleInputNode encodeBlockWeight
    fromMerkleNode = decodeMerkleInputNode decodeBlockWeight
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockWeight :: MonadPut m => BlockWeight -> m ()
encodeBlockWeight (BlockWeight w) = encodeHashDifficulty w
{-# INLINE encodeBlockWeight #-}

decodeBlockWeight :: MonadGet m => m BlockWeight
decodeBlockWeight = BlockWeight <$> decodeHashDifficulty
{-# INLINE decodeBlockWeight #-}

encodeBlockWeightBe :: MonadPut m => BlockWeight -> m ()
encodeBlockWeightBe (BlockWeight w) = encodeHashDifficultyBe w
{-# INLINE encodeBlockWeightBe #-}

decodeBlockWeightBe :: MonadGet m => m BlockWeight
decodeBlockWeightBe = BlockWeight <$> decodeHashDifficultyBe
{-# INLINE decodeBlockWeightBe #-}

