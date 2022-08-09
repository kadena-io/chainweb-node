{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockCreationTime
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- The creation time of a block. The creation time of a block must be strictly
-- larger than the creation time of the parent and the adjacent parents.
-- Chainweb node ignore blocks that have a creation time that is in the future
-- relatively the real world clock of the respective node.
--
module Chainweb.BlockCreationTime
(
-- * BlockCreationTime
  BlockCreationTime(..)
, encodeBlockCreationTime
, decodeBlockCreationTime
) where

import Control.DeepSeq

import Data.Aeson
import Data.Hashable

import GHC.Generics

import Numeric.AffineSpace

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Time
import Chainweb.Utils.Serialization

-- -------------------------------------------------------------------------- --
-- Block Creation Time

newtype BlockCreationTime = BlockCreationTime { _bct :: (Time Micros) }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag BlockCreationTime where
    type Tag BlockCreationTime = 'BlockCreationTimeTag
    toMerkleNode = encodeMerkleInputNode encodeBlockCreationTime
    fromMerkleNode = decodeMerkleInputNode decodeBlockCreationTime
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockCreationTime :: BlockCreationTime -> Put
encodeBlockCreationTime (BlockCreationTime t) = encodeTime t

decodeBlockCreationTime :: Get BlockCreationTime
decodeBlockCreationTime = BlockCreationTime <$> decodeTime
