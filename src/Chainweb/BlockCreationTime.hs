{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
-- TODO
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
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable

import GHC.Generics

import Numeric.AffineSpace

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Time

-- -------------------------------------------------------------------------- --
-- Block Creation Time

newtype BlockCreationTime = BlockCreationTime { _bct :: (Time Micros) }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

instance IsMerkleLogEntry ChainwebHashTag BlockCreationTime where
    type Tag BlockCreationTime = 'BlockCreationTimeTag
    toMerkleNode = encodeMerkleInputNode encodeBlockCreationTime
    fromMerkleNode = decodeMerkleInputNode decodeBlockCreationTime
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeBlockCreationTime :: MonadPut m => BlockCreationTime -> m ()
encodeBlockCreationTime (BlockCreationTime t) = encodeTime t

decodeBlockCreationTime :: MonadGet m => m BlockCreationTime
decodeBlockCreationTime = BlockCreationTime <$> decodeTime
