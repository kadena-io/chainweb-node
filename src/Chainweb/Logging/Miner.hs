{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Chainweb.Logging.Miner
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Datatypes for logging mined blocks.
--
module Chainweb.Logging.Miner
  ( NewMinedBlock(..)
  , OrphanedBlock(..)
  ) where

import Control.DeepSeq

import Data.Aeson
import Data.Text (Text)

import GHC.Generics

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Time
import Chainweb.MinerReward
import Numeric.Natural
import Chainweb.Parent
import Chainweb.BlockHash

data NewMinedBlock = NewMinedBlock
    { _minedBlockHeader :: !(ObjectEncoded BlockHeader)
    , _minedBlockTrans :: !Natural
    , _minedBlockSize :: !Natural
    , _minedBlockOutputSize :: !Natural
    , _minedBlockFees :: !Stu
    , _minedBlockDiscoveredAt :: !(Time Micros)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)

data OrphanedBlock = OrphanedBlock
    { _orphanedParent :: !(Parent BlockHash)
    , _orphanedPayloadHash :: !BlockPayloadHash
    , _orphanedDiscoveredAt :: !(Time Micros)
    , _orphanedReason :: !Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)
