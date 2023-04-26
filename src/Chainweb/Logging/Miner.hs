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
module Chainweb.Logging.Miner
  ( NewMinedBlock (..),
    OrphanedBlock (..),
  )
where

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Time
import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data NewMinedBlock = NewMinedBlock
  { _minedBlockHeader :: !(ObjectEncoded BlockHeader),
    _minedBlockTrans :: {-# UNPACK #-} !Word,
    -- | Bytes
    _minedBlockSize :: {-# UNPACK #-} !Word,
    _minedBlockMiner :: !Text,
    _minedBlockDiscoveredAt :: !(Time Micros)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)

data OrphanedBlock = OrphanedBlock
  { _orphanedHeader :: !(ObjectEncoded BlockHeader),
    _orphanedBestOnCut :: !(ObjectEncoded BlockHeader),
    _orphanedDiscoveredAt :: !(Time Micros),
    _orphanedMiner :: !Text,
    _orphanedReason :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)
