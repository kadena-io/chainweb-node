{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Logging.Miner
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Logging.Miner
( NewMinedBlock(..)
, OrphanedBlock(..)
) where

import Control.DeepSeq

import Data.Aeson
import Data.Text (Text)

import GHC.Generics

import Numeric.Natural

-- internal modules

import Chainweb.BlockHeader
import Chainweb.Time

data NewMinedBlock = NewMinedBlock
    { _minedBlockHeader :: !(ObjectEncoded BlockHeader)
    , _minedBlockTrans :: {-# UNPACK #-} !Word
    , _minedBlockSize :: {-# UNPACK #-} !Word   -- ^ Bytes
    , _minedHashAttempts :: !Natural
    , _minedBlockMiner :: !Text
    , _minedBlockDiscoveredAt :: !(Time Micros)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)

data OrphanedBlock = OrphanedBlock
    { _orphanedHeader :: !(ObjectEncoded BlockHeader)
    , _orphanedMiner :: !Text
    , _orphanedReason :: !Text
    , _orphanedCode :: !Int }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, NFData)
