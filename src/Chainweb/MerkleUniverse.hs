{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.MerkleUniverse
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.MerkleUniverse
( ChainwebHashTag(..)
) where

import Control.Monad.Catch

import Crypto.Hash.Algorithms

import Data.Void

-- internal modules

import Chainweb.Crypto.MerkleLog


data ChainwebHashTag
    = VoidTag
    | MerkleRootTag
    | ChainIdTag
    | BlockHeightTag
    | BlockWeightTag
    | BlockPayloadHashTag
    | BlockNonceTag
    | BlockCreationTimeTag
    | ChainwebVersionTag
    | PowHashTag
    | BlockHashTag
    | HashTargetTag
    | TransactionTag
    | TransactionOutputTag
    | BlockTransactionsHashTag
    | BlockOutputsHashTag
    | MinerDataTag
    | CoinbaseOutputTag
    | EpochStartTimeTag
    | FeatureFlagsTag
    deriving (Show, Eq)

instance MerkleUniverse ChainwebHashTag where
    type HashAlg ChainwebHashTag = SHA512t_256
    type MerkleTagVal ChainwebHashTag 'VoidTag = 0x0000
    type MerkleTagVal ChainwebHashTag 'MerkleRootTag = 0x0001
    type MerkleTagVal ChainwebHashTag 'ChainIdTag = 0x0002
    type MerkleTagVal ChainwebHashTag 'BlockHeightTag = 0x0003
    type MerkleTagVal ChainwebHashTag 'BlockWeightTag = 0x0004
    type MerkleTagVal ChainwebHashTag 'BlockPayloadHashTag = 0x0005
    type MerkleTagVal ChainwebHashTag 'BlockNonceTag = 0x0006
    type MerkleTagVal ChainwebHashTag 'BlockCreationTimeTag = 0x0007
    type MerkleTagVal ChainwebHashTag 'ChainwebVersionTag = 0x0008
    type MerkleTagVal ChainwebHashTag 'PowHashTag = 0x0009
    type MerkleTagVal ChainwebHashTag 'BlockHashTag = 0x0010
    type MerkleTagVal ChainwebHashTag 'HashTargetTag = 0x0011
    type MerkleTagVal ChainwebHashTag 'TransactionTag = 0x0013
    type MerkleTagVal ChainwebHashTag 'TransactionOutputTag = 0x0014
    type MerkleTagVal ChainwebHashTag 'BlockTransactionsHashTag = 0x0015
    type MerkleTagVal ChainwebHashTag 'BlockOutputsHashTag = 0x0016
    type MerkleTagVal ChainwebHashTag 'MinerDataTag = 0x0017
    type MerkleTagVal ChainwebHashTag 'CoinbaseOutputTag = 0x0018
    type MerkleTagVal ChainwebHashTag 'EpochStartTimeTag = 0x0019
    type MerkleTagVal ChainwebHashTag 'FeatureFlagsTag = 0x0020

instance IsMerkleLogEntry ChainwebHashTag Void where
    type Tag Void = 'VoidTag
    toMerkleNode = \case
    fromMerkleNode _ = throwM
        $ MerkleLogDecodeException "can't deserialize value of type Void"
