{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.ForkState
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.ForkState
(
-- * Fork State
  ForkState(..)
, encodeForkState
, decodeForkState

-- * Fork Number
, ForkNumber(..)
, forkNumber

-- * Fork Votes
, ForkVotes(..)
, forkVotes

-- * Vote Count Logic
, forkEpochLength
, voteStep
, addVote
, resetVotes
, countVotes
, decideVotes
, voteCountLength
) where

import Data.Word
import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils
import Chainweb.Utils.Serialization (Put, Get, putWord64le, getWord64le)
import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)
import Data.Aeson (ToJSON, FromJSON)
import Data.Bits
import Data.Hashable (Hashable)
import Data.Ratio
import GHC.Generics (Generic)
import Numeric.Additive (AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid)
import Numeric.Natural

-- | Starting with Chainweb 2.33, forks are identified by a monotonically
-- increasing fork number. Each block is created according to a specific fork
-- number.
--
-- Each fork number is associated with a specific block height at which it
-- becomes active. This number is also called the "fork height" for the
-- respective fork. Forks are either activated on a per chain or a per network
-- basis. The former are triggered as soon as the specific chain reaches the
-- block height for the respective fork. The latter are triggered when all
-- chains in the network have reached the respective block height.
--
-- The Fork height of a fork is determined by miners, usually via upgrading to
-- a new software version that includes the fork activation logic, but it is
-- also possible that a new software version allows miners to configure whether
-- they want to support a specific fork or not.
--
-- For the purpose of fork activation the feature flags field in the block
-- header is renamed into the "fork state" field.
--
-- The support for a fork by a miner is signaled by increasing the vote count
-- for the fork in a newly mined block by one. Votes are accumulated over
-- 120 epochs (5 days), which is called a "fork epoch". I.e. a fork epoch
-- consists of 12 * 120 block heights.
--
-- A fork is activated in an epoch when at least 2/3 of all blocks for the
-- previous 120 epochs have signaled for the respective fork number. Once a fork
-- is activated it can not be deactivated again. The fork number is
-- monontonically increasing.
--
-- A block that is signalling a fork number that is larger than the current fork
-- number plus one is considered invalid. In other words forks can only be
-- activated successively, one at a time. There will always be at least 120
-- epochs between the activation of two successive forks.
--
-- Starting with chainweb-node version 2.33, a node must accept blocks that have
-- a fork number equal to the current fork number and a fork vote count that is
-- either the fork vote count of the previous block or that number plus one. It
-- must reject all other blocks. The fork number at the time that chainweb node
-- version 2.33 is released is zero. That version does not increase the vote
-- count, so the vote count will be zero as long as no other version is
-- released that proposes a fork.
--
newtype ForkState = ForkState { _forkState :: Word64 }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON)

-- | Little endian encoding of the fork state.
--
encodeForkState :: ForkState -> Put
encodeForkState (ForkState h) = putWord64le h

-- | Little endian encoding of the fork state
--
decodeForkState :: Get ForkState
decodeForkState = ForkState <$> getWord64le

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ForkState where
    type Tag ForkState = 'ForkStateTag
    toMerkleNode = encodeMerkleInputNode encodeForkState
    fromMerkleNode = decodeMerkleInputNode decodeForkState

-- ---------------------------------------------------------------------------
-- Fork Number

newtype ForkNumber = ForkNumber { _getForkNumber :: Word32 }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Show, Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum, Bounded
        )

_forkNumber :: ForkState -> ForkNumber
_forkNumber (ForkState w) = int $ w .&. 0xFFFFFFFF

forkNumber :: Lens' ForkState ForkNumber
forkNumber = lens _forkNumber $ \(ForkState w) v -> ForkState
    $ (w .&. 0xFFFFFFFF00000000)
    .|. (fromIntegral v .&. 0xFFFFFFFF)

-- ---------------------------------------------------------------------------
-- Fork Votes

newtype ForkVotes = ForkVotes { _getForkVotes :: Word32 }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype
        ( Show, Hashable, ToJSON, FromJSON
        , AdditiveSemigroup, AdditiveAbelianSemigroup, AdditiveMonoid
        , Num, Integral, Real, Enum, Bounded
        )

_forkVotes :: ForkState -> ForkVotes
_forkVotes (ForkState w) = int $ (w `shiftR` 32) .&. 0xFFFFFFFF

forkVotes :: Lens' ForkState ForkVotes
forkVotes = lens _forkVotes $ \(ForkState w) v -> ForkState
    $ (w .&. 0x00000000FFFFFFFF)
    .|. ((fromIntegral v .&. 0xFFFFFFFF) `shiftL` 32)

-- -------------------------------------------------------------------------- --
-- -- Vote Count Logic

-- | A epoch is 120 * 120 block heights, which, on mainne, is expected to be 5
-- days.
--
-- Each fork epoch is divided into:
--
-- * 120 * 119 blocks for voting and
-- * 120 blocks for counting the votes
--
forkEpochLength :: Natural
forkEpochLength = 120 * 120 -- 5 days

-- | The last 120 blocks in a fork epoch are used to count the votes.
--
voteCountLength :: Natural
voteCountLength = 120

-- | The vote count is quantized into 1000 levels.
--
voteStep :: ForkVotes
voteStep = 1000

addVote :: ForkVotes -> ForkVotes
addVote v = v + voteStep

resetVotes :: ForkVotes
resetVotes = ForkVotes 0

-- | When counting fork votes, the vote count is the average of the votes of the
-- adjacent parents and the parent. In order to guarantee convergence, it is
-- imperative that rounding of the finally result is biased. The final result is
-- rounded using bankers rounding.
--
countVotes :: Traversable t => t ForkVotes -> ForkVotes
countVotes votes = sum votes `quot` ForkVotes (int $ length votes)

decideVotes :: ForkVotes -> Bool
decideVotes v = round (v % voteStep) * 3 >= (forkEpochLength - voteCountLength) * 2


-- -------------------------------------------------------------------------- --
-- Details
--
-- Validation:
--
-- validateForkNumber :: Parent BlockHeader -> BlockHeader -> Bool
-- validateForkNumber (Parent parent) hdr
--     | isForkEpoch hdr =
--         if view getForkNumberVotes parent >= ((forkEpochLength * 2) `div` 3)
--           then
--             view getForkNumber hdr == view getForkNumber parent + 1
--             && view getForkNumberVotes hdr <= 1
--           else
--             view getForkNumber hdr == view getForkNumber parent
--             && view getForkNumberVotes hdr <= 1
--     | otherwise =
--         view getForkNumber hdr == view getForkNumber (parent hdr)
--         && view getForkNumberVotes hdr <= view getForkNumberVotes (parent hdr) + 1
--
-- Votes:
--
-- * Voting is done per chain over a fork epoch mines a DA epoch.
-- * The last epoch in a fork epoch no voting is done. Instead the votes are
--   aggregated over all chains, by computing the global average:
--
--   * Use a quantized average consensus algorithm to compute the average vote.
--   * Use 1000 quantization levels.
--   * In each step use integer division with biased rounding.
--   * At the end roude to nearest integer using bankers rounding.
--
