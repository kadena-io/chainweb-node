{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.ForkNumber
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.ForkState
(
) where

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
-- either the fork number of the block height of the block or the respetive fork
-- number plus one. It must reject all other blocks. The fork number at the time
-- that chainweb node version 2.33 is released is zero.
--
newtype ForkState = ForkState { _forkState :: Word64 }
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData)

_forkNumber' :: ForkState -> Word32
_forkNumber' (ForkState w) = int $ w .&. 0xFFFFFFFF

_forkVotes' :: ForkState -> Word32
_forkVotes' (ForkState w) = int $ (w `shiftR` 32) .&. 0xFFFFFFFF

forkNumber' :: Lens' ForkState Word32
forkNumber' = lens _forkNumber $ \(ForkState w) v -> ForkState
    $ (w .&. 0xFFFFFFFF00000000)
    .|. (fromIntegral v .&. 0xFFFFFFFF)

forkVotes' :: Lens' ForkState Word32
forkVotes' = lens _forkVotes $ \(ForkState w) v -> ForkState
    $ (w .&. 0x00000000FFFFFFFF)
    .|. ((fromIntegral v .&. 0xFFFFFFFF) `shiftL` 32)

-- -------------------------------------------------------------------------- --
-- Fork State Validation in Block Headers

_forkNumber :: BlockHeader -> Word32
_forkNumber = view (forkState . forkNumber')

_forkVotes :: BlockHeader -> Word32
_forkVotes = view (forkState . forkVotes')

forkNumber :: Lens' BlockHeader Word32
forkNumber = forkState . forkNumber'

forkVotes :: Lens' BlockHeader Word32
forkVotes = forkState . forkVotes'

forkEpochLength :: Natural
forkEpochLength = 120 * 120 -- 5 days

isForkEpoch :: BlockHeader -> Bool
isForkEpoch hdr = mod (view blockHeight hdr) forkEpochLength == 0

getForkNumber :: BlockHeader -> Word32
getForkNumber = view signaledForkNumber . view forkState

getForkNumberVotes :: BlockHeader -> Word32
getForkNumberVotes = view signaledNumber . view forkState

validateForkNumber :: Parent BlockHeader -> BlockHeader -> Bool
validateForkNumber (Parent parent) hdr
    | isForkEpoch hdr =
        if view getForkNumberVotes parent >= (forkEpochLength * 2 `div 3)
          then
            view getForkNumber hdr == view getForkNumber parent + 1
            && view getForkNumberVotes hdr <= 1
          else
            view getForkNumber hdr == view getForkNumber parent
            && view getForkNumberVotes hdr <= 1
    | otherwise =
        view getForkNumber hdr == view getForkNumber (parent hdr)
        && view getForkNumberVotes hdr <= view getForkNumberVotes (parent hdr) + 1

