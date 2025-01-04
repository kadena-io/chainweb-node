{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module: Chainweb.MinerReward
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Miner reward.
--
-- Morally this is a property of the Chainweb version, however there is no need
-- to use value different from what is used on Mainnet on any network.
--
module Chainweb.MinerReward
(
-- * STU
  Stu(..)
, divideStu

-- * KDA
, Kda
, pattern Kda
, _kda
, stuToKda
, kdaToStu

-- * Miner Reward
, MinerReward(..)
, minerRewardKda
, blockMinerReward
, encodeMinerReward
, decodeMinerReward

-- * Internal
-- ** Miner Rewards Table
, minerRewards
, mkMinerRewards

-- ** Miner Rewards File
, rawMinerRewards

-- ** Consistency Checks
, rawMinerRewardsHash
, minerRewardsHash
, expectedMinerRewardsHash
, expectedRawMinerRewardsHash
) where

import Chainweb.BlockHeight (BlockHeight(..), encodeBlockHeight)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Control.DeepSeq (NFData)
import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms (SHA512)
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as CSV
import Data.Decimal
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Ratio
import Data.Vector qualified as V
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack
import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- STU

-- | Smallest Unit of KDA: 1 KDA == 1e12 STU.
--
-- Values are non-negative and substraction can result in an arithmetic
-- underflow.
--
newtype Stu = Stu { _stu :: Natural }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Enum, Num, Real, Integral, NFData)

instance HasTextRepresentation Stu where
    toText = toText . _stu
    fromText = fmap Stu . fromText
    {-# INLINEABLE toText #-}
    {-# INLINEABLE fromText #-}

instance ToJSON Stu where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINEABLE toJSON #-}
    {-# INLINEABLE toEncoding #-}

instance FromJSON Stu where
    parseJSON = parseJsonFromText "Stu"
    {-# INLINABLE parseJSON #-}

-- | Divide a Stu by a Natural number.
--
-- The result is rounded using bankers rounding.
--
divideStu :: Stu -> Natural -> Stu
divideStu s n = round $ s % fromIntegral n

-- -------------------------------------------------------------------------- --
-- KDA

-- | KDA encoded as Decimal.
--
-- No arithmetic conversions or operations are provided.
--
-- The precision of KDA values is 1e12 decimal digits. The value is stored in
-- a normalized format with the smallest possible mantissa.
--
newtype Kda = Kda_ Decimal
    deriving stock (Show, Eq, Ord, Generic)

-- | Smart constructor for KDA. It is an error if the Decimal has more than
-- twelve decimal digits.
--
pattern Kda :: HasCallStack => Decimal -> Kda
pattern Kda { _kda } <- Kda_ _kda where
    Kda k
        | roundTo 12 k /= k = error "KDA value with a precision of more than 12 decimal digits"
        | otherwise = Kda_ $ normalizeDecimal k
{-# COMPLETE Kda #-}

stuToKda :: HasCallStack => Stu -> Kda
stuToKda (Stu k) = Kda $ normalizeDecimal $ Decimal 12 (fromIntegral k)

kdaToStu :: Kda -> Stu
kdaToStu (Kda { _kda = s }) = Stu $ round (s * 1e12)

-- -------------------------------------------------------------------------- --
-- Miner Reward

--  | Miner Reward in Stu
--
--  The maximum miner reward is 23045230000000, which is smaller than 2^51-1.
--  Miner rewards can thus be represented losslessly as JSON numbers.
--
newtype MinerReward = MinerReward { _minerReward :: Stu }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via JsonTextRepresentation "MinerReward" MinerReward

instance HasTextRepresentation MinerReward where
    toText (MinerReward (Stu n)) = toText n
    fromText t = MinerReward . Stu <$> fromText t
    {-# INLINE toText #-}
    {-# INLINE fromText #-}


minerRewardKda :: MinerReward -> Kda
minerRewardKda (MinerReward d) = stuToKda d

-- | Calculate miner reward for a block at the given height.
--
-- NOTE:
-- This used to compute the value as @roundTo 8 $ (_kda $ stuToKda m) / n@.
-- The new caclulcation based on Stu is equivalent for 10 and 20 chains,
-- except for the pre-last entry in the miner rewards table, namely
-- @(125538056,0.023999333). However, since this value hasen't yet been used
-- in any network, we can still change the algorithm.
--
blockMinerReward
    :: ChainwebVersion
    -> BlockHeight
    -> MinerReward
blockMinerReward v h = case M.lookupGE h minerRewards of
    Nothing -> MinerReward $ Stu 0
    Just (_, s) -> MinerReward $ divideStu s n
  where
    !n = int . order $ chainGraphAt v h

-- | Binary encoding of mining rewards as unsigned integral number in little
-- endian encoding.
--
-- The maximum miner reward is 23045230000000. The miner reward can therefore be
-- encoded in as Word64 value.
--
encodeMinerReward :: MinerReward -> Put
encodeMinerReward (MinerReward (Stu n)) = putWord64le (int n)
{-# INLINE encodeMinerReward #-}

decodeMinerReward :: Get MinerReward
decodeMinerReward = MinerReward . int <$>  getWord64le
{-# INLINE decodeMinerReward #-}

-- -------------------------------------------------------------------------- --
-- Internal
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Miner Rewards Table

type MinerRewardsTable = M.Map BlockHeight Stu

-- | Rewards table mapping 3-month periods to their rewards according to the
-- calculated exponential decay over about a 120 year period (125538057 block
-- heights).
--
-- It provides the total reward per block height accross all chains. Use the
-- 'blockMinerReward' function to obtain the reward for a single block at a
-- given block height.
--
-- Morally this is a property of the Chainweb version, however there is no need
-- to use value different from what is used on Mainnet on any network.
--
-- Mining rewards are between 0 and 24 KDA. Values decrease monotonically over
-- 125538057 block heights (about 120 years).
--
minerRewards :: MinerRewardsTable
minerRewards = mkMinerRewards
{-# NOINLINE minerRewards #-}

-- | Compute the miner rewards table.
--
-- The indirection from 'minerReward' to 'mkMinerReward' is required because the
-- HasCallStack constraints prevents this value from being a CAF that gets
-- cached.
--
mkMinerRewards :: HasCallStack => MinerRewardsTable
mkMinerRewards =
    case CSV.decode CSV.NoHeader (BL.fromStrict rawMinerRewards) of
        Left e -> error
            $ "cannot construct miner rewards table: " <> sshow e
        Right vs ->
            let rewards = M.fromList . V.toList . V.map formatRow $ vs
            in if minerRewardsHash rewards == expectedMinerRewardsHash
                then rewards
                else error $ "hash of miner rewards table does not match expected hash"
  where
    formatRow :: (Word64, CsvDecimal) -> (BlockHeight, Stu)
    formatRow (a, b) = (BlockHeight $ int a, kdaToStu (Kda $ _csvDecimal b))

-- -------------------------------------------------------------------------- --
-- Miner Rewards File

-- | Read in the reward csv via TH for deployment purposes.
--
-- Rewards are encoded in KDA with a precision of up to nine decimal digits.
--
rawMinerRewards :: HasCallStack => B.ByteString
rawMinerRewards
    | rawMinerRewardsHash rawBytes == expectedRawMinerRewardsHash = rawBytes
    | otherwise = error "hash of raw miner rewards file does not match expected value."
  where
    rawBytes = $(embedFile "rewards/miner_rewards.csv")

-- --------------------------------------------------------------------------
-- Consistency Checks

rawMinerRewardsHash :: B.ByteString -> Digest SHA512
rawMinerRewardsHash = hash

minerRewardsHash :: MinerRewardsTable -> Digest SHA512
minerRewardsHash = hash
    . runPutS
    . traverse_ (\(k,v) -> encodeBlockHeight k >> putWord64le (fromIntegral v))
    . M.toAscList

expectedMinerRewardsHash :: Digest SHA512
expectedMinerRewardsHash = read "8e4fb006c5045b3baab638d16d62c952e4981a4ba473ec63620dfb54093d5104abd0be1a62ce52113575d598881fb57e84a41ec5c617e4348e270b9eacd300c9"

expectedRawMinerRewardsHash :: Digest SHA512
expectedRawMinerRewardsHash = read "903d10b06666c0d619c8a28c74c3bb0af47209002f005b12bbda7b7df1131b2072ce758c1a8148facb1506022215ea201629f38863feb285c7e66f5965498fe0"

