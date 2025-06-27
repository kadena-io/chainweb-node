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

-- * MStu
, MStu(..)
, divideMStu

-- * GStu
, GStu(..)
, divideGStu

-- * KDA
, Kda
, pattern Kda
, _kda
, stuToKda
, kdaToStu
, mstuToKda
, kdaToMStu
, gstuToKda
, kdaToGStu

-- * Miner Reward
, MinerReward(..)
, minerRewardKda
, minerRewardGStu
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
-- Stu

-- | Stu: 1 KDA == 1e18 Stu.
--
-- This is the smallest unit of KDA throughout the Chainweb ecosystem.
--
-- It is used for balances in native X-Channels.
--
-- It is also used to represent balances and transfered amount of native KDA in
-- the EVM.
--
-- Values are non-negative and substraction can result in an arithmetic
-- underflow.
--
newtype Stu = Stu { _stu :: Natural }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData)

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
-- Mega Stu

-- | Mega Stu: 1 KDA == 1e12 MStu.
--
-- This is smallest unit of KDA on Pact Chains.
--
-- Values are non-negative and substraction can result in an arithmetic
-- underflow.
--
newtype MStu = MStu { _mstu :: Natural }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData)

instance HasTextRepresentation MStu where
    toText = toText . _mstu
    fromText = fmap MStu . fromText
    {-# INLINEABLE toText #-}
    {-# INLINEABLE fromText #-}

instance ToJSON MStu where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINEABLE toJSON #-}
    {-# INLINEABLE toEncoding #-}

instance FromJSON MStu where
    parseJSON = parseJsonFromText "MStu"
    {-# INLINABLE parseJSON #-}

-- | Divide a MStu by a Natural number.
--
-- The result is rounded using bankers rounding.
--
divideMStu :: MStu -> Natural -> MStu
divideMStu s n = round $ s % fromIntegral n

-- -------------------------------------------------------------------------- --
-- Giga Stu

-- | Giga Stu: 1 KDA == 1e9 gstu.
--
-- This is used to represent gas and withdrawals in the EVM. In particular, it
-- is used for mining rewards in the EVM.
--
-- Values are non-negative and substraction can result in an arithmetic
-- underflow.
--
newtype GStu = GStu { _gstu :: Natural }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData)

instance HasTextRepresentation GStu where
    toText = toText . _gstu
    fromText = fmap GStu . fromText
    {-# INLINEABLE toText #-}
    {-# INLINEABLE fromText #-}

instance ToJSON GStu where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINEABLE toJSON #-}
    {-# INLINEABLE toEncoding #-}

instance FromJSON GStu where
    parseJSON = parseJsonFromText "GStu"
    {-# INLINABLE parseJSON #-}

-- | Divide a GStu by a Natural number.
--
-- The result is rounded using bankers rounding.
--
divideGStu :: GStu -> Natural -> GStu
divideGStu s n = round $ s % fromIntegral n

-- -------------------------------------------------------------------------- --
-- KDA

-- | KDA encoded as Decimal.
--
-- No arithmetic conversions or operations are provided.
--
-- The available precision of KDA values depends on the payload provider
-- context.
--
-- The maximum precision across the Chainweb ecosystem is 1e18 decimal digits
-- (Stu). The minimum precision that payload providers must suppport is 1e9
-- (GStu).
--
-- Native X-channels use KDA with a precision of 1e18 (Stu). Fractional amounts
-- that are smaller than the precision available at the receiver can not be
-- withdrawn and remain in the channel.
--
-- Mining rewards are calcuated in KDA with a precision of 1e9 (GStu).
--
-- The value is stored in a normalized format with the smallest possible
-- mantissa.
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

-- -----------------------------------------------------------------------------
-- Conversions

stuToKda :: HasCallStack => Stu -> Kda
stuToKda (Stu k) = Kda $ normalizeDecimal $ Decimal 18 (fromIntegral k)

kdaToStu :: Kda -> Stu
kdaToStu (Kda { _kda = s }) = Stu $ round (s * 1e18)

mstuToKda :: MStu -> Kda
mstuToKda (MStu k) = Kda $ normalizeDecimal $ Decimal 12 (fromIntegral k)

kdaToMStu :: Kda -> MStu
kdaToMStu (Kda { _kda = s }) = MStu $ round (s * 1e12)

gstuToKda :: GStu -> Kda
gstuToKda (GStu k) = Kda $ normalizeDecimal $ Decimal 9 (fromIntegral k)

kdaToGStu :: Kda -> GStu
kdaToGStu (Kda { _kda = s }) = GStu $ round (s * 1e9)

-- -------------------------------------------------------------------------- --
-- Miner Reward

--  | Miner Reward in GStu
--
--  The maximum miner reward is 23045230000, which is smaller than 2^51-1.
--  Miner rewards can thus be represented losslessly as JSON numbers.
--
newtype MinerReward = MinerReward { _minerReward :: GStu }
    deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON) via JsonTextRepresentation "MinerReward" MinerReward

instance HasTextRepresentation MinerReward where
    toText (MinerReward (GStu n)) = toText n
    fromText t = MinerReward . GStu <$> fromText t
    {-# INLINE toText #-}
    {-# INLINE fromText #-}


minerRewardKda :: MinerReward -> Kda
minerRewardKda (MinerReward d) = gstuToKda d

minerRewardGStu :: MinerReward -> GStu
minerRewardGStu (MinerReward d) = d

-- | Calculate miner reward for a block at the given height.
--
-- NOTE:
-- This used to compute the value as @roundTo 8 $ (_kda $ stuToKda m) / n@.
-- The new caclulcation based on GStu is equivalent for 10 and 20 chains,
-- except for the pre-last entry in the miner rewards table, namely
-- @(125538056,0.023999333). However, since this value has not yet been used
-- in any network, we can still change the algorithm.
--
-- For other graphs that do not cleanly divide 10000, this value is does not
-- hold and changes in precision constitue a forking change.
--
blockMinerReward
    :: HasVersion
    => BlockHeight
    -> MinerReward
blockMinerReward h = case M.lookupGE h minerRewards of
    Nothing -> MinerReward $ GStu 0
    Just (_, s) -> MinerReward $ divideGStu s n
  where
    !n = int . order $ chainGraphAt h

-- | Binary encoding of mining rewards as unsigned integral number in little
-- endian encoding.
--
-- The maximum miner reward is 23045230000. The miner reward can therefore be
-- encoded in as Word64 value.
--
encodeMinerReward :: MinerReward -> Put
encodeMinerReward (MinerReward (GStu n)) = putWord64le (int n)
{-# INLINE encodeMinerReward #-}

decodeMinerReward :: Get MinerReward
decodeMinerReward = MinerReward . int <$>  getWord64le
{-# INLINE decodeMinerReward #-}

-- -------------------------------------------------------------------------- --
-- Internal
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Miner Rewards Table

type MinerRewardsTable = M.Map BlockHeight GStu

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
    formatRow :: (Word64, CsvDecimal) -> (BlockHeight, GStu)
    formatRow (a, b) = (BlockHeight $ int a, kdaToGStu (Kda $ _csvDecimal b))

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
expectedMinerRewardsHash = read "264189d9d9181d76e2be948e3b336ab09b723cc095c059b6b3c79ff4e58b5c412c5d0b01882464a4f27fbe297b3f0dcbe3bda6ee6eb85556eeba6224ecb6458e"

expectedRawMinerRewardsHash :: Digest SHA512
expectedRawMinerRewardsHash = read "903d10b06666c0d619c8a28c74c3bb0af47209002f005b12bbda7b7df1131b2072ce758c1a8148facb1506022215ea201629f38863feb285c7e66f5965498fe0"
