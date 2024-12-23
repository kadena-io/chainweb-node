{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.Test.MinerReward
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.MinerReward
( tests
) where

import Chainweb.BlockHeight
import Chainweb.MinerReward
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Mainnet

import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as CSV
import Data.Decimal
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Data.Word

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary Stu where
    arbitrary = Stu <$> arbitrary

instance Arbitrary Kda where
    arbitrary = fmap Kda $ Decimal <$> choose (0,12) <*> arbitrary

newtype PositiveKda = PositiveKda { _positive :: Kda }
    deriving (Show, Eq, Ord)

instance Arbitrary PositiveKda where
    arbitrary = fmap (PositiveKda . Kda) $ Decimal
        <$> choose (0,12)
        <*> (getNonNegative <$> arbitrary)

tests :: TestTree
tests = testGroup "MinerReward"
    [ testProperty "kdaToStuToKda" prop_kdaToStuToKda
    , testProperty "stuToKdaToStu" prop_stuToKdaToStu
    , testCase "finalReward" test_finalMinerReward
    , testCase "minerRewardsMax" test_minerRewardsMax
    , testCase "minerRewardsFitWord64" test_minerRewardsFitWord64
    , testCase "expectedMinerRewardsHash" test_expectedMinerRewardsHash
    , testCase "expectedRawMinerRewardsHash" test_expectedRawMinerRewardsHash
    , testCase "assert blockMinerRewardLegacyCompact" test_blockMinerRewardLegacyCompat
    , testProperty "blockMinerRewardLegacyCompat" prop_blockMinerRewardLegacyCompat
    ]

-- --------------------------------------------------------------------------
-- Properties and Assertions

maxRewardHeight :: BlockHeight
maxRewardHeight = 125538057

prop_kdaToStuToKda :: PositiveKda -> Property
prop_kdaToStuToKda (PositiveKda kda) = stuToKda (kdaToStu kda) === kda

prop_stuToKdaToStu :: Stu -> Property
prop_stuToKdaToStu stu = kdaToStu (stuToKda stu) === stu

prop_blockMinerRewardLegacyCompat :: BlockHeight -> Property
prop_blockMinerRewardLegacyCompat h
    | h < maxRewardHeight - 2 =
        legacyBlockMinerReward v h === minerRewardKda (blockMinerReward v h)
    | h == maxRewardHeight - 1 =
        legacyBlockMinerReward v h =/= minerRewardKda (blockMinerReward v h)
    | h == maxRewardHeight =
        legacyBlockMinerReward v h === minerRewardKda (blockMinerReward v h)
    | otherwise = expectFailure
        -- legacyMinerRewards is expected to throw an exception
        $ legacyBlockMinerReward v h === minerRewardKda (blockMinerReward v h)

  where
    v = Mainnet01

-- 2.304523
--
test_finalMinerReward :: Assertion
test_finalMinerReward = do
    mapM_ rewardIsZero $ take 100 [maxRewardHeight..]
    mapM_ rewardIsZero $ take 10 [maxRewardHeight, (maxRewardHeight + 1000)..]
  where
    rewardIsZero h = assertEqual
        "The final miner reward is 0"
        (Kda 0)
        (minerRewardKda (blockMinerReward Mainnet01 h))

test_minerRewardsMax :: Assertion
test_minerRewardsMax = assertBool
    "maximum miner reward is smaller than 1e12 * 24"
    (_stu (maximum minerRewards) < 1e12 * 24)

test_minerRewardsFitWord64 :: Assertion
test_minerRewardsFitWord64 = assertBool
    "maximum miner reward fits into Word64"
    (_stu (maximum minerRewards) <= fromIntegral (maxBound @Word64))

test_expectedMinerRewardsHash :: Assertion
test_expectedMinerRewardsHash = assertEqual
    "expected miner rewards hash"
    expectedMinerRewardsHash
    (minerRewardsHash minerRewards)

test_expectedRawMinerRewardsHash :: Assertion
test_expectedRawMinerRewardsHash = assertEqual
    "expected raw miner rewards hash"
    expectedRawMinerRewardsHash
    (rawMinerRewardsHash rawMinerRewards)

-- --------------------------------------------------------------------------
-- Backward compatibility with legacy implementation

-- | Miner rewards are expected to match the legacy values execpt for
--
-- - block height 125538056 and
-- - block heights strictly larger than 125538057
--
test_blockMinerRewardLegacyCompat :: Assertion
test_blockMinerRewardLegacyCompat = do
    mapM_ rewardsMatch [0..10000]
    mapM_ rewardsMatch [0,1000..maxRewardHeight - 2]
    mapM_ rewardsMatch [maxRewardHeight - 1000 .. maxRewardHeight - 2]
    mapM_ rewardsMatch [maxRewardHeight]
    assertEqual
        "the only block height that is not compatible with the legacy reward computation is 125538056"
        [maxRewardHeight - 1]
        legacyCompatExceptions
  where
    v = Mainnet01
    rewardsMatch h = assertEqual
        "miner reward value matches the legacy value"
        (legacyBlockMinerReward v h)
        (minerRewardKda (blockMinerReward v h))

    legacyCompatExceptions = M.keys $ M.filterWithKey
        (\k _ -> legacyBlockMinerReward v k /= minerRewardKda (blockMinerReward v k))
        minerRewards

-- This should be a CAF and can thus not include the computation in
-- 'mkLegacyMinerRewards' which has a 'HasCallStack' constraint.
--
legacyMinerRewards :: M.Map BlockHeight Kda
legacyMinerRewards = Kda <$> mkLegacyMinerRewards
{-# NOINLINE legacyMinerRewards #-}

-- | The algorithm that was used to parse the rewards table until end of 2024.
--
mkLegacyMinerRewards :: HasCallStack => M.Map BlockHeight Decimal
mkLegacyMinerRewards =
    case CSV.decode CSV.NoHeader (BL.fromStrict rawMinerRewards) of
      Left e -> error
        $ "cannot construct miner reward map: " <> sshow e
      Right vs -> M.fromList . V.toList . V.map formatRow $ vs
  where
    formatRow :: (Word64, CsvDecimal) -> (BlockHeight, Decimal)
    formatRow (!a,!b) = (BlockHeight $ int a, (_csvDecimal b))

legacyBlockMinerReward
    :: ChainwebVersion
    -> BlockHeight
    -> Kda
legacyBlockMinerReward v h =
    case M.lookupGE h legacyMinerRewards of
        Nothing -> error "The end of the chain has been reached"
        Just (_, m) -> Kda $ roundTo 8 (_kda m / n)
  where
    !n = int . order $ chainGraphAt v h

