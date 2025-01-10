{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Miner.Pact
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- The definition of the Pact miner and the Pact miner reward.
--
module Chainweb.Miner.Pact
( -- * Data
  MinerId(..)
, MinerKeys(..)
, Miner(..)
, MinerRewards(..)
  -- * Combinators
, toMinerData
, fromMinerData
, readRewards
, rawMinerRewards
  -- * Optics
, minerId
, minerKeys
  -- * Defaults
, noMiner
, defaultMiner
) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)

import Data.Aeson hiding (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import Data.Decimal (Decimal)
import Data.FileEmbed (embedFile)
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word

-- internal modules

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Payload
import Chainweb.Utils

import qualified Pact.JSON.Encode as J
import qualified Pact.Types.KeySet as Pact4

-- -------------------------------------------------------------------------- --
-- Miner data

-- | `MinerId` is a thin wrapper around `Text` to differentiate it from user
-- addresses.
--
newtype MinerId = MinerId { _minerId :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, IsString, NFData, Hashable, J.Encode)

-- | `MinerKeys` are a thin wrapper around a Pact `KeySet` to differentiate it
-- from user keysets.
--
newtype MinerKeys = MinerKeys Pact4.KeySet
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, NFData)

-- | Miner info data consists of a miner id (text), and its keyset (a pact
-- type).
--
data Miner = Miner !MinerId !MinerKeys
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- | IMPORTANT: This JSON encoding is included in the chain Merkle Tree. The
-- order of the properties here is significant!
--
-- These JSON instances are used (among other things) to embed Miner data into
-- the Genesis Payloads. If these change, the payloads become unreadable!
--
instance J.Encode Miner where
    build (Miner (MinerId m) (MinerKeys ks)) = J.object
        [ "account" J..= m
        , "predicate" J..= Pact4._ksPredFun ks
        , "public-keys" J..= J.Array (Pact4._ksKeys ks)
        ]
    {-# INLINE build #-}

instance FromJSON Miner where
    parseJSON = withObject "Miner" $ \o -> Miner
        <$> (MinerId <$> o .: "account")
        <*> (MinerKeys <$> (Pact4.KeySet <$> o .: "public-keys" <*> o .: "predicate"))

-- | A lens into the miner id of a miner.
--
minerId :: Lens' Miner MinerId
minerId = lens (\(Miner i _) -> i) (\(Miner _ k) b -> Miner b k)
{-# INLINE minerId #-}

-- | A lens into the miner keys of a miner.
--
minerKeys :: Lens' Miner MinerKeys
minerKeys = lens (\(Miner _ k) -> k) (\(Miner i _) b -> Miner i b)
{-# INLINE minerKeys #-}

-- | Keyset taken from cp examples in Pact
-- The private key here was taken from `examples/cp` from the Pact repository
--
defaultMiner :: Miner
defaultMiner = Miner (MinerId "miner")
    $ MinerKeys
    $ Pact4.mkKeySet
      ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
      "keys-all"

{-# NOINLINE defaultMiner #-}

-- | A trivial Miner.
--
noMiner :: Miner
noMiner = Miner (MinerId "NoMiner") (MinerKeys $ Pact4.mkKeySet [] "<")
{-# NOINLINE noMiner #-}

-- | Convert from Pact `Miner` to Chainweb `MinerData`.
--
toMinerData :: Miner -> MinerData
toMinerData = MinerData . J.encodeStrict
{-# INLINABLE toMinerData  #-}

-- | Convert from Chainweb `MinerData` to Pact Miner.
--
fromMinerData :: MonadThrow m => MinerData -> m Miner
fromMinerData = decodeStrictOrThrow' . _minerData
{-# INLINABLE fromMinerData #-}

newtype MinerRewards = MinerRewards
    { _minerRewards :: Map BlockHeight Decimal
      -- ^ The map of blockheight thresholds to miner rewards
    } deriving (Eq, Ord, Show, Generic)

-- | Rewards table mapping 3-month periods to their rewards
-- according to the calculated exponential decay over 120 year period
--
readRewards :: MinerRewards
readRewards =
    case CSV.decode CSV.NoHeader (BL.fromStrict rawMinerRewards) of
      Left e -> error
        $ "cannot construct miner reward map: "
        <> sshow e
      Right vs -> MinerRewards $ M.fromList . V.toList . V.map formatRow $ vs
  where
    formatRow :: (Word64, CsvDecimal) -> (BlockHeight, Decimal)
    formatRow (!a,!b) = (BlockHeight $ int a, _csvDecimal b)

-- | Read in the reward csv via TH for deployment purposes.
--
rawMinerRewards :: ByteString
rawMinerRewards = $(embedFile "rewards/miner_rewards.csv")
{-# NOINLINE rawMinerRewards #-}
