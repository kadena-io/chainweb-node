{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Miner.Pact
-- Copyright: Copyright © 2019 Kadena LLC.
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
, minerRewardHeights
, minerRewards
  -- * Defaults
, noMiner
, defaultMiner
  -- * CLI Utils
, pMiner
) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadThrow)

import Data.Aeson hiding (decode)
import Data.ByteString (ByteString)
import qualified Data.Csv as CSV
import Data.Decimal (roundTo)
import Data.Default (Default(..))
import Data.FileEmbed (embedFile)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import qualified Data.Set as S
import Data.String (IsString)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Vector as V
import Data.Word

import Options.Applicative

-- internal modules

import Chainweb.BlockHeader (BlockHeight(..))
import Chainweb.Graph (HasChainGraph(..), order)
import Chainweb.Payload (MinerData(..))
import Chainweb.Utils

import Pact.Parse (ParsedDecimal(..))
import Pact.Types.Names
import Pact.Types.Term (KeySet(..), PublicKey, mkKeySet)

-- -------------------------------------------------------------------------- --
-- Miner data

-- | `MinerId` is a thin wrapper around `Text` to differentiate it from user
-- addresses.
newtype MinerId = MinerId Text
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, IsString, NFData)

-- | `MinerKeys` are a thin wrapper around a Pact `KeySet` to differentiate it
-- from user keysets.
--
newtype MinerKeys = MinerKeys KeySet
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, NFData)

-- | Miner info data consists of a miner id (text), and its keyset (a pact
-- type).
--
data Miner = Miner !MinerId !MinerKeys
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- NOTE: These JSON instances are used (among other things) to embed Miner data
-- into the Genesis Payloads. If these change, the payloads become unreadable!
--
instance ToJSON Miner where
    toJSON (Miner (MinerId m) (MinerKeys ks)) = object
        [ "account" .= m
        , "public-keys" .= _ksKeys ks
        , "predicate" .= _ksPredFun ks
        ]

instance FromJSON Miner where
    parseJSON = withObject "Miner" $ \o -> Miner
        <$> (MinerId <$> o .: "account")
        <*> (MinerKeys <$> (KeySet <$> o .: "public-keys" <*> o .: "predicate"))

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
    $ mkKeySet
      ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
      "keys-all"

{-# INLINE defaultMiner #-}

-- | A trivial Miner.
--
noMiner :: Miner
noMiner = Miner (MinerId "NoMiner") (MinerKeys $ mkKeySet [] "<")
{-# INLINE noMiner #-}

-- | Convert from Pact `Miner` to Chainweb `MinerData`.
--
toMinerData :: Miner -> MinerData
toMinerData = MinerData . encodeToByteString
{-# INLINABLE toMinerData  #-}

-- | Convert from Chainweb `MinerData` to Pact Miner.
--
fromMinerData :: MonadThrow m => MinerData -> m Miner
fromMinerData = decodeStrictOrThrow' . _minerData
{-# INLINABLE fromMinerData #-}

data MinerRewards = MinerRewards
    { _minerRewards :: !(HashMap BlockHeight ParsedDecimal)
      -- ^ The map of blockheight thresholds to miner rewards
    , _minerRewardHeights :: !(Vector BlockHeight)
      -- ^ A (sorted) vector of blockheights (head is most recent)
    } deriving (Eq, Ord, Show, Generic)

-- | A getter into the map of heights to rewards
--
minerRewards :: Getter MinerRewards (HashMap BlockHeight ParsedDecimal)
minerRewards = to _minerRewards

-- | A lens into the sorted vector of significant block heights pegged to a reward
--
minerRewardHeights :: Lens' MinerRewards (Vector BlockHeight)
minerRewardHeights = lens _minerRewardHeights (\t b -> t { _minerRewardHeights = b })

-- | Rewards table mapping 3-month periods to their rewards
-- according to the calculated exponential decay over 120 year period
--
readRewards :: HasChainGraph v => v -> MinerRewards
readRewards v =
    case CSV.decode CSV.NoHeader (toS rawMinerRewards) of
      Left e -> error
        $ "cannot construct miner reward map: "
        <> sshow e
      Right vs ->
        let !rs = HM.fromList . V.toList . V.map formatRow $ vs
        in MinerRewards rs (V.fromList . sort $! HM.keys rs)
  where
    formatRow :: (Word64, Double) -> (BlockHeight, ParsedDecimal)
    formatRow (!a,!b) =
      let
        !n = v ^. chainGraph . to (int . order)
        !m = fromRational $ toRational b
      in (BlockHeight $ int a, ParsedDecimal $ roundTo 8 (m / n))

-- | Read in the reward csv via TH for deployment purposes.
--
rawMinerRewards :: ByteString
rawMinerRewards = $(embedFile "rewards/miner_rewards.csv")
{-# NOINLINE rawMinerRewards #-}

--------------------------------------------------------------------------------
-- CLI Utils

pMiner :: Parser Miner
pMiner = Miner
    <$> strOption (long "miner-account" <> help "Coin Contract account name of Miner")
    <*> (MinerKeys <$> pks)
  where
    pks :: Parser KeySet
    pks = KeySet <$> (fmap S.fromList $ many pKey) <*> pPred

pKey :: Parser PublicKey
pKey = strOption (long "miner-key"
    <> help "Public key of the account to send rewards (can pass multiple times)")

pPred :: Parser Name
pPred = (\s -> Name $ BareName s def) <$>
    strOption (long "miner-pred" <> value "keys-all" <> help "Keyset predicate")
