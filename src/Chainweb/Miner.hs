{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.Pact.Miner
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- The definition of the pact miner and the Pact miner reward
--
module Chainweb.Miner
( -- * Data
  MinerId(..)
, MinerKeys(..)
, Miner(..)
  -- Combinators
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

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Catch


import Data.Aeson hiding (decode)
import Data.ByteString (ByteString)
import qualified Data.Csv as CSV
import Data.Default
import Data.FileEmbed
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Vector as V
import Data.Word

-- chainweb types

import Chainweb.BlockHeader
import Chainweb.Graph
import Chainweb.Payload (MinerData(..))
import Chainweb.Utils

-- Pact types

import Pact.Parse
import Pact.Types.Term (KeySet(..), Name(..))


-- -------------------------------------------------------------------------- --
-- Miner data

-- | Miner id is a thin wrapper around 'Text' to differentiate it from user
-- addresses
newtype MinerId = MinerId Text
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, NFData)

-- | Miner keys are a thin wrapper around a Pact 'KeySet' to differentiate it from
-- user keysets
--
newtype MinerKeys = MinerKeys KeySet
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, NFData)

-- | Miner info data consists of a miner id (text), and
-- its keyset (a pact type)
--
data Miner = Miner !MinerId !MinerKeys
    deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Miner where
    toJSON (Miner (MinerId m) (MinerKeys ks)) = object
      [ "m" .= m
      , "ks" .= _ksKeys ks
      , "kp" .= _ksPredFun ks
      ]

instance FromJSON Miner where
    parseJSON = withObject "Miner" $ \o -> Miner
      <$> (MinerId <$> o .: "m")
      <*> (MinerKeys <$> (KeySet <$> o .: "ks" <*> o .: "kp"))

-- | A lens into the miner id of a miner
--
minerId :: Lens' Miner MinerId
minerId = lens (\(Miner i _) -> i) (\(Miner _ k) b -> Miner b k)
{-# INLINE minerId #-}

-- | A lens into the miner keys of a miner
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
      ( KeySet
        ["f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"]
        (Name "keys-all" def)
      )
{-# INLINE defaultMiner #-}

instance Default Miner where
    def = defaultMiner
    {-# INLINE def #-}

-- | Trivial Miner
--
noMiner :: Miner
noMiner = Miner (MinerId "NoMiner") (MinerKeys $ KeySet [] (Name "<" def))
{-# INLINE noMiner #-}

-- | Convert from Pact 'Miner' to Chainweb 'MinerData'
--
toMinerData :: Miner -> MinerData
toMinerData = MinerData . encodeToByteString
{-# INLINABLE toMinerData  #-}

-- | Convert from Chainweb 'MinerData' to Pact Miner
--
fromMinerData :: MonadThrow m => MinerData -> m Miner
fromMinerData = decodeStrictOrThrow' . _minerData
{-# INLINABLE fromMinerData #-}

-- | Rewards table mapping 3-month periods to their rewards
-- according to the calculated exponential decay over 120 year period
--
readRewards
    :: HasChainGraph v
    => v -> HashMap BlockHeight ParsedDecimal
readRewards v =
    case CSV.decode CSV.NoHeader (toS rawMinerRewards) of
      Left e -> error
        $ "cannot construct miner reward map: "
        <> sshow e
      Right vs -> HM.fromList
        . V.toList
        . V.map formatRow
        $ vs
  where
    formatRow :: (Word64, Double) -> (BlockHeight, ParsedDecimal)
    formatRow (!a,!b) =
      let
        !n = v ^. chainGraph . to (int . size)
        !m = fromRational $ toRational b
      in (BlockHeight a, ParsedDecimal $ m / n)

-- | Read in the reward csv via TH for deployment purposes
--
rawMinerRewards :: ByteString
rawMinerRewards = $(embedFile "rewards/miner_rewards.csv")
{-# NOINLINE rawMinerRewards #-}
