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
-- The definition of the Pact miner.
--
module Chainweb.Miner.Pact
( -- * Data
  MinerId(..)
, MinerGuard(..)
, Miner(..)
  -- * Combinators
, toMinerData
, fromMinerData
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
import Data.Hashable
import Data.String (IsString(..))
import Data.Text (Text)

-- internal modules

import Chainweb.Payload
import Chainweb.Utils

import qualified Pact.JSON.Encode as J
import qualified Pact.Core.Guards as Pact
import qualified Pact.Core.Names as Pact
import qualified Pact.Core.PactValue as Pact
import qualified Pact.Core.StableEncoding as Pact
import Control.Applicative ((<|>))
import qualified Data.Set as Set

-- -------------------------------------------------------------------------- --
-- Miner data

-- | `MinerId` is a thin wrapper around `Text` to differentiate it from user
-- addresses.
--
newtype MinerId = MinerId { _minerId :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, ToJSON, FromJSON, IsString, NFData, Hashable, J.Encode)

-- | `MinerGuard` is a thin wrapper around a Pact `Guard` to differentiate it
-- from user-level guards.
--
newtype MinerGuard = MinerGuard (Pact.Guard Pact.QualifiedName Pact.PactValue)
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show, NFData)

-- | Miner info data consists of a miner id (text), and its keyset (a pact
-- type).
--
data Miner = Miner
    { _minerMinerId :: !MinerId
    , _minerMinerGuard :: !MinerGuard
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- | IMPORTANT: This JSON encoding is included in the chain Merkle Tree. The
-- order of the properties here is significant!
--
-- These JSON instances are used (among other things) to embed Miner data into
-- the Genesis Payloads. If these change, the payloads become unreadable!
--
instance J.Encode Miner where
    build (Miner (MinerId m) (MinerGuard (Pact.GKeyset ks))) = J.object
        [ "account" J..= m
        , "predicate" J..= Pact.StableEncoding (Pact._ksPredFun ks)
        , "public-keys" J..= J.Array (Pact.StableEncoding <$> Set.toList (Pact._ksKeys ks))
        ]
    build (Miner (MinerId m) (MinerGuard g)) = J.object
        [ "account" J..= m
        , "guard" J..= Pact.StableEncoding g
        ]
    {-# INLINE build #-}

instance FromJSON Miner where
    parseJSON = withObject "Miner" $ \o -> Miner
        <$> (MinerId <$> o .: "account")
        <*>
        (
        MinerGuard . Pact.GKeyset <$>
          (Pact.KeySet <$>
            (Set.fromList . fmap Pact._stableEncoding <$> o .: "public-keys") <*>
            (Pact._stableEncoding <$> o .: "predicate"))
        <|>
        MinerGuard . Pact._stableEncoding <$> o .: "guard"
        )

-- | A lens into the miner id of a miner.
--
minerId :: Lens' Miner MinerId
minerId = lens (\(Miner i _) -> i) (\(Miner _ k) b -> Miner b k)
{-# INLINE minerId #-}

-- | A lens into the miner keys of a miner.
--
minerKeys :: Lens' Miner MinerGuard
minerKeys = lens (\(Miner _ g) -> g) (\(Miner i _) b -> Miner i b)
{-# INLINE minerKeys #-}

-- | Keyset taken from cp examples in Pact
-- The private key here was taken from `examples/cp` from the Pact repository
--
defaultMiner :: Miner
defaultMiner = Miner (MinerId "miner")
    $ MinerGuard
    $ Pact.GKeyset
    $ Pact.KeySet
      (Set.fromList [Pact.PublicKeyText "f880a433d6e2a13a32b6169030f56245efdd8c1b8a5027e9ce98a88e886bef27"])
      Pact.KeysAll

{-# NOINLINE defaultMiner #-}

-- | A trivial Miner.
--
noMiner :: Miner
noMiner = Miner (MinerId "NoMiner")
  (MinerGuard $ Pact.GKeyset $ Pact.KeySet mempty (Pact.CustomPredicate (Pact.TBN (Pact.BareName "<"))))
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
