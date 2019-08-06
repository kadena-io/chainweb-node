{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Chainweb.Pact.Miner
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- The definition of the pact miner and the Pact miner reward
--
module Chainweb.Pact.Miner
( -- * Data
  MinerId(..)
, MinerKeys(..)
, Miner(..)
  -- Combinators
, minerReward
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

import Control.DeepSeq
import Control.Lens (Lens', lens)
import Control.Monad.Catch

import Data.Aeson
import Data.Default
import Data.Kind (Type)
import Data.Text (Text)

-- chainweb types

import Chainweb.Payload (MinerData(..))
import Chainweb.Utils

-- Pact types

import Pact.Types.Term (KeySet(..), Name(..))


-- -------------------------------------------------------------------------- --
-- Miner data

-- | Miner id is a thin wrapper around 'Text' to differentiate it from user
-- addresses
newtype MinerId :: Type where
    MinerId :: Text -> MinerId
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData)

-- | Miner keys are a thin wrapper around a Pact 'KeySet' to differentiate it from
-- user keysets
--
newtype MinerKeys :: Type where
    MinerKeys :: KeySet -> MinerKeys
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData)


-- | Miner info data consists of a miner id (text), and
-- its keyset (a pact type)
data Miner where
    Miner :: !MinerId -> !MinerKeys -> Miner
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Miner where
    toJSON (Miner (MinerId m) (MinerKeys ks)) = object
      [ "m" .= m
      , "ks" .= _ksKeys ks
      , "kp" .= _ksPredFun ks
      ]

instance FromJSON Miner where
    parseJSON = withObject "Miner" $ \o -> Miner
      <$> (MinerId <$> o .: "m")
      <*> ( MinerKeys <$>
            ( KeySet
            <$> o .: "ks"
            <*> o .: "kp"
            )
          )

-- | A lens into the miner id of a miner
--
minerId :: Lens' Miner MinerId
minerId = lens (\(Miner i _) -> i) (\(Miner _ k) b -> Miner b k)

-- | A lens into the miner keys of a miner
--
minerKeys :: Lens' Miner MinerKeys
minerKeys = lens (\(Miner _ k) -> k) (\(Miner i _) b -> Miner i b)

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

instance Default Miner where
    def = defaultMiner

-- | Empty Miner
--
noMiner :: Miner
noMiner = Miner (MinerId "NoMiner") (MinerKeys $ KeySet [] (Name "<" def))

-- | Convert from Pact 'Miner' to Chainweb 'MinerData'
--
toMinerData :: Miner -> MinerData
toMinerData = MinerData . encodeToByteString

-- | Convert from Chainweb 'MinerData' to Pact Miner
fromMinerData :: MonadThrow m => MinerData -> m Miner
fromMinerData = decodeStrictOrThrow' . _minerData

-- -------------------------------------------------------------------------- --
-- Miner reward

minerReward :: IO ()
minerReward = undefined
