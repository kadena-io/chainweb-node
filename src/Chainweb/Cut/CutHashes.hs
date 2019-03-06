{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Cut.CutHashes
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Cut.CutHashes
(
-- * CutHashes
  CutHashes(..)
, cutToCutHashes
) where

import Control.DeepSeq

import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as HM

import GHC.Generics

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Cut Hashes

data CutHashes = CutHashes
    { _cutHashes :: !(HM.HashMap ChainId BlockHash)
    , _cutOrigin :: !(Maybe PeerInfo)
        -- ^ 'Nothing' is used for locally mined Cuts
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

instance ToJSON CutHashes
instance FromJSON CutHashes
instance NFData CutHashes

cutToCutHashes :: Maybe PeerInfo -> Cut -> CutHashes
cutToCutHashes p c = CutHashes (_blockHash <$> _cutMap c) p
