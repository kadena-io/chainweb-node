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

import Control.Arrow
import Control.DeepSeq

import Data.Aeson
import Data.Function
import Data.Hashable
import qualified Data.HashMap.Strict as HM

import GHC.Generics

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Version

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Cut Hashes

data CutHashes = CutHashes
    { _cutHashes :: !(HM.HashMap ChainId BlockHash)
    , _cutOrigin :: !(Maybe PeerInfo)
        -- ^ 'Nothing' is used for locally mined Cuts
    , _cutHashesWeight :: !BlockWeight
    , _cutHashesHeight :: !BlockHeight
    , _cutHashesChainwebVersion :: !ChainwebVersion
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (Hashable, NFData)

instance Ord CutHashes where
    compare = compare `on` (_cutHashesWeight &&& _cutHashes)

instance ToJSON CutHashes where
    toJSON c = object
        [ "hashes" .= _cutHashes c
        , "origin" .= _cutOrigin c
        , "weight" .= _cutHashesWeight c
        , "height" .= _cutHashesHeight c
        , "instance" .= _cutHashesChainwebVersion c
        ]

instance FromJSON CutHashes where
    parseJSON = withObject "CutHashes" $ \o -> CutHashes
        <$> o .: "hashes"
        <*> o .: "origin"
        <*> o .: "weight"
        <*> o .: "height"
        <*> o .: "instance"

cutToCutHashes :: Maybe PeerInfo -> Cut -> CutHashes
cutToCutHashes p c = CutHashes
    { _cutHashes = _blockHash <$> _cutMap c
    , _cutOrigin = p
    , _cutHashesWeight = _cutWeight c
    , _cutHashesHeight = _cutHeight c
    , _cutHashesChainwebVersion = _chainwebVersion c
    }

