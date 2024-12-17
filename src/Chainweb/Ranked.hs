{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Chainweb.Ranked
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Blockheight indexed data with an encoding that sort lexicographically by
-- height.
--
-- The main purpose of this data structure is to provide locallity for
-- blockheight indexed data in key-value databases.
--
module Chainweb.Ranked
( Ranked(..)
, encodeRanked
, decodeRanked
) where

import Chainweb.BlockHeight
import Chainweb.Utils.Serialization

import Control.DeepSeq
import Control.Monad

import Data.Hashable

import GHC.Generics

-- -------------------------------------------------------------------------- --
-- BlockHeight Ranked Data

-- | BlockHeight Ranked Data
--
-- Blockheight indexed data with an encoding that sort lexicographically by
-- height.
--
-- The main purpose of this data structure is to provide locallity for
-- blockheight indexed data in key-value databases.
--
data Ranked a = Ranked
    { _rankedHeight :: !BlockHeight
    , _ranked :: !a
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

encodeRanked :: (a -> Put) -> Ranked a -> Put
encodeRanked putA (Ranked r a) = do
    encodeBlockHeightBe r -- big endian encoding for lexicographical order
    putA a
{-# INLINE encodeRanked #-}

decodeRanked :: Get a -> Get (Ranked a)
decodeRanked decodeA = Ranked
    <$!> decodeBlockHeightBe
    <*> decodeA
{-# INLINE decodeRanked #-}

