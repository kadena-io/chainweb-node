{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

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
, JsonRanked(..)
) where

import Chainweb.BlockHeight
import Chainweb.Utils
import Chainweb.Utils.Serialization

import Control.DeepSeq
import Control.Monad

import Data.Aeson
import Data.Hashable
import Data.Typeable (Proxy(..), Typeable, typeRep)

import GHC.Generics (Generic)
import GHC.TypeLits

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

-- -------------------------------------------------------------------------- --

-- | JSON Encoding for Ranked Types.
--
-- The first type parameter is the JSON key for the value.
--
newtype JsonRanked (s :: Symbol) a = JsonRanked { _jsonRanked :: Ranked a }

instance (ToJSON a, KnownSymbol s) => ToJSON (JsonRanked s a) where
    toJSON (JsonRanked r) = object
        [ "height" .= _rankedHeight r
        , symbolText @s .= _ranked r
        ]
    toEncoding (JsonRanked r) = pairs $ mconcat
        [ "height" .= _rankedHeight r
        , symbolText @s .= _ranked r
        ]
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance (KnownSymbol s, Typeable a, FromJSON a) => FromJSON (JsonRanked s a) where
    parseJSON = withObject ("Ranked " <> show (typeRep (Proxy @a))) $ \o ->
        fmap JsonRanked $ Ranked
            <$> o .: "height"
            <*> o .: symbolText @s
    {-# INLINE parseJSON  #-}
