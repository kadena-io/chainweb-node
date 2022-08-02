{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Difficulty
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.Difficulty
( properties
, prop_littleEndian
) where

import Data.Serialize.Put
import qualified Data.ByteString as B

import Test.QuickCheck (Property, property)

-- internal modules

import Chainweb.Difficulty

prop_littleEndian :: Bool
prop_littleEndian = all run [1..31]
  where
    run i = (==) i
        $ length
        $ takeWhile (== 0x00)
        $ reverse
        $ B.unpack
        $ runPut
        $ encodePowHashNat (maxBound `div` 2^(8*i))

properties :: [(String, Property)]
properties =
    [ ("BlockHashNat is encoded as little endian", property prop_littleEndian)
    ]

