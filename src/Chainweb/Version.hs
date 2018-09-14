{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Version
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Version
( ChainwebVersion(..)
, encodeChainwebVersion
, decodeChainwebVersion
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable (Hashable)

import GHC.Generics

-- -------------------------------------------------------------------------- --
-- Chainweb Version

-- | Generally, a chain is uniquely identified by it's genesis block. For efficiency
-- and convenience we explicitely propagate 'ChainwebVersion' and the 'ChainId'
-- to all blocks in the chain. At runtime the 'ChainId' is represented at
-- the type level (but included as value in serialized representations). Thus,
-- the ChainwebVersion identifies a chain at runtime at the value level.
--
-- We assume that values that are identified through different Chainweb
-- versions are not mixed at runtime. This is not enforced at the type level.
--
data ChainwebVersion
    = Test
    | Simulation
    | Testnet00
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON)

encodeChainwebVersion :: MonadPut m => ChainwebVersion -> m ()
encodeChainwebVersion Test = putWord32le 0x0
encodeChainwebVersion Simulation = putWord32le 0x1
encodeChainwebVersion Testnet00 = putWord32le 0x2

decodeChainwebVersion :: MonadGet m => m ChainwebVersion
decodeChainwebVersion = getWord32le >>= \case
    0x0 -> return Test
    0x1 -> return Simulation
    0x2 -> return Testnet00
    x -> fail $ "Unknown chainweb version: " ++ show x

