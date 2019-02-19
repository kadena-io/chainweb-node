{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

-- * Typelevel ChainwebVersion
, ChainwebVersionT(..)
, ChainwebVersionSymbol
, chainwebVersionSymbolVal
, SomeChainwebVersionT(..)
, KnownChainwebVersionSymbol
, someChainwebVersionVal

-- * Singletons
, Sing(SChainwebVersion)
, SChainwebVersion

) where

import Control.DeepSeq
import Control.Monad.Catch

import Data.Aeson
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Hashable (Hashable)
import Data.Proxy
import qualified Data.Text as T

import GHC.Generics
import GHC.TypeLits

-- internal modules

import Chainweb.Crypto.MerkleLog
import Chainweb.MerkleUniverse
import Chainweb.Utils

import Data.Singletons

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
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time is epoch,
        --   * target is maxBound,
        --   * nonce is constant,
        --   * creationTime of BlockHeaders is parent time plus one second, and
        --   * POW is simulated by poison process thread delay.
        --

    | TestWithTime
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time current time
        --   * target is maxBound,
        --   * nonce is constant
        --   * creationTime of BlockHeaders is actual time, and
        --   * POW is simulated by poison process thread delay.
        --

    | TestWithPow
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time current time
        --   * target is maxBound,
        --   * nonce is constant, and
        --   * creationTime of BlockHeaders is actual time.
        --

    | Simulation
    | Testnet00
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass (Hashable, NFData)

encodeChainwebVersion :: MonadPut m => ChainwebVersion -> m ()
encodeChainwebVersion Test = putWord32le 0x00000000
encodeChainwebVersion TestWithTime = putWord32le 0x00000001
encodeChainwebVersion TestWithPow = putWord32le 0x00000002
encodeChainwebVersion Simulation = putWord32le 0x00000003
encodeChainwebVersion Testnet00 = putWord32le 0x00010000
{-# INLINABLE encodeChainwebVersion #-}

decodeChainwebVersion :: MonadGet m => m ChainwebVersion
decodeChainwebVersion = getWord32le >>= \case
    0x00000000 -> return Test
    0x00000001 -> return TestWithTime
    0x00000002 -> return TestWithPow
    0x00000003 -> return Simulation
    0x00010000 -> return Testnet00
    x -> fail $ "Unknown Chainweb version: " ++ show x
{-# INLINABLE decodeChainwebVersion #-}

instance ToJSON ChainwebVersion where
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance FromJSON ChainwebVersion where
    parseJSON = parseJsonFromText "ChainwebVersion"

instance IsMerkleLogEntry ChainwebHashTag ChainwebVersion where
    type Tag ChainwebVersion = 'ChainwebVersionTag
    toMerkleNode = encodeMerkleInputNode encodeChainwebVersion
    fromMerkleNode = decodeMerkleInputNode decodeChainwebVersion
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

chainwebVersionToText :: ChainwebVersion -> T.Text
chainwebVersionToText Test = "test"
chainwebVersionToText TestWithTime = "testWithTime"
chainwebVersionToText TestWithPow = "testWithPow"
chainwebVersionToText Simulation = "simulation"
chainwebVersionToText Testnet00 = "testnet00"
{-# INLINABLE chainwebVersionToText #-}

chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText "test" = return Test
chainwebVersionFromText "testWithTime" = return TestWithTime
chainwebVersionFromText "testWithPow" = return TestWithPow
chainwebVersionFromText "simulation" =return Simulation
chainwebVersionFromText "testnet00" = return Testnet00
chainwebVersionFromText t = throwM . TextFormatException
    $ "Unknown Chainweb version: \"" <> t <> "\"."
{-# INLINABLE chainwebVersionFromText #-}

instance HasTextRepresentation ChainwebVersion where
    toText = chainwebVersionToText
    {-# INLINE toText #-}
    fromText = chainwebVersionFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Type level ChainwebVersion

newtype ChainwebVersionT = ChainwebVersionT Symbol

data SomeChainwebVersionT = forall (a :: ChainwebVersionT)
        . KnownChainwebVersionSymbol a => SomeChainwebVersionT (Proxy a)

class KnownSymbol (ChainwebVersionSymbol n) => KnownChainwebVersionSymbol (n :: ChainwebVersionT) where
    type ChainwebVersionSymbol n :: Symbol
    chainwebVersionSymbolVal :: Proxy n -> T.Text

instance (KnownSymbol n) => KnownChainwebVersionSymbol ('ChainwebVersionT n) where
    type ChainwebVersionSymbol ('ChainwebVersionT n) = n
    chainwebVersionSymbolVal _ = T.pack $ symbolVal (Proxy @n)

someChainwebVersionVal :: ChainwebVersion -> SomeChainwebVersionT
someChainwebVersionVal v = case someSymbolVal (sshow v) of
    (SomeSymbol (Proxy :: Proxy v)) -> SomeChainwebVersionT (Proxy @('ChainwebVersionT v))

-- -------------------------------------------------------------------------- --
-- Singletons

data instance Sing (v :: ChainwebVersionT) where
    SChainwebVersion :: KnownChainwebVersionSymbol v => Sing v

type SChainwebVersion (v :: ChainwebVersionT) = Sing v

instance KnownChainwebVersionSymbol v => SingI (v :: ChainwebVersionT) where
    sing = SChainwebVersion

instance SingKind ChainwebVersionT where
    type Demote ChainwebVersionT = ChainwebVersion

    fromSing (SChainwebVersion :: Sing v) = unsafeFromText
        . chainwebVersionSymbolVal $ Proxy @v

    toSing n = case someChainwebVersionVal n of
        SomeChainwebVersionT p -> SomeSing (singByProxy p)
