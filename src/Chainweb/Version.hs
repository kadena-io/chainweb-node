{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
, chainwebVersionFromText
, chainwebVersionToText
, chainwebVersionId

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

-- * HasChainwebVersion
, HasChainwebVersion(..)
, mkChainId
, chainIds
, someChainId
, randomChainId

-- * ChainId
, module Chainweb.ChainId

-- * Re-exports from Chainweb.ChainGraph

-- ** Chain Graph
, ChainGraph
, HasChainGraph(..)
, adjacentChainIds

-- ** Graph Properties
, order
, diameter
, degree
, shortestPath

-- ** Undirected Edges
, AdjPair
, _getAdjPair
, pattern Adj
, adjs
, adjsOfVertex
, checkAdjacentChainIds
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Catch

import Data.Aeson hiding (pairs)
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Proxy
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Word

import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits

import System.Random

-- internal modules

import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
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
    = Test ChainGraph
        -- ^ Test instance with:
        --
        --   * configurable graph,
        --   * genesis block time is epoch,
        --   * target is maxBound,
        --   * nonce is constant,
        --   * creationTime of BlockHeaders is parent time plus one second, and
        --   * POW is simulated by poison process thread delay.
        --

    | TimedConsensus ChainGraph
        -- ^ Test instance with:
        --
        --   * configurable graph,
        --   * genesis block time current time,
        --   * target is maxBound,
        --   * nonce is constant,
        --   * creationTime of BlockHeaders is actual time, and
        --   * POW is simulated by poison process thread delay.
        --

    | PowConsensus ChainGraph
        -- ^ Test instance with:
        --
        --   * configurable graph,
        --   * genesis block time current time,
        --   * target is maxBound,
        --   * nonce is constant, and
        --   * creationTime of BlockHeaders is actual time.
        --

    | Testnet00
    | Testnet01
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

instance Show ChainwebVersion where
    show = T.unpack . toText
    {-# INLINE show #-}

-- | This function and its dual `fromChainwebVersionId` are used to efficiently
-- serialize a `ChainwebVersion` and its associated internal `ChainGraph` value.
-- __This function must be injective (one-to-one)!__ The scheme is as follows:
--
--   * Production `ChainwebVersion`s start from @0x00000001@ and count upwards.
--     Their value must be less than @0x8000000@, but this limit is unlikely to
--     ever be reached.
--
--   * `ChainwebVersion`s for testing begin at @0x80000000@, as can be seen in
--     `toTestChainwebVersion`. This value is combined (via `.|.`) with the
--     "code" of their associated `ChainGraph` (as seen in `graphToCode`). Such
--     codes start at @0x00010000@ and count upwards.
--
chainwebVersionId :: ChainwebVersion -> Word32
chainwebVersionId v@Test{} = toTestChainwebVersion v
chainwebVersionId v@TimedConsensus{} = toTestChainwebVersion v
chainwebVersionId v@PowConsensus{} = toTestChainwebVersion v
chainwebVersionId Testnet00 = 0x00000001
chainwebVersionId Testnet01 = 0x00000002
{-# INLINABLE chainwebVersionId #-}

fromChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromChainwebVersionId 0x00000001 = Testnet00
fromChainwebVersionId 0x00000002 = Testnet01
fromChainwebVersionId i = fromTestChainwebVersionId i
{-# INLINABLE fromChainwebVersionId #-}

encodeChainwebVersion :: MonadPut m => ChainwebVersion -> m ()
encodeChainwebVersion = putWord32le . chainwebVersionId
{-# INLINABLE encodeChainwebVersion #-}

decodeChainwebVersion :: MonadGet m => m ChainwebVersion
decodeChainwebVersion = fromChainwebVersionId <$> getWord32le
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

chainwebVersionToText :: HasCallStack => ChainwebVersion -> T.Text
chainwebVersionToText Testnet00 = "testnet00"
chainwebVersionToText Testnet01 = "testnet01"
chainwebVersionToText v = fromJuste $ HM.lookup v prettyVersions
{-# INLINABLE chainwebVersionToText #-}

-- | Read textual representation of a `ChainwebVersion`.
--
chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText "testnet00" = pure Testnet00
chainwebVersionFromText "testnet01" = pure Testnet01
chainwebVersionFromText t =
    case HM.lookup t chainwebVersions of
        Just v -> pure v
        Nothing -> case t of
            "test" -> pure $ Test petersonChainGraph
            "timedConsensus" -> pure $ TimedConsensus petersonChainGraph
            "powConsensus" -> pure $ PowConsensus petersonChainGraph
            _ -> throwM . TextFormatException $ "Unknown Chainweb version: " <> t

instance HasTextRepresentation ChainwebVersion where
    toText = chainwebVersionToText
    {-# INLINE toText #-}
    fromText = chainwebVersionFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Value Maps

chainwebVersions :: HM.HashMap T.Text ChainwebVersion
chainwebVersions = HM.fromList $
    f Test "test"
    <> f TimedConsensus "timedConsensus"
    <> f PowConsensus "powConsensus"
    <> [ ("testnet00", Testnet00), ("testnet01", Testnet01) ]
  where
    f v p = map (\(k, g) -> (p <> k, v g)) pairs
    pairs = [ ("-singleton", singletonChainGraph)
            , ("-pair", pairChainGraph)
            , ("-triangle", triangleChainGraph)
            , ("-peterson", petersonChainGraph)
            , ("-twenty", twentyChainGraph)
            , ("-hoffman-singleton", hoffmanSingletonGraph)
            ]

prettyVersions :: HM.HashMap ChainwebVersion T.Text
prettyVersions = HM.fromList . map swap $ HM.toList chainwebVersions

-- -------------------------------------------------------------------------- --
-- Test instances
--
-- The code in this section must not be called in production.
--

-- | See `chainwebVersionId` for a complete explanation of the values in this
-- section below.
--
toTestChainwebVersion :: HasCallStack => ChainwebVersion -> Word32
toTestChainwebVersion v =
    testVersionToCode v .|. graphToCode (view (chainGraph . chainGraphKnown) v)

-- | For the binary encoding of a `ChainGraph` within a `ChainwebVersion`.
--
graphToCode :: KnownGraph -> Word32
graphToCode Singleton = 0x00010000
graphToCode Pair = 0x00020000
graphToCode Triangle = 0x00030000
graphToCode Peterson = 0x00040000
graphToCode Twenty = 0x00050000
graphToCode HoffmanSingle = 0x00060000

codeToGraph :: HasCallStack => Word32 -> KnownGraph
codeToGraph 0x00010000 = Singleton
codeToGraph 0x00020000 = Pair
codeToGraph 0x00030000 = Triangle
codeToGraph 0x00040000 = Peterson
codeToGraph 0x00050000 = Twenty
codeToGraph 0x00060000 = HoffmanSingle
codeToGraph _ = error "Unknown Graph Code"

-- | Split a `Word32` representation of a `ChainwebVersion` / `ChainGraph` pair
-- into its constituent pieces.
--
splitTestCode :: Word32 -> (Word32, Word32)
splitTestCode w = (0xf000ffff .&. w, 0x0fff0000 .&. w)

codeToTestVersion :: HasCallStack => Word32 -> (ChainGraph -> ChainwebVersion)
codeToTestVersion 0x80000000 = Test
codeToTestVersion 0x80000001 = TimedConsensus
codeToTestVersion 0x80000002 = PowConsensus
codeToTestVersion _ = error "Unknown ChainwebVersion Code"

testVersionToCode :: ChainwebVersion -> Word32
testVersionToCode Test{} = 0x80000000
testVersionToCode TimedConsensus{} = 0x80000001
testVersionToCode PowConsensus{} = 0x80000002
testVersionToCode Testnet00 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"
testVersionToCode Testnet01 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"

fromTestChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromTestChainwebVersionId i =
    uncurry ($) . bimap codeToTestVersion (knownGraph . codeToGraph) $ splitTestCode i

-- -------------------------------------------------------------------------- --
-- Basic Properties

chainwebVersionGraph :: ChainwebVersion -> ChainGraph
chainwebVersionGraph (Test g) = g
chainwebVersionGraph (TimedConsensus g) = g
chainwebVersionGraph (PowConsensus g) = g
chainwebVersionGraph Testnet00 = petersonChainGraph
chainwebVersionGraph Testnet01 = twentyChainGraph

instance HasChainGraph ChainwebVersion where
    _chainGraph = chainwebVersionGraph
    {-# INLINE _chainGraph #-}

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

-- -------------------------------------------------------------------------- --
-- HasChainwebVersion Class

class HasChainwebVersion a where
    _chainwebVersion :: a -> ChainwebVersion
    _chainwebVersion = view chainwebVersion
    {-# INLINE _chainwebVersion #-}

    chainwebVersion :: Getter a ChainwebVersion
    chainwebVersion = to _chainwebVersion
    {-# INLINE chainwebVersion #-}

    {-# MINIMAL _chainwebVersion | chainwebVersion #-}

instance HasChainwebVersion ChainwebVersion where
    _chainwebVersion = id
    {-# INLINE _chainwebVersion #-}

chainIds :: HasChainwebVersion v => v -> HS.HashSet ChainId
chainIds = graphChainIds . _chainGraph . _chainwebVersion
{-# INLINE chainIds #-}

mkChainId
    :: MonadThrow m
    => HasChainwebVersion v
    => Integral i
    => v
    -> i
    -> m ChainId
mkChainId v i = cid
    <$ checkWebChainId (chainwebVersionGraph $ _chainwebVersion v) cid
  where
    cid = unsafeChainId (fromIntegral i)
{-# INLINE mkChainId #-}

-- | Sometimes, in particular for testing and examples, some fixed chain id is
-- needed, but it doesn't matter which one. This function provides some valid
-- chain ids.
--
someChainId :: HasCallStack => HasChainwebVersion v => v -> ChainId
someChainId = head . toList . chainIds
    -- 'head' is guaranteed to succeed because the empty graph isn't a valid chain
    -- graph.
{-# INLINE someChainId #-}

-- | Uniformily get a random ChainId
--
randomChainId :: HasChainwebVersion v => v -> IO ChainId
randomChainId v = (!!) (toList cs) <$> randomRIO (0, length cs - 1)
  where
    cs = chainIds v
