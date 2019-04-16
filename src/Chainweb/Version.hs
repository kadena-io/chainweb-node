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

import Control.Arrow ((&&&))
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
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time is epoch,
        --   * target is maxBound,
        --   * nonce is constant,
        --   * creationTime of BlockHeaders is parent time plus one second, and
        --   * POW is simulated by poison process thread delay.
        --

    | TestWithTime ChainGraph
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time current time
        --   * target is maxBound,
        --   * nonce is constant
        --   * creationTime of BlockHeaders is actual time, and
        --   * POW is simulated by poison process thread delay.
        --

    | TestWithPow ChainGraph
        -- ^ Test instance with
        --
        --   * configurable graph,
        --   * genesis block time current time
        --   * target is maxBound,
        --   * nonce is constant, and
        --   * creationTime of BlockHeaders is actual time.
        --

    | Testnet00
    deriving (Eq, Ord, Generic)
    deriving anyclass (Hashable, NFData)

instance Show ChainwebVersion where
    show = T.unpack . toText
    {-# INLINE show #-}

isTestChainwebVersionId :: Word32 -> Bool
isTestChainwebVersionId i = 0x80000000 .&. i /= 0x0
{-# INLINABLE isTestChainwebVersionId #-}

chainwebVersionId :: ChainwebVersion -> Word32
chainwebVersionId v@Test{} = toTestChainwebVersion v 0x80000000
chainwebVersionId v@TestWithTime{} = toTestChainwebVersion v 0x80000001
chainwebVersionId v@TestWithPow{} = toTestChainwebVersion v 0x80000002
chainwebVersionId Testnet00 = 0x00000001
{-# INLINABLE chainwebVersionId #-}

fromChainwebVersionId :: MonadGet m => Word32 -> m ChainwebVersion
fromChainwebVersionId i
    | isTestChainwebVersionId i = return $ fromTestChainwebVersionId i
    | otherwise = case i of
        0x00000001 -> return Testnet00
        _ -> fail $ "Unknown Chainweb version id: " ++ show i
{-# INLINABLE fromChainwebVersionId #-}

encodeChainwebVersion :: MonadPut m => ChainwebVersion -> m ()
encodeChainwebVersion = putWord32le . chainwebVersionId
{-# INLINABLE encodeChainwebVersion #-}

decodeChainwebVersion :: MonadGet m => m ChainwebVersion
decodeChainwebVersion = getWord32le >>= fromChainwebVersionId
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
chainwebVersionToText v =
    case HM.lookup v prettyVersions of
        Just t -> t
        Nothing -> case v of
            Test{} -> "test"
            TestWithTime{} -> "testWithTime"
            TestWithPow{} -> "testWithPow"
            Testnet00 -> "testnet00"
{-# INLINABLE chainwebVersionToText #-}

-- | Read textual representation of a `ChainwebVersion`.
--
chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText t =
    case HM.lookup t chainwebVersions of
        Just v -> pure v
        Nothing -> case t of
            "test" -> pure $ Test petersonChainGraph
            "testWithTime" -> pure $ TestWithTime petersonChainGraph
            "testWithPow" -> pure $ TestWithPow petersonChainGraph
            _ -> throwM . TextFormatException $ "Unknown Chainweb version: " <> t

instance HasTextRepresentation ChainwebVersion where
    toText = chainwebVersionToText
    {-# INLINE toText #-}
    fromText = chainwebVersionFromText
    {-# INLINE fromText #-}

chainwebVersions :: HM.HashMap T.Text ChainwebVersion
chainwebVersions = HM.fromList $
    f Test "test"
    <> f TestWithTime "testWithTime"
    <> f TestWithPow "testWithPow"
    <> [ ("testnet00", Testnet00) ]
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

chainwebVersionIds :: HM.HashMap Word32 ChainwebVersion
chainwebVersionIds =
    HM.fromList . map (chainwebVersionId &&& id) $ HM.elems chainwebVersions

-- -------------------------------------------------------------------------- --
-- Test instances
--
-- The code in this section must not be called in production.
--

toTestChainwebVersion :: HasCallStack => ChainwebVersion -> Word32 -> Word32
toTestChainwebVersion Testnet00 _ =
    error "toTestChainwebVersion must not be called for a production instance"
toTestChainwebVersion v i = i .|. (testChainwebVersionMask .&. int (hash v))

testChainwebVersionMask :: Word32
testChainwebVersionMask = 0x7fff0000

fromTestChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromTestChainwebVersionId i =
    case HM.lookup i chainwebVersionIds of
        Nothing -> error "failed to lookup test chainweb version in testChainwebVersionMap"
        Just v -> v

-- -------------------------------------------------------------------------- --
-- Basic Properties

chainwebVersionGraph :: ChainwebVersion -> ChainGraph
chainwebVersionGraph (Test g) = g
chainwebVersionGraph (TestWithTime g) = g
chainwebVersionGraph (TestWithPow g) = g
chainwebVersionGraph Testnet00 = petersonChainGraph

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
