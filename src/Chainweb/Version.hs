{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Chainweb.Version
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Properties of Chainweb Versions
--
module Chainweb.Version
( ChainwebVersion(..)
, encodeChainwebVersion
, decodeChainwebVersion
, chainwebVersionFromText
, chainwebVersionToText
, chainwebVersionId

-- * Properties of Chainweb Version
-- ** Chain Graph
, chainwebGraphs
, genesisGraph
, genesisHeight
, to20ChainsDevelopment
-- ** POW
, BlockRate(..)
, blockRate
, WindowWidth(..)
, window
, headerSizeBytes
, workSizeBytes
-- ** Payload Validation Parameters
, maxBlockGasLimit
-- ** Payload Validation Guards
, vuln797Fix
, coinV2Upgrade
, to20ChainRebalance
, pactBackCompat_v16
, skipTxTimingValidation
, enableModuleNameFix
, enableModuleNameFix2
, enablePactEvents
, enableSPVBridge
, pact4coin3Upgrade
, pact420Upgrade
, enforceKeysetFormats
, AtOrAfter(..)
, doCheckTxHash
, chainweb213Pact
, chainweb214Pact
, chainweb215Pact
, chainweb216Pact
, pact44NewTrans

-- ** BlockHeader Validation Guards
, slowEpochGuard
, oldTargetGuard
, skipFeatureFlagValidationGuard
, oldDaGuard

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
, pattern FromSingChainwebVersion

-- * HasChainwebVersion
, HasChainwebVersion(..)
, mkChainId
, chainIds

-- * ChainId
, module Chainweb.ChainId

-- * Re-exports from Chainweb.ChainGraph

-- ** Chain Graph
, ChainGraph
, HasChainGraph(..)
, adjacentChainIds
, chainGraphAt
, chainGraphAt_
, chainwebGraphsAt

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

-- * Internal. Don't use. Exported only for testing
, headerSizes
, headerBaseSizeBytes
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Aeson hiding (pairs)
import Data.Bits
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Text as T
import Data.Word

import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits

import Numeric.Natural

import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Environment (lookupEnv)

import Text.Read (readMaybe)

-- internal modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
import Chainweb.MerkleUniverse
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Serialization

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
    --------------------
    -- TESTING INSTANCES
    --------------------
    = Test ChainGraph
        -- ^ General-purpose test instance, where:
        --
        --   * the underlying `ChainGraph` is configurable,
        --   * the genesis block time is the Linux epoch,
        --   * each `HashTarget` is maxBound,
        --   * each mining `Nonce` is constant,
        --   * the creationTime of `BlockHeader`s is the parent time plus one second, and
        --   * POW is simulated by poison process thread delay.
        --
        -- This is primarily used in unit tests.
        --

    | TimedConsensus ChainGraph ChainGraph
        -- ^ Test instance for confirming the behaviour of our Consensus
        -- mechanisms (Cut processing, Header validation, etc.), where:
        --
        --   * the underlying `ChainGraph` is configurable,
        --   * the genesis block time is the Linux epoch,
        --   * each `HashTarget` is maxBound,
        --   * each mining `Nonce` is constant,
        --   * the creationTime of `BlockHeader`s is the actual time,
        --   * POW is simulated by poison process thread delay, and
        --   * there are /no/ Pact or mempool operations running.
        --
        -- This is primarily used in our @slow-tests@ executable.
        --

    | PowConsensus ChainGraph
        -- ^ Test instance for confirming the behaviour of the Proof-of-Work
        -- mining algorithm and Difficulty Adjustment, where:
        --
        --   * the underlying `ChainGraph` is configurable,
        --   * the genesis block time the current time,
        --   * the genesis `HashTarget` is 7 bits lower than maxBound,
        --   * the `Nonce` changes with each mining attempt,
        --   * creationTime of BlockHeaders is the actual time, and
        --   * there are /no/ Pact or mempool operations running.
        --
        -- This is primarily used in our @slow-tests@ executable.
        --

    | TimedCPM ChainGraph
        -- ^ Test instance for confirming the combined behaviour of our Consensus
        -- mechanisms, Pact code processing and validation, and Mempool, where:
        --
        --   * the underlying `ChainGraph` is configurable,
        --   * the genesis block time is the Linux epoch,
        --   * each `HashTarget` is maxBound,
        --   * each mining `Nonce` is constant,
        --   * the creationTime of `BlockHeader`s is the actual time,
        --   * POW is simulated by poison process thread delay, and
        --   * the Pact Service and Mempool operations are running.
        --
        -- This is primarily used in our @run-nodes@ executable.
        --

    | FastTimedCPM ChainGraph
        -- ^ Test instance for confirming the combined behaviour of our Consensus
        -- mechanisms, Pact code processing and validation, and Mempool, where:
        --
        -- * the underlying `ChainGraph` is configurable,
        -- * the genesis block time is the Linux epoch,
        -- * each `HashTarget` is maxBound,
        -- * each mining `Nonce` is constant,
        -- * the creationTime of `BlockHeader`'s is the actual time,
        -- * POW is not simulated by poison process thread delay, and
        -- * the Pact Service and Mempool operations are running.
        --
        -- This is primarily used in our @standalone@ executable.
        --

    ------------------------
    -- DEVELOPMENT INSTANCES
    ------------------------
    | Development
        -- ^ An instance which has no guarantees about the long-term stability
        -- of its parameters. They are free to change as developers require.

    -----------------------
    -- PRODUCTION INSTANCES
    -----------------------
    | Testnet04
    | Mainnet01
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
chainwebVersionId v@Test{} = toTestChainwebVersionId v
chainwebVersionId v@TimedConsensus{} = toTestChainwebVersionId v
chainwebVersionId v@PowConsensus{} = toTestChainwebVersionId v
chainwebVersionId v@TimedCPM{} = toTestChainwebVersionId v
chainwebVersionId v@FastTimedCPM{} = toTestChainwebVersionId v
chainwebVersionId Development = 0x00000001
chainwebVersionId Testnet04 = 0x00000007
chainwebVersionId Mainnet01 = 0x00000005
{-# INLINABLE chainwebVersionId #-}

fromChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromChainwebVersionId 0x00000001 = Development
fromChainwebVersionId 0x00000007 = Testnet04
fromChainwebVersionId 0x00000005 = Mainnet01
fromChainwebVersionId i = fromTestChainwebVersionId i
{-# INLINABLE fromChainwebVersionId #-}

encodeChainwebVersion :: ChainwebVersion -> Put
encodeChainwebVersion = putWord32le . chainwebVersionId
{-# INLINABLE encodeChainwebVersion #-}

decodeChainwebVersion :: Get ChainwebVersion
decodeChainwebVersion = fromChainwebVersionId <$> getWord32le
{-# INLINABLE decodeChainwebVersion #-}

instance ToJSON ChainwebVersion where
    toJSON = toJSON . toText
    toEncoding = toEncoding . toText
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON ChainwebVersion where
    parseJSON = parseJsonFromText "ChainwebVersion"
    {-# INLINE parseJSON #-}

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag ChainwebVersion where
    type Tag ChainwebVersion = 'ChainwebVersionTag
    toMerkleNode = encodeMerkleInputNode encodeChainwebVersion
    fromMerkleNode = decodeMerkleInputNode decodeChainwebVersion
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
chainwebVersionToText :: HasCallStack => ChainwebVersion -> T.Text
chainwebVersionToText Development = "development"
chainwebVersionToText Testnet04 = "testnet04"
chainwebVersionToText Mainnet01 = "mainnet01"
chainwebVersionToText (Test g) = "test-" <> toText g
chainwebVersionToText (TimedConsensus g1 g2) = "timedConsensus-" <> toText g1 <> "-" <> toText g2
chainwebVersionToText (PowConsensus g) =  "powConsensus-" <> toText g
chainwebVersionToText (TimedCPM g) =  "timedCPM-" <> toText g
chainwebVersionToText (FastTimedCPM g) =  "fastTimedCPM-" <> toText g
{-# INLINABLE chainwebVersionToText #-}

-- | Read textual representation of a `ChainwebVersion`.
--
-- NOTE: This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
--
chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText "development" = pure Development
chainwebVersionFromText "testnet04" = pure Testnet04
chainwebVersionFromText "mainnet01" = pure Mainnet01
chainwebVersionFromText t = case T.splitOn "-" t of
    [ "test", g ] -> Test <$> fromText g
    [ "timedConsensus", g1, g2 ] ->  TimedConsensus <$> fromText g1 <*> fromText g2
    [ "powConsensus", g ] -> PowConsensus <$> fromText g
    [ "timedCPM", g ] -> TimedCPM <$> fromText g
    [ "fastTimedCPM", g ] -> FastTimedCPM <$> fromText g
    _ -> throwM . TextFormatException $ "Unknown Chainweb version: " <> t

instance HasTextRepresentation ChainwebVersion where
    toText = chainwebVersionToText
    {-# INLINE toText #-}
    fromText = chainwebVersionFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Test instances
--
-- The code in this section must not be called in production.
--

data GraphPos = P1 | P2 deriving (Bounded, Enum)

graphToCodeN :: GraphPos -> KnownGraph -> Word32
graphToCodeN p g = shiftL (graphToCode g) (4 * (4 + fromEnum p))
  where
    graphToCode :: KnownGraph -> Word32
    graphToCode Singleton = 0x00000001
    graphToCode Pair = 0x00000002
    graphToCode Triangle = 0x00000003
    graphToCode Peterson = 0x00000004
    graphToCode Twenty = 0x00000005
    graphToCode HoffmanSingleton = 0x00000006

codeToGraphN :: HasCallStack => GraphPos -> Word32 -> KnownGraph
codeToGraphN p c = codeToGraph (shiftR c (4 * (4 + fromEnum p)) .&. 0x0000000f)
  where
    codeToGraph :: HasCallStack => Word32 -> KnownGraph
    codeToGraph 0x00000001 = Singleton
    codeToGraph 0x00000002 = Pair
    codeToGraph 0x00000003 = Triangle
    codeToGraph 0x00000004 = Peterson
    codeToGraph 0x00000005 = Twenty
    codeToGraph 0x00000006 = HoffmanSingleton
    codeToGraph _ = error "Unknown Graph Code"

toTestChainwebVersionId :: HasCallStack => ChainwebVersion -> Word32
toTestChainwebVersionId (Test g) = 0x80000000
    .|. graphToCodeN P1 (view chainGraphKnown g)
toTestChainwebVersionId (TimedConsensus g1 g2) = 0x80000001
    .|. graphToCodeN P1 (view chainGraphKnown g1)
    .|. graphToCodeN P2 (view chainGraphKnown g2)
toTestChainwebVersionId (PowConsensus g) = 0x80000002
    .|. graphToCodeN P1 (view chainGraphKnown g)
toTestChainwebVersionId (TimedCPM g) = 0x80000003
    .|. graphToCodeN P1 (view chainGraphKnown g)
toTestChainwebVersionId (FastTimedCPM g) = 0x80000004
    .|. graphToCodeN P1 (view chainGraphKnown g)
toTestChainwebVersionId Development =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"
toTestChainwebVersionId Testnet04 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"
toTestChainwebVersionId Mainnet01 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"

fromTestChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromTestChainwebVersionId c = case v of
    0x80000000 -> Test (knownChainGraph $ codeToGraphN P1 g)
    0x80000001 -> TimedConsensus
        (knownChainGraph $ codeToGraphN P1 g)
        (knownChainGraph $ codeToGraphN P2 g)
    0x80000002 -> PowConsensus (knownChainGraph $ codeToGraphN P1 g)
    0x80000003 -> TimedCPM (knownChainGraph $ codeToGraphN P1 g)
    0x80000004 -> FastTimedCPM (knownChainGraph $ codeToGraphN P1 g)
    _ -> error "Unknown ChainwebVersion Code"
  where
    (v, g) = (0xf000ffff .&. c, 0x0fff0000 .&. c)

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

    {-# INLINE fromSing #-}
    {-# INLINE toSing #-}

pattern FromSingChainwebVersion :: Sing (n :: ChainwebVersionT) -> ChainwebVersion
pattern FromSingChainwebVersion sng <- ((\v -> withSomeSing v SomeSing) -> SomeSing sng)
  where FromSingChainwebVersion sng = fromSing sng
{-# COMPLETE FromSingChainwebVersion #-}

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

-- | All known chainIds. This includes chains that are not yet "active".
--
chainIds :: HasChainwebVersion v => v -> HS.HashSet ChainId
chainIds = graphChainIds . snd . NE.head . chainwebGraphs . _chainwebVersion
{-# INLINE chainIds #-}

mkChainId
    :: MonadThrow m
    => HasChainwebVersion v
    => Integral i
    => v
    -> BlockHeight
    -> i
    -> m ChainId
mkChainId v h i = cid
    <$ checkWebChainId (chainGraphAt (_chainwebVersion v) h) cid
  where
    cid = unsafeChainId (fromIntegral i)
{-# INLINE mkChainId #-}

-- -------------------------------------------------------------------------- --
-- Properties of Chainweb Versions
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Graph

-- | Graphs of chainweb version
--
-- Invariants:
--
-- * Entries are sorted by 'BlockHeight' in decreasing order.
-- * The last entry is for 'BlockHeight' 0.
-- * The graphs decrease in order.
--
-- The functions provided in 'Chainweb.Version.Utils' are generally more
-- convenient to use than this function.
--
chainwebGraphs :: ChainwebVersion -> NE.NonEmpty (BlockHeight, ChainGraph)
chainwebGraphs (Test g) = pure (0, g)
chainwebGraphs (TimedConsensus g1 g2) = (8, g2) NE.:| [ (0, g1) ]
chainwebGraphs (PowConsensus g) = pure (0, g)
chainwebGraphs (TimedCPM g) = pure (0, g)
chainwebGraphs (FastTimedCPM g) = pure (0, g)
chainwebGraphs Testnet04 =
    ( to20ChainsTestnet, twentyChainGraph ) NE.:|
    [ ( 0, petersonChainGraph ) ]
chainwebGraphs Mainnet01 =
    ( to20ChainsMainnet, twentyChainGraph ) NE.:|
    [ ( 0, petersonChainGraph ) ]
chainwebGraphs Development =
    ( to20ChainsDevelopment, twentyChainGraph ) NE.:|
    [ ( 0, petersonChainGraph ) ]
{-# INLINE chainwebGraphs #-}

to20ChainsMainnet :: BlockHeight
to20ChainsMainnet = 852_054 -- 2020-08-20 16:00:00

to20ChainsTestnet :: BlockHeight
to20ChainsTestnet = 332_604 -- 2020-07-28 16:00:00

to20ChainsDevelopment :: BlockHeight
to20ChainsDevelopment = 60

-- | Return the Graph History at a given block height in descending order.
--
-- The functions provided in 'Chainweb.Version.Utils' are generally more
-- convenient to use than this function.
--
-- This function is safe because of invariants provided by 'chainwebGraphs'.
-- (There are also unit tests the confirm this.)
--
chainwebGraphsAt
    :: HasCallStack
    => ChainwebVersion
    -> BlockHeight
    -> NE.NonEmpty (BlockHeight, ChainGraph)
chainwebGraphsAt v h = NE.fromList
    $ NE.dropWhile ((> h) . fst)
    $ chainwebGraphs v
{-# INLINE chainwebGraphsAt #-}

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt :: HasCallStack => ChainwebVersion -> BlockHeight -> ChainGraph
chainGraphAt v = snd . NE.head . chainwebGraphsAt v
{-# INLINE chainGraphAt #-}

-- | The 'ChainGraph' for the given 'BlockHeight'
--
chainGraphAt_
    :: HasCallStack
    => HasChainwebVersion v
    => v
    -> BlockHeight
    -> ChainGraph
chainGraphAt_ = chainGraphAt . _chainwebVersion
{-# INLINE chainGraphAt_ #-}

-- | The genesis graph for a given Chain
--
-- Invariant:
--
-- * The given ChainId exists in the first graph of the graph history.
--   (We generally assume that this invariant holds throughout the code base.
--   It is enforced via the 'mkChainId' smart constructor for ChainId.)
--
genesisGraph
    :: HasCallStack
    => HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> ChainGraph
genesisGraph v = chainGraphAt v_ . genesisHeight v_ . _chainId
  where
    v_ = _chainwebVersion v
{-# INLINE genesisGraph #-}

instance HasChainGraph (ChainwebVersion, BlockHeight) where
    _chainGraph = uncurry chainGraphAt
    {-# INLINE _chainGraph #-}

-- -------------------------------------------------------------------------- --
-- Genesis Height

-- | Returns the height of the genesis block for a chain.
--
-- The implementation is somewhat expensive. With the current number of chains
-- this isn't an issue. Otherwise the result should be hardcoded or memoized.
--
-- TODO: memoize the genesis header for the production versions. Give this
-- function a less attractive name and instead use this name to return the
-- block height from the genesis header.
--
-- Invariant:
--
-- * The given ChainId exists in the first graph of the graph history.
--   (We generally assume that this invariant holds throughout the code base.
--   It is enforced via the 'mkChainId' smart constructor for ChainId.)
--
genesisHeight :: HasCallStack => ChainwebVersion -> ChainId -> BlockHeight
genesisHeight v c = fst
    $ head
    $ NE.dropWhile (not . flip isWebChain c . snd)
    $ NE.reverse (chainwebGraphs v)

-- -------------------------------------------------------------------------- --
-- POW Parameters

-- | The gap in SECONDS that we desire between the Creation Time of subsequent
-- blocks in some chain.
--
newtype BlockRate = BlockRate { _getBlockRate :: Seconds }

-- | The Proof-of-Work `BlockRate` for each `ChainwebVersion`. This is the
-- number of seconds we expect to pass while a miner mines on various chains,
-- eventually succeeding on one.
--
blockRate :: ChainwebVersion -> BlockRate
blockRate Test{} = BlockRate 0
blockRate TimedConsensus{} = BlockRate 4
blockRate PowConsensus{} = BlockRate 10
blockRate TimedCPM{} = BlockRate 4
blockRate FastTimedCPM{} = BlockRate 1
-- 120 blocks per hour, 2,880 per day, 20,160 per week, 1,048,320 per year.
blockRate Testnet04 = BlockRate 30
blockRate Mainnet01 = BlockRate 30
blockRate Development = BlockRate $ maybe 30 int customeDevnetRate

customeDevnetRate :: Maybe Int
customeDevnetRate =
    readMaybe =<< unsafeDupablePerformIO (lookupEnv "DEVELOPMENT_BLOCK_RATE")
{-# NOINLINE customeDevnetRate #-}

-- | The number of blocks to be mined after a difficulty adjustment, before
-- considering a further adjustment. Critical for the "epoch-based" adjustment
-- algorithm seen in `adjust`.
--
newtype WindowWidth = WindowWidth Natural

-- | The Proof-of-Work `WindowWidth` for each `ChainwebVersion`. For chainwebs
-- that do not expect to perform POW, this should be `Nothing`.
--
window :: ChainwebVersion -> Maybe WindowWidth
window Test{} = Nothing
window TimedConsensus{} = Nothing
-- 5 blocks, should take 50 seconds.
window PowConsensus{} = Just $ WindowWidth 8
window TimedCPM{} = Nothing
window FastTimedCPM{} = Nothing
-- 120 blocks, should take 1 hour given a 30 second BlockRate.
window Development = Just $ WindowWidth 120
-- 120 blocks, should take 1 hour given a 30 second BlockRate.
window Testnet04 = Just $ WindowWidth 120
window Mainnet01 = Just $ WindowWidth 120

-- -------------------------------------------------------------------------- --
-- Header Serialization

-- | The size in bytes of the constant portion of the serialized header. This is
-- the header /without/ the adjacent hashes.
--
-- NOTE: This is an internal function. For the actual size of the serialized header
-- use 'headerSizeBytes'.
--
headerBaseSizeBytes :: ChainwebVersion -> Natural
headerBaseSizeBytes _ = 318 - 110

-- | This is an internal function. Use 'headerSizeBytes' instead.
--
-- Postconditions: for all @v@
--
-- * @not . null $ headerSizes v@, and
-- * @0 == (fst . last) (headerSizes v)@.
--
-- Note that for all but genesis headers the number of adjacent hashes depends
-- on the graph of the parent.
--
headerSizes :: ChainwebVersion -> NE.NonEmpty (BlockHeight, Natural)
headerSizes v = fmap (\g -> headerBaseSizeBytes v + 36 * degree g + 2) <$> chainwebGraphs v

-- | The size of the serialized block header.
--
-- This function is safe because of the invariant of 'headerSize' that there
-- exists and entry for block height 0.
--
-- Note that for all but genesis headers the number of adjacent hashes depends
-- on the graph of the parent.
--
headerSizeBytes
    :: HasCallStack
    => ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Natural
headerSizeBytes v cid h = snd
    $ head
    $ NE.dropWhile ((> relevantHeight) . fst)
    $ headerSizes v
  where
    relevantHeight
        | genesisHeight v cid == h = h
        | otherwise = h - 1
{-# INLINE headerSizeBytes #-}

-- | The size of the work bytes /without/ the preamble of the chain id and target
--
-- The chain graph, and therefore also the header size, is constant for all
-- blocks at the same height except for genesis blocks. Because genesis blocks
-- are never mined, we can ignore this difference here and just return the
-- result for chain 0.
--
-- NOTE: For production versions we require that the value is constant for a
-- given chainweb version. This would only ever change as part of the
-- introduction of new block header format.
--
workSizeBytes
    :: HasCallStack
    => ChainwebVersion
    -> BlockHeight
    -> Natural
workSizeBytes v h = headerSizeBytes v (unsafeChainId 0) h - 32
{-# INLINE workSizeBytes #-}

-- -------------------------------------------------------------------------- --
-- Pact Validation Parameters

-- | This the hard upper limit of the gas within a block. Blocks that use more
-- gas are invalid and rejected. This limit is needed as a DOS protection.
--
-- Smaller limits can be configured for creating new blocks.
--
-- Before the chainweb-node 2.16 fork, there was no maximum block gas limit.
--
maxBlockGasLimit
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Maybe Natural
maxBlockGasLimit Mainnet01 _ bh = 180000 <$ guard (chainweb216Pact After Mainnet01 bh)
maxBlockGasLimit Testnet04 _ bh = 180000 <$ guard (chainweb216Pact After Testnet04 bh)
maxBlockGasLimit Development _ _ = Just 180000
maxBlockGasLimit _ _ _ = Just 2_000000

-- -------------------------------------------------------------------------- --
-- Pact Validation Guards

-- | Mainnet applied vlun797Fix at @[timeMicrosQQ| 2019-12-10T21:00:00.0 |]@.
--
-- This function provides the block heights when the fix became effective on the
-- respective chains.
--
vuln797Fix
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Bool
vuln797Fix Mainnet01 cid h
    | cid == unsafeChainId 0 = h >= 121452
    | cid == unsafeChainId 1 = h >= 121452
    | cid == unsafeChainId 2 = h >= 121452
    | cid == unsafeChainId 3 = h >= 121451
    | cid == unsafeChainId 4 = h >= 121451
    | cid == unsafeChainId 5 = h >= 121452
    | cid == unsafeChainId 6 = h >= 121452
    | cid == unsafeChainId 7 = h >= 121451
    | cid == unsafeChainId 8 = h >= 121452
    | cid == unsafeChainId 9 = h >= 121451
vuln797Fix _ _ _ = True
{-# INLINE vuln797Fix #-}

-- | Mainnet upgraded to coin v2 at time at @[timeMicrosQQ| 2019-12-17T15:00:00.0 |]@.
--
-- This function provides the block heights when coin v2 became effective on the
-- respective chains.
--
coinV2Upgrade
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Bool
coinV2Upgrade Mainnet01 cid h
    | cid == unsafeChainId 0 = h == 140808
    | cid == unsafeChainId 1 = h == 140809
    | cid == unsafeChainId 2 = h == 140808
    | cid == unsafeChainId 3 = h == 140809
    | cid == unsafeChainId 4 = h == 140808
    | cid == unsafeChainId 5 = h == 140808
    | cid == unsafeChainId 6 = h == 140808
    | cid == unsafeChainId 7 = h == 140809
    | cid == unsafeChainId 8 = h == 140808
    | cid == unsafeChainId 9 = h == 140808
    -- new chains on mainnet start already with v2 deployed in the genesis block
coinV2Upgrade Testnet04 cid h
    | chainIdInt @Int cid >= 10  && chainIdInt @Int cid < 20 = h == 337000
    | otherwise = h == 1
coinV2Upgrade Development cid h
    | cid == unsafeChainId 0 = h == 3
    | otherwise = h == 4
coinV2Upgrade _ _ 1 = True
coinV2Upgrade _ _ _ = False

-- | 20-chain rebalance
--
-- This function provides the block heights when remediations will be applied
-- to correspond to genesis grants in the new chains.
--
to20ChainRebalance
    :: ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> Bool
to20ChainRebalance Mainnet01 _ h = h == to20ChainsMainnet
to20ChainRebalance Testnet04 _ h = h == to20ChainsTestnet
to20ChainRebalance Development _ h = h == to20ChainsDevelopment
to20ChainRebalance _ _ 2 = True
to20ChainRebalance _ _ _ = False

-- | Preserve Pact bugs pre 1.6 chainweb version
-- Mainnet 328000 ~ UTC Feb 20 15:36, EST Feb 20 10:56
--
pactBackCompat_v16 :: ChainwebVersion -> BlockHeight -> Bool
pactBackCompat_v16 Mainnet01 h = h < 328000
pactBackCompat_v16 _ _ = False

-- | Early versions of chainweb used the creation time of the current header
-- for validation of pact tx creation time and TTL. Nowadays the times of
-- the parent header a used.
--
-- When this guard is enabled timing validation is skipped.
--
skipTxTimingValidation :: ChainwebVersion -> BlockHeight -> Bool
skipTxTimingValidation Mainnet01 h = h < 449940 -- ~ 2020-04-03T00:00:00Z
skipTxTimingValidation _ h = h <= 1
    -- For most chainweb versions there is a large gap between creation times of
    -- the genesis blocks and the corresponding first blocks.
    --
    -- Some tests fake block heights without updating pdData appropriately. This
    -- causes tx validation at height 1, even though the block height is larger.
    -- By using the current header time for the block of height <= 1 we relax
    -- the tx timing checks a bit.

-- | Checks height after which module name fix in effect.
--
enableModuleNameFix :: ChainwebVersion -> BlockHeight -> Bool
enableModuleNameFix Mainnet01 bh = bh >= 448501 -- ~ 2020-04-02T12:00:00Z
enableModuleNameFix _ bh = bh >= 2

-- | Related, later fix (Pact #801)
--
enableModuleNameFix2 :: ChainwebVersion -> BlockHeight -> Bool
enableModuleNameFix2 Mainnet01 bh = bh >= 752214 -- ~ 2020-07-17 0:00:00 UTC
enableModuleNameFix2 Testnet04 bh = bh >= 289966 -- ~ 2020-07-13
enableModuleNameFix2 _ bh = bh >= 2

-- | Turn on pact events in command output.
enablePactEvents :: ChainwebVersion -> BlockHeight -> Bool
enablePactEvents Mainnet01 bh = bh >= 1138000
enablePactEvents Testnet04 bh = bh >= 660000
enablePactEvents Development bh = bh >= 40
enablePactEvents (FastTimedCPM g) bh
    | g == singletonChainGraph || g == pairChainGraph = True
    | g == petersonChainGraph = bh > 10
    | otherwise = False
enablePactEvents _ bh = bh >= 2

-- | Bridge support: ETH and event SPV.
enableSPVBridge :: ChainwebVersion -> BlockHeight -> Bool
enableSPVBridge Mainnet01 = (>= 1_275_000) -- 2021-01-14T16:35:58
enableSPVBridge Testnet04 = (>= 820_000) -- 2021-01-14T17:12:02
enableSPVBridge Development = (>= 50)
enableSPVBridge (FastTimedCPM g) = const $ g == pairChainGraph || g == petersonChainGraph
enableSPVBridge _ = const True

data AtOrAfter = At | After deriving (Eq,Show)

-- | Pact 4 / coin v3 fork
pact4coin3Upgrade :: AtOrAfter -> ChainwebVersion -> BlockHeight -> Bool
pact4coin3Upgrade aoa v h = case aoa of
    At -> go (==) v h
    After -> go (<) v h
  where
    go f Mainnet01 = f 1_722_500 -- 2021-06-19T03:34:05
    go f Testnet04 = f 1_261_000 -- 2021-06-17T15:54:14
    go f Development = f 80
    go f (FastTimedCPM g) | g == petersonChainGraph = f 20
    go f _ = f 4
    -- lowering this number causes some tests in Test.Pact.SPV to fail

pact420Upgrade :: ChainwebVersion -> BlockHeight -> Bool
pact420Upgrade Mainnet01 = (>= 2_334_500) -- 2022-01-17T17:51:12
pact420Upgrade Testnet04 = (>= 1_862_000) -- 2022-01-13T16:11:10
pact420Upgrade Development = (>= 90)
pact420Upgrade (FastTimedCPM g) | g == petersonChainGraph = (>= 5)
pact420Upgrade _ = const True

enforceKeysetFormats :: ChainwebVersion -> BlockHeight -> Bool
enforceKeysetFormats Mainnet01 = (>= 2_162_000) -- 2021-11-18T20:06:55
enforceKeysetFormats Testnet04 = (>= 1_701_000) -- 2021-11-18T17:54:36
enforceKeysetFormats Development = (>= 100)
enforceKeysetFormats (FastTimedCPM g) | g == petersonChainGraph = (>= 10)
enforceKeysetFormats _ = const True

doCheckTxHash :: ChainwebVersion -> BlockHeight -> Bool
doCheckTxHash Mainnet01 = (>= 2_349_800) -- 2022-01-23T02:53:38
doCheckTxHash Testnet04 = (>= 1_889_000) -- 2022-01-24T04:19:24
doCheckTxHash Development = (>= 110)
doCheckTxHash (FastTimedCPM g) | g == petersonChainGraph = (>= 7)
doCheckTxHash _ = const True

-- | Pact changes for Chainweb 2.13
--
chainweb213Pact :: ChainwebVersion -> BlockHeight -> Bool
chainweb213Pact Mainnet01 = (>= 2_447_315) -- 2022-02-26 00:00:00
chainweb213Pact Testnet04 = (>= 1_974_556) -- 2022-02-25 00:00:00
chainweb213Pact Development = (>= 95)
chainweb213Pact (FastTimedCPM g) | g == petersonChainGraph = (> 25)
chainweb213Pact _ = const True

-- | Fork for musl trans funs
pact44NewTrans :: ChainwebVersion -> BlockHeight -> Bool
pact44NewTrans Mainnet01 = (>= 2_965_885) -- Todo: add date
pact44NewTrans Testnet04 = (>= 2_500_369) -- Todo: add date
pact44NewTrans _ = const True

-- | Pact and coin contract changes for Chainweb 2.14
--
chainweb214Pact
    :: AtOrAfter
    -> ChainwebVersion
    -> BlockHeight
    -> Bool
chainweb214Pact aoa v h = case aoa of
    At -> go (==) v h
    After -> go (<) v h
  where
    go f Mainnet01 = f 2605663 -- 2022-04-22T00:00:00Z
    go f Testnet04 = f 2134331 -- 2022-04-21T12:00:00Z
    go f Development = f 115
    go f (FastTimedCPM g) | g == petersonChainGraph = f 30
    go f _ = f 5

-- | Pact and coin contract changes for Chainweb 2.15
--
chainweb215Pact
    :: AtOrAfter
    -> ChainwebVersion
    -> BlockHeight
    -> Bool
chainweb215Pact aoa v h = case aoa of
    At -> go (==) v h
    After -> go (<) v h
  where
    go f Mainnet01 = f 2766630 -- 2022-06-17T00:00:00+00:00
    go f Testnet04 = f 2295437 -- 2022-06-16T12:00:00+00:00
    go f Development = f 165
    go f (FastTimedCPM g) | g == petersonChainGraph = f 35
    go f _ = f 10

-- | Pact and coin contract changes for Chainweb 2.16
--
chainweb216Pact
    :: AtOrAfter
    -> ChainwebVersion
    -> BlockHeight
    -> Bool
chainweb216Pact aoa v h = case aoa of
    At -> go (==) v h
    After -> go (<) v h
  where
    go f Mainnet01 = f 2988324 -- 2022-09-02 00:00:00+00:00
    go f Testnet04 = f 2516739 -- 2022-09-01 12:00:00+00:00
    go f Development = f 215
    go f (FastTimedCPM g) | g == petersonChainGraph = f 53
    go f _ = f 16

-- -------------------------------------------------------------------------- --
-- Header Validation Guards
--
-- The guards in this section encode when changes to validation rules for data
-- on the chain become effective.
--
-- Only the following types are allowed as parameters for guards
--
-- * BlockHeader,
-- * ParentHeader,
-- * BlockCreationTime, and
-- * ParentCreationTime
--
-- The result is a simple 'Bool'.
--
-- Guards should have meaningful names and should be used in a way that all
-- places in the code base that depend on the guard should reference the
-- respective guard. That way all dependent code can be easily identified using
-- ide tools, like for instance @grep@.
--
-- Each guard should have a description that provides background for the change
-- and provides all information needed for maintaining the code or code that
-- depends on it.
--

-- | Turn off slow epochs (emergency DA) for blocks from 80,000 onwward.
--
-- Emergency DA is considered a miss-feature.
--
-- It's intended purpose is to prevent chain hopping attacks, where an attacker
-- temporarily adds a large amount of hash power, thus increasing the
-- difficulty. When the hash power is removed, the remaining hash power may not
-- be enough to reach the next block in reasonable time.
--
-- In practice, emergency DAs cause more problems than they solve. In
-- particular, they increase the chance of deep forks. Also they make the
-- behavior of the system unpredictable in states of emergency, when stability
-- is usually more important than throughput.
--
slowEpochGuard
    :: ChainwebVersion
    -> BlockHeight
        -- ^ BlockHeight of parent Header
    -> Bool
slowEpochGuard Mainnet01 h = h < 80000
slowEpochGuard _ _ = False
{-# INLINE slowEpochGuard #-}

-- | Use the current block time for computing epoch start date and
-- target.
--
-- When this guard is switched off, there will be a single epoch of just 119
-- blocks. The target computation won't compensate for that, since the effects
-- are marginal.
--
oldTargetGuard :: ChainwebVersion -> BlockHeight -> Bool
oldTargetGuard Mainnet01 h = h < 452820 -- ~ 2020-04-04T00:00:00Z
oldTargetGuard _ _ = False
{-# INLINE oldTargetGuard #-}

-- | Skip validation of feature flags.
--
-- Unused feature flag bits are supposed to be set to 0. As of Chainweb 1.7, the
-- Feature Flag bytes and Nonce bytes have switched places in `BlockHeader`. For
-- live chains, enforcing the following condition must be ignored for the
-- historical blocks for which both the Nonce and Flags could be anything.
--
skipFeatureFlagValidationGuard :: ChainwebVersion -> BlockHeight -> Bool
skipFeatureFlagValidationGuard Mainnet01 h = h < 530500  -- ~ 2020-05-01T00:00:xxZ
skipFeatureFlagValidationGuard _ _ = False

oldDaGuard :: ChainwebVersion -> BlockHeight -> Bool
oldDaGuard Mainnet01 h = h < 771_414 -- ~ 2020-07-23 16:00:00
oldDaGuard Testnet04 h = h < 318_204 -- ~ 2020-07-23 16:00:00
oldDaGuard Development h = h < 13 -- after DA at 10
oldDaGuard _ _ = False
