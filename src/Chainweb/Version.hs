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
{-# LANGUAGE QuasiQuotes #-}
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
, chainwebVersionFromText
, chainwebVersionToText
, chainwebVersionId

-- * Properties of Chainweb Version
-- ** POW
, BlockRate(..)
, blockRate
, WindowWidth(..)
, window
-- ** Date- and Version-based Transaction Disabling
, txEnabledDate
, transferActivationDate
, enableUserContracts
, vuln797FixDate
, upgradeCoinV2Date

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

import Numeric.Natural

import System.Random

-- internal modules

import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
import Chainweb.MerkleUniverse
import Chainweb.Time
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

    | TimedConsensus ChainGraph
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
chainwebVersionId v@Test{} = toTestChainwebVersion v
chainwebVersionId v@TimedConsensus{} = toTestChainwebVersion v
chainwebVersionId v@PowConsensus{} = toTestChainwebVersion v
chainwebVersionId v@TimedCPM{} = toTestChainwebVersion v
chainwebVersionId v@FastTimedCPM{} = toTestChainwebVersion v
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

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
chainwebVersionToText :: HasCallStack => ChainwebVersion -> T.Text
chainwebVersionToText Development = "development"
chainwebVersionToText Testnet04 = "testnet04"
chainwebVersionToText Mainnet01 = "mainnet01"
chainwebVersionToText v = fromJuste $ HM.lookup v prettyVersions
{-# INLINABLE chainwebVersionToText #-}

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
-- | Read textual representation of a `ChainwebVersion`.
--
chainwebVersionFromText :: MonadThrow m => T.Text -> m ChainwebVersion
chainwebVersionFromText "development" = pure Development
chainwebVersionFromText "testnet04" = pure Testnet04
chainwebVersionFromText "mainnet01" = pure Mainnet01
chainwebVersionFromText t =
    case HM.lookup t chainwebVersions of
        Just v -> pure v
        Nothing -> case t of
            "test" -> pure $ Test petersonChainGraph
            "timedConsensus" -> pure $ TimedConsensus petersonChainGraph
            "powConsensus" -> pure $ PowConsensus petersonChainGraph
            "timedCPM" -> pure $ TimedCPM petersonChainGraph
            _ -> throwM . TextFormatException $ "Unknown Chainweb version: " <> t

instance HasTextRepresentation ChainwebVersion where
    toText = chainwebVersionToText
    {-# INLINE toText #-}
    fromText = chainwebVersionFromText
    {-# INLINE fromText #-}

-- -------------------------------------------------------------------------- --
-- Value Maps

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
chainwebVersions :: HM.HashMap T.Text ChainwebVersion
chainwebVersions = HM.fromList $
    f Test "test"
    <> f TimedConsensus "timedConsensus"
    <> f PowConsensus "powConsensus"
    <> f TimedCPM "timedCPM"
    <> f FastTimedCPM "fastTimedCPM"
    <> [ ("development", Development)
       , ("testnet04", Testnet04)
       , ("mainnet01", Mainnet01)
       ]
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
codeToTestVersion 0x80000003 = TimedCPM
codeToTestVersion 0x80000004 = FastTimedCPM
codeToTestVersion _ = error "Unknown ChainwebVersion Code"

testVersionToCode :: ChainwebVersion -> Word32
testVersionToCode Test{} = 0x80000000
testVersionToCode TimedConsensus{} = 0x80000001
testVersionToCode PowConsensus{} = 0x80000002
testVersionToCode TimedCPM{} = 0x80000003
testVersionToCode FastTimedCPM{} = 0x80000004
testVersionToCode Development =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"
testVersionToCode Testnet04 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"
testVersionToCode Mainnet01 =
    error "Illegal ChainwebVersion passed to toTestChainwebVersion"

fromTestChainwebVersionId :: HasCallStack => Word32 -> ChainwebVersion
fromTestChainwebVersionId i =
    uncurry ($) . bimap codeToTestVersion (knownGraph . codeToGraph) $ splitTestCode i

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

-- -------------------------------------------------------------------------- --
-- Properties of Chainweb Versions
-- -------------------------------------------------------------------------- --

-- -------------------------------------------------------------------------- --
-- Graph

chainwebVersionGraph :: ChainwebVersion -> ChainGraph
chainwebVersionGraph (Test g) = g
chainwebVersionGraph (TimedConsensus g) = g
chainwebVersionGraph (PowConsensus g) = g
chainwebVersionGraph (TimedCPM g) = g
chainwebVersionGraph (FastTimedCPM g) = g
chainwebVersionGraph Development = petersonChainGraph
chainwebVersionGraph Testnet04 = petersonChainGraph
chainwebVersionGraph Mainnet01 = petersonChainGraph

instance HasChainGraph ChainwebVersion where
    _chainGraph = chainwebVersionGraph
    {-# INLINE _chainGraph #-}

-- -------------------------------------------------------------------------- --
-- POW Parameters

-- | The gap in SECONDS that we desire between the Creation Time of subsequent
-- blocks in some chain.
--
newtype BlockRate = BlockRate Seconds

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
blockRate Development = BlockRate 30
blockRate Testnet04 = BlockRate 30
blockRate Mainnet01 = BlockRate 30

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

-- | This is used in a core validation rule and has been present for several
-- versions of the node software. Changing it risks a fork in the network.
-- Must be AFTER 'upgradeCoinV2Date', and BEFORE 'transferActivationDate' in mainnet.
--
txEnabledDate :: ChainwebVersion -> Maybe (Time Micros)
txEnabledDate Test{} = Nothing
txEnabledDate TimedConsensus{} = Nothing
txEnabledDate PowConsensus{} = Nothing
txEnabledDate TimedCPM{} = Nothing
txEnabledDate FastTimedCPM{} = Nothing
txEnabledDate Development = Just [timeMicrosQQ| 2019-11-30T05:00:00.0 |]
txEnabledDate Testnet04 = Nothing
txEnabledDate Mainnet01 = Just [timeMicrosQQ| 2019-12-17T15:30:00.0 |]

-- | KILLSWITCH: The date after which nodes in the 1.1.x series will
-- spontaneously allow Transactions in the system. This constant can be removed
-- once the date has passed, and /must not be used in core validation code/.
-- Must be after 'txEnabledDate' in mainnet.
--
transferActivationDate :: ChainwebVersion -> Maybe (Time Micros)
transferActivationDate Test{} = Nothing
transferActivationDate TimedConsensus{} = Nothing
transferActivationDate PowConsensus{} = Nothing
transferActivationDate TimedCPM{} = Nothing
transferActivationDate FastTimedCPM{} = Nothing
transferActivationDate Development = Just [timeMicrosQQ| 2019-12-13T23:25:00.0 |]
transferActivationDate Testnet04 = Nothing
transferActivationDate Mainnet01 = Just [timeMicrosQQ| 2019-12-17T16:00:00.0 |]

-- | Time after which fixes for vuln797 will be validated in blocks.
--
vuln797FixDate :: ChainwebVersion -> Time (Micros)
vuln797FixDate Test{} = epoch
vuln797FixDate TimedConsensus{} = epoch
vuln797FixDate PowConsensus{} = epoch
vuln797FixDate TimedCPM{} = epoch
vuln797FixDate FastTimedCPM{} = epoch
vuln797FixDate Development = epoch
vuln797FixDate Testnet04 = epoch
vuln797FixDate Mainnet01 = [timeMicrosQQ| 2019-12-10T21:00:00.0 |]
{-# INLINE vuln797FixDate #-}

-- | Enable user contract install
enableUserContracts :: ChainwebVersion -> Bool
enableUserContracts Mainnet01 = False
enableUserContracts _ = True

-- | Upgrade coin v2 at time, or at block height 1
-- | Must be BEFORE 'txEnabledDate' in mainnet.
upgradeCoinV2Date :: ChainwebVersion -> Maybe (Time Micros)
upgradeCoinV2Date Test{} = Nothing
upgradeCoinV2Date TimedConsensus{} = Nothing
upgradeCoinV2Date PowConsensus{} = Nothing
upgradeCoinV2Date TimedCPM{} = Nothing
upgradeCoinV2Date FastTimedCPM{} = Nothing
upgradeCoinV2Date Development = Just [timeMicrosQQ| 2019-12-13T23:15:00.0 |]
upgradeCoinV2Date Testnet04 = Nothing
upgradeCoinV2Date Mainnet01 = Just [timeMicrosQQ| 2019-12-17T15:00:00.0 |]
