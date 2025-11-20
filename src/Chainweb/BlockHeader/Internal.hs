{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHeader
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This module contains the implementation of a 'BlockHeader', and provides
-- read (via record and 'Getter') and write (via record and 'Setter') access to
-- them. Editing or manually constructing 'BlockHeader's outside of tests is dangerous
-- and likely to result in invalid headers, whether through invalid block hashes
-- or invalid adjacent hash records.

-- You should prefer using 'Chainweb.BlockHeader' over this module, unless you
-- are writing tests.
module Chainweb.BlockHeader.Internal
(
-- * Newtype wrappers for function parameters
  ParentHeader(..)
, parentHeader
, parentHeaderHash
, _rankedParentHash
, rankedParentHash
, ParentCreationTime(..)

-- * Block Payload Hash
, BlockPayloadHash
, BlockPayloadHash_(..)
, encodeBlockPayloadHash
, decodeBlockPayloadHash

-- * Nonce
, Nonce(..)
, encodeNonce
, encodeNonceToWord64
, decodeNonce

-- * EpochStartTime
, EpochStartTime(..)
, encodeEpochStartTime
, decodeEpochStartTime
, epochStart

-- * POW Target
, powTarget

-- * BlockHeader
, BlockHeader(..)
-- ** Getters
, blockFlags
, blockCreationTime
, blockParent
, blockAdjacentHashes
, blockTarget
, blockPayloadHash
, blockChainId
, blockWeight
, blockHeight
, blockChainwebVersion
, blockEpochStart
, blockNonce
, blockHash

-- ** Utilities
, _blockPow
, blockPow
, _blockAdjacentChainIds
, blockAdjacentChainIds
, _rankedBlockHash
, rankedBlockHash
, _rankedBlockPayloadHash
, rankedBlockPayloadHash
, encodeBlockHeader
, encodeBlockHeaderWithoutHash
, decodeBlockHeader
, decodeBlockHeaderWithoutHash
, decodeBlockHeaderChecked
, decodeBlockHeaderCheckedChainId
, blockHeaderShortDescription
, ObjectEncoded(..)
, ExtendedObjectEncoded(..)

, timeBetween
, getAdjacentHash
, computeBlockHash
, adjacentChainIds
, absBlockHeightDiff

-- * IsBlockHeader
, IsBlockHeader(..)

-- * Genesis BlockHeader
, isGenesisBlockHeader
, genesisParentBlockHash
, genesisBlockHeader
, genesisBlockHeaders
, genesisBlockHeadersAtHeight
, genesisHeight

-- * Create a new BlockHeader
, newBlockHeader

-- * Fork State
, blockForkState
, _blockForkNumber
, blockForkNumber
, _blockForkVotes
, blockForkVotes
, forkEpochLength
, isForkEpochStart
, isForkCountBlock
, isForkVoteBlock
, newForkState
, isLastForkEpochBlock
, genesisForkState

-- * CAS Constraint
, BlockHeaderCas

-- * Misc
, headerSizes
, headerSizeBytes
, workSizeBytes
) where

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.BlockPayloadHash
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.PowHash
import Chainweb.Storage.Table
import Chainweb.Time
import Chainweb.TreeDB (TreeDbEntry(..))
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Guards
import Chainweb.Version.Mainnet
import Chainweb.Version.Registry (lookupVersionByName, lookupVersionByCode)
import Chainweb.Version.Testnet04
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Crypto.Hash.Algorithms
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable
import Data.IORef
import Data.Kind
import Data.Memory.Endian qualified as BA
import Data.MerkleLog hiding (Actual, Expected, MerkleHash)
import Data.Text qualified as T
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack
import Numeric.AffineSpace
import Numeric.Natural
import System.IO.Unsafe
import Text.Read (readEither)
import Chainweb.ForkState

-- -------------------------------------------------------------------------- --
-- Nonce

newtype Nonce = Nonce Word64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (Hashable,Enum)

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag Nonce where
    type Tag Nonce = 'BlockNonceTag
    toMerkleNode = encodeMerkleInputNode encodeNonce
    fromMerkleNode = decodeMerkleInputNode decodeNonce
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeNonce :: Nonce -> Put
encodeNonce (Nonce n) = putWord64le n

encodeNonceToWord64 :: Nonce -> Word64
encodeNonceToWord64 (Nonce n) = BA.unLE $ BA.toLE n

decodeNonce :: Get Nonce
decodeNonce = Nonce <$> getWord64le

instance ToJSON Nonce where
    toJSON (Nonce i) = toJSON $ show i
    toEncoding (Nonce i) = toEncoding $ show i
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON Nonce where
    parseJSON = withText "Nonce"
        $ either fail (return . Nonce) . readEither . T.unpack

-- -------------------------------------------------------------------------- --
-- POW Target Computation

newtype EpochStartTime = EpochStartTime (Time Micros)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag EpochStartTime where
    type Tag EpochStartTime = 'EpochStartTimeTag
    toMerkleNode = encodeMerkleInputNode encodeEpochStartTime
    fromMerkleNode = decodeMerkleInputNode decodeEpochStartTime
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

encodeEpochStartTime :: EpochStartTime -> Put
encodeEpochStartTime (EpochStartTime t) = encodeTime t

decodeEpochStartTime :: Get EpochStartTime
decodeEpochStartTime = EpochStartTime <$> decodeTime

-- -----------------------------------------------------------------------------
-- [Deprecated] Feature Flags
--
-- Deprecated: renamed into 'blockForkState'

-- newtype FeatureFlags = FeatureFlags Word64
--     deriving stock (Show, Eq, Generic)
--     deriving anyclass (NFData)
--     deriving newtype (ToJSON, FromJSON)
--
-- encodeFeatureFlags :: FeatureFlags -> Put
-- encodeFeatureFlags (FeatureFlags ff) = putWord64le ff
--
-- decodeFeatureFlags :: Get FeatureFlags
-- decodeFeatureFlags = FeatureFlags <$> getWord64le
--
-- instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag FeatureFlags where
--     type Tag FeatureFlags = 'ForkStateTag
--     toMerkleNode = encodeMerkleInputNode encodeFeatureFlags
--     fromMerkleNode = decodeMerkleInputNode decodeFeatureFlags
--
-- mkFeatureFlags :: FeatureFlags
-- mkFeatureFlags = FeatureFlags 0x0

-- -------------------------------------------------------------------------- --
-- Block Header

-- | BlockHeader
--
-- Values of this type should never be constructed directly by external code.
-- Instead the 'newBlockHeader' smart constructor should be used. Once
-- constructed 'BlockHeader' values must not be modified.
--
-- Some redundant, aggregated information is included in the block and the block
-- hash. This enables nodes to check blocks inductively with respect to existing
-- blocks without recalculating the aggregated value from the genesis block
-- onward.
--
-- The POW hash is not included, since it can be derived from the Nonce and the
-- other fields of the 'BlockHeader'.
--
-- /IMPORTANT/: Fields in this record must have pairwise distinct types.
--
data BlockHeader :: Type where
    BlockHeader ::
        { _blockFlags :: {-# UNPACK #-} !ForkState
            -- ^ Fork state of the block. This used to be called "feature
            -- flags". The old name is still used in textual encodings (e.g.
            -- JSON).

        , _blockCreationTime :: {-# UNPACK #-} !BlockCreationTime
            -- ^ The time when the block was creates as recorded by the miner
            -- of the block. The value must be strictly monotonically increasing
            -- within the chain of blocks. Nodes must ignore blocks with values
            -- that are in the future and reconsider a block when its value is
            -- in the past. Nodes do not have to store blocks until they become
            -- recent (but may do it).
            --
            -- The block creation time is used to determine the block difficulty for
            -- future blocks.
            --
            -- Nodes are not supposed to consider the creation time when
            -- choosing between two valid (this implies that creation time of a
            -- block is not the future) forks.
            --
            -- This creates an incentive for nodes to maintain an accurate clock
            -- with respect to an (unspecified) commonly accepted time source,
            -- such as the public NTP network.
            --
            -- It is possible that a miner always chooses the smallest possible
            -- creation time value. It is not clear what advantage a miner would
            -- gain from doing so, but attack models should consider and
            -- investigate such behavior.
            --
            -- On the other hand miners may choose to compute forks with creation
            -- time long in the future. By doing so, the difficulty on such a fork
            -- would decrease allowing the miner to compute very long chains very
            -- quickly. However, those chains would become valid only after a long
            -- time passed and would be of low PoW weight. The algorithm for
            -- computing the difficulty must ensure this strategy doesn't give
            -- an advantage to an attacker that would increase the success
            -- probability for an attack.

        , _blockParent :: {-# UNPACK #-} !BlockHash
            -- ^ authoritative

        , _blockAdjacentHashes :: !BlockHashRecord
            -- ^ authoritative

        , _blockTarget :: {-# UNPACK #-} !HashTarget
            -- ^ authoritative

        , _blockPayloadHash :: {-# UNPACK #-} !BlockPayloadHash
            -- ^ authoritative

        , _blockChainId :: {-# UNPACK #-} !ChainId

        , _blockWeight :: {-# UNPACK #-} !BlockWeight
            -- ^ the accumulated weight of the chain. It is redundant information
            -- that is subject to the inductive property that the block weight
            -- of a block is the block weight of the parent plus the difficulty
            -- of the block.

        , _blockHeight :: {-# UNPACK #-} !BlockHeight
            -- ^ block height records the length of the chain. It is redundant
            -- information and thus subject the inductive property that
            -- the block height of a block is the block height of its parent
            -- plus one.

        , _blockChainwebVersion :: !ChainwebVersionCode
            -- ^ the Chainweb version is a constant for the chain. A chain
            -- is uniquely identified by its genesis block. Thus this is
            -- redundant information and thus subject to the inductive property
            -- that the Chainweb version of a block equals the Chainweb version
            -- of its parent.

        , _blockEpochStart :: {-# UNPACK #-} !EpochStartTime
            -- ^ The start time of the current difficulty adjustment epoch.
            -- Epochs divide the sequence of blocks in the chain into continuous
            -- ranges of blocks. Each epoch is defined by the minimal block
            -- height of the blocks in the epoch.

        , _blockNonce :: {-# UNPACK #-} !Nonce
            -- ^ authoritative

        , _blockHash :: {-# UNPACK #-} !BlockHash
            -- ^ the hash of the block. It includes all of the above block properties.
        }
        -> BlockHeader
        deriving (Show, Generic)
        deriving anyclass (NFData)

instance Eq BlockHeader where
     (==) = (==) `on` _blockHash
     {-# INLINE (==) #-}

instance Ord BlockHeader where
     compare = compare `on` _blockHash

instance Hashable BlockHeader where
    hashWithSalt s = hashWithSalt s . _blockHash

instance HasChainId BlockHeader where
    _chainId = _blockChainId

instance HasChainGraph BlockHeader where
    _chainGraph h = _chainGraph (_chainwebVersion h, _blockHeight h)

instance HasChainwebVersion BlockHeader where
    _chainwebVersion = lookupVersionByCode . _blockChainwebVersion

instance IsCasValue BlockHeader where
    type CasKeyType BlockHeader = BlockHash
    casKey = _blockHash
    {-# INLINE casKey #-}

type BlockHeaderCas tbl = Cas tbl BlockHeader

-- | Used for quickly identifying "which block" this is.
-- Example output:
-- "0 @ bSQgL5 (height 4810062)"
--
blockHeaderShortDescription :: BlockHeader -> T.Text
blockHeaderShortDescription bh =
    T.unwords
        [ toText (_chainId bh)
        , "@"
        , blockHashToTextShort (_blockHash bh)
        , "(height " <> sshow (getBlockHeight $ _blockHeight bh) <> ")"
        ]

makeLenses ''BlockHeader

-- | During the first epoch after genesis there are 10 extra difficulty
-- adjustments. This is to account for rapidly changing total hash power in the
-- early stages of the network.
--
effectiveWindow :: BlockHeader -> Maybe WindowWidth
effectiveWindow h = WindowWidth <$> case _versionWindow (_chainwebVersion h) of
    WindowWidth w
        | int (_blockHeight h) <= w -> Just $ max 1 $ w `div` 10
        | otherwise -> Just w

-- | Return whether the given 'BlockHeader' is the last header in its epoch.
--
isLastInEpoch :: BlockHeader -> Bool
isLastInEpoch h = case effectiveWindow h of
    Nothing -> False
    Just (WindowWidth w) -> (int (_blockHeight h) + 1) `mod` w == 0

-- | If it is discovered that the last DA occured significantly in the past, we
-- assume that a large amount of hash power has suddenly dropped out of the
-- network. Thus we must perform Emergency Difficulty Adjustment to avoid
-- stalling the chain.
--
-- NOTE: emergency DAs are now regarded a misfeature and have been disabled in
-- all chainweb version. Emergency DAs are enabled (and have occured) only on
-- mainnet01 for cut heights smaller than 80,000.
--
slowEpoch :: ParentHeader -> BlockCreationTime -> Bool
slowEpoch (ParentHeader p) (BlockCreationTime ct) = actual > (expected * 5)
  where
    EpochStartTime es = _blockEpochStart p
    v = _chainwebVersion p
    BlockDelay bd = _versionBlockDelay v
    WindowWidth ww = _versionWindow v

    expected :: Micros
    expected = bd * int ww

    actual :: Micros
    actual = timeSpanToMicros $ ct .-. es

-- | Compute the POW target for a new BlockHeader.
--
-- Alternatively, the new chains can use a higher target and the target of the
-- old chains arent' adjusted. That includes the risk of larger orphan rates. In
-- particular after the first and second DA, the current DA will compute targets
-- that are averages between chains, which cause the difficulty to go donwn
-- globally. This is usually mostly mitigated after the third DA after the
-- transition.
--
powTarget
    :: ParentHeader
        -- ^ parent header
    -> HM.HashMap ChainId ParentHeader
        -- ^ adjacent Parents
    -> BlockCreationTime
        -- ^ block creation time of new block
        --
        -- This parameter is used only when @oldTargetGuard@ is @True@.
        --
    -> HashTarget
        -- ^ POW target of new block
powTarget p@(ParentHeader ph) as bct = case effectiveWindow ph of
    Nothing -> maxTarget
    Just w
        -- Emergency DA, legacy
        | slowEpochGuard ver (_chainId ph) (_blockHeight ph) && slowEpoch p bct ->
            activeAdjust w
        | isLastInEpoch ph -> activeAdjust w
        | otherwise -> _blockTarget ph
  where
    ver = _chainwebVersion ph
    t = EpochStartTime $ if oldTargetGuard ver (_chainId ph) (_blockHeight ph)
        then _bct bct
        else _bct (_blockCreationTime ph)

    activeAdjust w
        | oldDaGuard ver (_chainId ph) (_blockHeight ph + 1)
            = legacyAdjust (_versionBlockDelay ver) w (t .-. _blockEpochStart ph) (_blockTarget ph)
        | otherwise
            = avgTarget $ adjustForParent w <$> (p : HM.elems as)

    adjustForParent w (ParentHeader a)
        = adjust (_versionBlockDelay ver) w (toEpochStart a .-. _blockEpochStart a) (_blockTarget a)

    toEpochStart = EpochStartTime . _bct . _blockCreationTime

    avgTarget targets = HashTarget $ floor $ s / int (length targets)
      where
        s = sum $ fmap (int @_ @Rational . _hashTarget) targets
{-# INLINE powTarget #-}

-- | Compute the epoch start value for a new BlockHeader
--
epochStart
    :: ParentHeader
        -- ^ parent header
    -> HM.HashMap ChainId ParentHeader
        -- ^ Adjacent parents of the block. It is not checked whether the
        -- set of adjacent parents conforms with the current graph.
    -> BlockCreationTime
        -- ^ block creation time of new block
        --
        -- This parameter is used only when @oldTargetGuard@ is @True@.
        --
    -> EpochStartTime
        -- ^ epoch start time of new block
epochStart ph@(ParentHeader p) adj (BlockCreationTime bt)
    | Nothing <- effectiveWindow p = _blockEpochStart p

    -- A special case for starting a new devnet, to compensate the inaccurate
    -- creation time of the genesis blocks. This would result in a very long
    -- first epoch that cause a trivial target in the second epoch.
    | ver ^. versionCheats . fakeFirstEpochStart, _blockHeight p == 1 = EpochStartTime (_bct $ _blockCreationTime p)

    -- New Graph: the block time of the genesis block isn't accurate, we thus
    -- use the block time of the first block on the chain. Depending on where
    -- this is within an epoch, this can cause a shorter epoch, which could
    -- cause a larger difficulty and a reduced target. That is fine, since new
    -- chains are expected to start with a low difficulty.
    | parentIsFirstOnNewChain = EpochStartTime (_bct $ _blockCreationTime p)

    -- End of epoch, DA adjustment (legacy version)
    | isLastInEpoch p && oldTargetGuard ver (_chainId p) (_blockHeight p) = EpochStartTime bt

    -- End of epoch, DA adjustment
    | isLastInEpoch p = EpochStartTime (_bct $ _blockCreationTime p)

    -- Within epoch with old legacy DA
    | oldDaGuard ver (_chainId p) (_blockHeight p + 1) = _blockEpochStart p

    -- Within an epoch with new DA
    | otherwise = _blockEpochStart p

    -- Experimental, allow DA to support multiple hash functions
    --  | otherwise = _blockEpochStart p .+^ _adjustmentAvg
  where
    ver = _chainwebVersion p
    cid = _chainId p

    -- Add a penalty for fast chains by adding the different between the
    -- creation time of the current chain and the maximum of the adjacent chains
    -- to the epoch start time. By shortening the epoch DA is going to adjust to
    -- a higher difficulty.
    --
    -- This DA has the disadvantage, that it adjusts to a block rate that is
    -- smaller than the targeted blockrate, because with high probablity all
    -- chains are receiving some positive penalty.
    --
    -- Properties of DA:
    --
    -- * Requires that miners set creation time >0.5 of solve time.
    -- * Requires correction factor for targeted block rate.
    -- * Can handle non continuous non uniform distribution of hash power
    --   accross chains.
    --
    _adjustmentMax = maximum adjCreationTimes .-. _blockCreationTime p
        -- the maximum is at least @_blockCreationTime p@ and thus the result is
        -- greater or equal 0.

    -- This computes @mean adjCreationTimes - _blockCreationTime p
    --
    -- It holds that
    --
    -- \(\left(mean_{0 \leq i < n} a_i\right)
    -- = \frac{\sum_{0 \leq i < n} a_i}{n} - t
    -- = \frac{\left(sum_{0 \leq i < n} a_i\right) - \left(\sum_{0 \leq i < n} t\right)}{n}
    -- = \frac{sum_{0 \leq i < n} (a_i - t)}{n}
    -- \)
    --
    -- this is numerically sound because we compute the differences on integral
    -- types without rounding.
    --
    -- Properties of DA:
    --
    -- * Requires that miners set creation time >0.5 of solve time
    -- * Can handle non continuous non uniform distribution of hash power
    --   accross chains.
    --
    _adjustmentAvg = x `divTimeSpan` length adjCreationTimes
      where
        x :: TimeSpan Micros
        x = foldr1 addTimeSpan $ (.-. _blockCreationTime p) <$> adjCreationTimes

    -- This includes the parent header itself, but excludes any adjacent genesis
    -- headers which usually don't have accurate creation time.
    --
    -- The result is guaranteed to be non-empty
    --
    adjCreationTimes = fmap _blockCreationTime
        $ HM.insert cid (_parentHeader ph)
        $ HM.filter (not . isGenesisBlockHeader)
        $ fmap _parentHeader adj

    parentIsFirstOnNewChain
        = _blockHeight p > 1 && _blockHeight p == genesisHeight ver cid + 1
{-# INLINE epochStart #-}

-- -------------------------------------------------------------------------- --
-- Newtype wrappers for function parameters

newtype ParentCreationTime = ParentCreationTime
    { _parentCreationTime :: BlockCreationTime }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON, Hashable, LeftTorsor)

newtype ParentHeader = ParentHeader
    { _parentHeader :: BlockHeader }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

parentHeader :: Lens' ParentHeader BlockHeader
parentHeader = lens _parentHeader $ \_ hdr -> ParentHeader hdr

parentHeaderHash :: Getter ParentHeader BlockHash
parentHeaderHash = parentHeader . blockHash

_rankedParentHash :: ParentHeader -> RankedBlockHash
_rankedParentHash = _rankedBlockHash . _parentHeader

rankedParentHash :: Getter ParentHeader RankedBlockHash
rankedParentHash = parentHeader . rankedBlockHash

instance HasChainId ParentHeader where
    _chainId = _chainId . _parentHeader
    {-# INLINE _chainId #-}

instance HasChainwebVersion ParentHeader where
    _chainwebVersion = _chainwebVersion . _parentHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ParentHeader where
    _chainGraph = _chainGraph . _parentHeader
    {-# INLINE _chainGraph #-}

isGenesisBlockHeader :: BlockHeader -> Bool
isGenesisBlockHeader b =
    _blockHeight b == genesisHeight (_chainwebVersion b) (_chainId b)

-- | The genesis block hash includes the Chainweb version and the 'ChainId'
-- within the Chainweb version.
--
-- It is the '_blockParent' of the genesis block
--
genesisParentBlockHash :: HasChainId p => ChainwebVersion -> p -> BlockHash
genesisParentBlockHash v p = BlockHash $ MerkleLogHash
    $ merkleRoot $ merkleTree @ChainwebMerkleHashAlgorithm
        [ InputNode "CHAINWEB_GENESIS"
        , encodeMerkleInputNode encodeChainwebVersionCode (_versionCode v)
        , encodeMerkleInputNode encodeChainId (_chainId p)
        ]

{-# NOINLINE genesisBlockHeaderCache #-}
genesisBlockHeaderCache :: IORef (HashMap ChainwebVersionCode (HashMap ChainId BlockHeader))
genesisBlockHeaderCache = unsafePerformIO $ do
    newIORef HM.empty

-- | A block chain is globally uniquely identified by its genesis hash.
-- Internally, we use the 'ChainwebVersionTag value and the 'ChainId'
-- as identifiers. We thus include the 'ChainwebVersionTag value and the
-- 'ChainId' into the genesis block hash.
--
-- We assume that there is always only a single 'ChainwebVersionTag in
-- scope and identify chains only by their internal 'ChainId'.
--
genesisBlockHeaders :: ChainwebVersion -> HashMap ChainId BlockHeader
genesisBlockHeaders = \v ->
    if _versionCode v == _versionCode mainnet then mainnetGenesisHeaders
    else if _versionCode v == _versionCode testnet04 then testnetGenesisHeaders
    else unsafeDupablePerformIO $
        HM.lookup (_versionCode v) <$> readIORef genesisBlockHeaderCache >>= \case
            Just hs -> return hs
            Nothing -> do
                let freshGenesisHeaders = makeGenesisBlockHeaders v
                modifyIORef' genesisBlockHeaderCache $ HM.insert (_versionCode v) freshGenesisHeaders
                return freshGenesisHeaders
  where
    mainnetGenesisHeaders = makeGenesisBlockHeaders mainnet
    testnetGenesisHeaders = makeGenesisBlockHeaders testnet04

genesisBlockHeader :: (HasCallStack, HasChainId p) => ChainwebVersion -> p -> BlockHeader
genesisBlockHeader v p = genesisBlockHeaders v ^?! ix (_chainId p)

makeGenesisBlockHeaders :: ChainwebVersion -> HashMap ChainId BlockHeader
makeGenesisBlockHeaders v = HM.fromList [ (cid, makeGenesisBlockHeader v cid) | cid <- HS.toList (chainIds v)]

makeGenesisBlockHeader :: ChainwebVersion -> ChainId -> BlockHeader
makeGenesisBlockHeader v cid =
    makeGenesisBlockHeader' v cid (_genesisTime (_versionGenesis v) ^?! atChain cid) (Nonce 0)

-- | Like `genesisBlockHeader`, but with slightly more control.
--
-- This call generates the block header from the definitions in
-- "Chainweb.Version". It is a somewhat expensive call, since it involves
-- building the Merkle tree.
--
makeGenesisBlockHeader'
    :: HasChainId p
    => ChainwebVersion
    -> p
    -> BlockCreationTime
    -> Nonce
    -> BlockHeader
makeGenesisBlockHeader' v p ct@(BlockCreationTime t) n =
    fromLog @ChainwebMerkleHashAlgorithm mlog
  where
    (h, g) = genesisHeightAndGraph v p
    cid = _chainId p

    mlog = newMerkleLog
        $ genesisForkState
        :+: ct
        :+: genesisParentBlockHash v cid
        :+: (v ^?! versionGenesis . genesisBlockTarget . atChain cid)
        :+: genesisBlockPayloadHash v cid
        :+: cid
        :+: BlockWeight 0
        :+: h -- because of chain graph changes (new chains) not all chains start at 0
        :+: _versionCode v
        :+: EpochStartTime t
        :+: n
        :+: MerkleLogBody (blockHashRecordToVector adjParents)
    adjParents = BlockHashRecord $ HM.fromList $
        (\c -> (c, genesisParentBlockHash v c)) <$> HS.toList (adjacentChainIds g p)

-- | The set of genesis block headers as it exited at a particular block height
--
genesisBlockHeadersAtHeight
    :: ChainwebVersion
    -> BlockHeight
    -> HashMap ChainId BlockHeader
genesisBlockHeadersAtHeight v h =
    HM.filter (\hdr -> _blockHeight hdr <= h) (genesisBlockHeaders v)
--
-- -------------------------------------------------------------------------- --
-- Genesis Height

-- | Returns the height of the genesis block for a chain.
--
-- Invariant:
--
-- * The given ChainId exists in the first graph of the graph history.
--   (We generally assume that this invariant holds throughout the code base.
--   It is enforced via the 'mkChainId' smart constructor for ChainId.)
--
genesisHeight :: HasCallStack => ChainwebVersion -> ChainId -> BlockHeight
genesisHeight v c = _blockHeight (genesisBlockHeader v c)

instance HasMerkleLog ChainwebMerkleHashAlgorithm ChainwebHashTag BlockHeader where

    -- /IMPORTANT/ a types must occur at most once in this list
    type MerkleLogHeader BlockHeader =
        '[ ForkState
        , BlockCreationTime
        , BlockHash
        , HashTarget
        , BlockPayloadHash
        , ChainId
        , BlockWeight
        , BlockHeight
        , ChainwebVersionCode
        , EpochStartTime
        , Nonce
        ]
    type MerkleLogBody BlockHeader = BlockHash

    toLog bh = merkleLog @ChainwebMerkleHashAlgorithm root entries
      where
        BlockHash (MerkleLogHash root) = _blockHash bh
        entries
            = _blockFlags bh
            :+: _blockCreationTime bh
            :+: _blockParent bh
            :+: _blockTarget bh
            :+: _blockPayloadHash bh
            :+: _blockChainId bh
            :+: _blockWeight bh
            :+: _blockHeight bh
            :+: _blockChainwebVersion bh
            :+: _blockEpochStart bh
            :+: _blockNonce bh
            :+: MerkleLogBody (blockHashRecordToVector $ _blockAdjacentHashes bh)

    fromLog l = BlockHeader
            { _blockFlags = flags
            , _blockCreationTime = time
            , _blockHash = BlockHash (MerkleLogHash $ _merkleLogRoot l)
            , _blockParent = parentHash
            , _blockTarget = target
            , _blockPayloadHash = payload
            , _blockChainId = cid
            , _blockWeight = weight
            , _blockHeight = height
            , _blockChainwebVersion = cwvc
            , _blockEpochStart = es
            , _blockNonce = nonce
            , _blockAdjacentHashes = blockHashRecordFromVector adjGraph cid adjParents
            }
      where
        ( flags
            :+: time
            :+: parentHash
            :+: target
            :+: payload
            :+: cid
            :+: weight
            :+: height
            :+: cwvc
            :+: es
            :+: nonce
            :+: MerkleLogBody adjParents
            ) = _merkleLogEntries l
        cwv = lookupVersionByCode cwvc

        adjGraph
            | height == genesisBlockHeight cwv cid = chainGraphAt cwv height
            | otherwise = chainGraphAt cwv (height - 1)

encodeBlockHeaderWithoutHash :: BlockHeader -> Put
encodeBlockHeaderWithoutHash b = do
    encodeForkState (_blockFlags b)
    encodeBlockCreationTime (_blockCreationTime b)
    encodeBlockHash (_blockParent b)
    encodeBlockHashRecord (_blockAdjacentHashes b)
    encodeHashTarget (_blockTarget b)
    encodeBlockPayloadHash (_blockPayloadHash b)
    encodeChainId (_blockChainId b)
    encodeBlockWeight (_blockWeight b)
    encodeBlockHeight (_blockHeight b)
    encodeChainwebVersionCode (_blockChainwebVersion b)
    encodeEpochStartTime (_blockEpochStart b)
    encodeNonce (_blockNonce b)

encodeBlockHeader :: BlockHeader -> Put
encodeBlockHeader b = do
    encodeBlockHeaderWithoutHash b
    encodeBlockHash (_blockHash b)

-- | Decode and check that
--
-- 1. chain id is in graph
-- 2. all adjacentParent match adjacents in graph
--
decodeBlockHeaderChecked :: Get BlockHeader
decodeBlockHeaderChecked = do
    !bh <- decodeBlockHeader
    _ <- checkAdjacentChainIds bh bh (Expected $ _blockAdjacentChainIds bh)
    return bh

-- | Decode and check that
--
-- 1. chain id is in graph
-- 2. all adjacentParent match adjacents in graph
-- 3. chainId matches the expected chain id
--
decodeBlockHeaderCheckedChainId
    :: HasChainId p
    => Expected p
    -> Get BlockHeader
decodeBlockHeaderCheckedChainId p = do
    !bh <- decodeBlockHeaderChecked
    _ <- checkChainId p (Actual (_chainId bh))
    return bh

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeaderWithoutHash :: Get BlockHeader
decodeBlockHeaderWithoutHash = do
    a0 <- decodeForkState
    a1 <- decodeBlockCreationTime
    a2 <- decodeBlockHash -- parent hash
    a3 <- decodeBlockHashRecord
    a4 <- decodeHashTarget
    a5 <- decodeBlockPayloadHash
    a6 <- decodeChainId
    a7 <- decodeBlockWeight
    a8 <- decodeBlockHeight
    a9 <- decodeChainwebVersionCode
    a11 <- decodeEpochStartTime
    a12 <- decodeNonce
    return
        $! fromLog @ChainwebMerkleHashAlgorithm
        $ newMerkleLog
        $ a0
        :+: a1
        :+: a2
        :+: a4
        :+: a5
        :+: a6
        :+: a7
        :+: a8
        :+: a9
        :+: a11
        :+: a12
        :+: MerkleLogBody (blockHashRecordToVector a3)

-- | Decode a BlockHeader and trust the result
--
decodeBlockHeader :: Get BlockHeader
decodeBlockHeader = BlockHeader
    <$> decodeForkState
    <*> decodeBlockCreationTime
    <*> decodeBlockHash -- parent hash
    <*> decodeBlockHashRecord
    <*> decodeHashTarget
    <*> decodeBlockPayloadHash
    <*> decodeChainId
    <*> decodeBlockWeight
    <*> decodeBlockHeight
    <*> decodeChainwebVersionCode
    <*> decodeEpochStartTime
    <*> decodeNonce
    <*> decodeBlockHash

instance ToJSON BlockHeader where
    toJSON = toJSON . encodeB64UrlNoPaddingText . runPutS . encodeBlockHeader
    toEncoding = b64UrlNoPaddingTextEncoding . runPutS . encodeBlockHeader
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON BlockHeader where
    parseJSON = withText "BlockHeader" $ \t ->
        case runGetS decodeBlockHeader =<< decodeB64UrlNoPaddingText t of
            Left (e :: SomeException) -> fail (sshow e)
            (Right !x) -> return x

_blockAdjacentChainIds :: BlockHeader -> HS.HashSet ChainId
_blockAdjacentChainIds =
    HS.fromList . HM.keys . _getBlockHashRecord . _blockAdjacentHashes

blockAdjacentChainIds :: Getter BlockHeader (HS.HashSet ChainId)
blockAdjacentChainIds = to _blockAdjacentChainIds

-- | @getAdjacentHash cid h@ returns the adjacent hash of h for chain cid. It
-- throws a @ChainNotAdjacentException@ if @cid@ is not adajcent with @_chainId
-- h@ in the chain graph of @h@.
--
getAdjacentHash :: MonadThrow m => HasChainId p => p -> BlockHeader -> m BlockHash
getAdjacentHash p b = firstOf (blockAdjacentHashes . ixg (_chainId p)) b
    ??? ChainNotAdjacentException
        (_chainId b)
        (Expected $ _chainId p)
        (Actual $ _blockAdjacentChainIds b)
{-# INLINE getAdjacentHash #-}

computeBlockHash :: BlockHeader -> BlockHash
computeBlockHash h = BlockHash $ MerkleLogHash $ computeMerkleLogRoot h
{-# INLINE computeBlockHash #-}

-- | The Proof-Of-Work hash includes all data in the block except for the
-- '_blockHash'. The value (interpreted as 'BlockHashNat' must be smaller than
-- the value of '_blockTarget' (interpreted as 'BlockHashNat').
--
_blockPow :: BlockHeader -> PowHash
_blockPow h = cryptoHash @Blake2s_256
    $ runPutS $ encodeBlockHeaderWithoutHash h

blockPow :: Getter BlockHeader PowHash
blockPow = to _blockPow
{-# INLINE blockPow #-}

-- | The number of microseconds between the creation time of two `BlockHeader`s.
--
timeBetween :: BlockCreationTime -> BlockCreationTime -> Micros
timeBetween after before = f after - f before
  where
    f :: BlockCreationTime -> Micros
    f (BlockCreationTime (Time (TimeSpan ts))) = ts

-- | Absolute BlockHeight Difference
--
absBlockHeightDiff :: BlockHeader -> BlockHeader -> BlockHeight
absBlockHeightDiff a b
    | _blockHeight a >= _blockHeight b = _blockHeight a - _blockHeight b
    | otherwise = _blockHeight b - _blockHeight a

-- -------------------------------------------------------------------------- --
-- Object JSON encoding

-- | By default a binary encoding of block headers is used as JSON encoding. In
-- some circumstance, like logging and configuration files, a textual encoding
-- is desired.
--
newtype ObjectEncoded a = ObjectEncoded { _objectEncoded :: a }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, NFData)

newtype ExtendedObjectEncoded a = ExtendedObjectEncoded { _extendedObjectEncoded :: a }
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, NFData)

blockHeaderProperties
    :: KeyValue e kv
    => ObjectEncoded BlockHeader
    -> [kv]
blockHeaderProperties (ObjectEncoded b) =
    [ "nonce" .= _blockNonce b
    , "creationTime" .= _blockCreationTime b
    , "parent" .= _blockParent b
    , "adjacents" .= _blockAdjacentHashes b
    , "target" .= _blockTarget b
    , "payloadHash" .= _blockPayloadHash b
    , "chainId" .= _chainId b
    , "weight" .= _blockWeight b
    , "height" .= _blockHeight b
    , "chainwebVersion" .= _versionName (_chainwebVersion b)
    , "epochStart" .= _blockEpochStart b
    , "featureFlags" .= _blockFlags b
    , "hash" .= _blockHash b
    ]
{-# INLINE blockHeaderProperties #-}

extendedBlockHeaderProperties
    :: KeyValue e kv
    => ExtendedObjectEncoded BlockHeader
    -> [kv]
extendedBlockHeaderProperties (ExtendedObjectEncoded b) =
    blockHeaderProperties (ObjectEncoded b) <>
    [ "powHash" .= _blockPow b
    , "forkNumber" .= _blockForkNumber b
    , "forkVotes" .= _blockForkVotes b
    ]
{-# INLINE extendedBlockHeaderProperties #-}

instance ToJSON (ObjectEncoded BlockHeader) where
    toJSON = object . blockHeaderProperties
    toEncoding = pairs . mconcat . blockHeaderProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

parseBlockHeaderObject :: Object -> Parser BlockHeader
parseBlockHeaderObject o = BlockHeader
    <$> o .: "featureFlags"
    <*> o .: "creationTime"
    <*> o .: "parent"
    <*> o .: "adjacents"
    <*> o .: "target"
    <*> o .: "payloadHash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> o .: "height"
    -- TODO: lookupVersionByName should probably be deprecated for performance,
    -- so perhaps we move this codec outside of the node proper.
    <*> (_versionCode . lookupVersionByName <$> (o .: "chainwebVersion"))
    <*> o .: "epochStart"
    <*> o .: "nonce"
    <*> o .: "hash"

instance FromJSON (ObjectEncoded BlockHeader) where
    parseJSON = withObject "BlockHeader"
        $ fmap ObjectEncoded . parseBlockHeaderObject
    {-# INLINE parseJSON #-}

instance ToJSON (ExtendedObjectEncoded BlockHeader) where
    toJSON = object . extendedBlockHeaderProperties
    toEncoding = pairs . mconcat . extendedBlockHeaderProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- -------------------------------------------------------------------------- --
-- Fork State

blockForkState :: Lens' BlockHeader ForkState
blockForkState = blockFlags . coerced

_blockForkNumber :: BlockHeader -> ForkNumber
_blockForkNumber = view (blockForkState . forkNumber)

_blockForkVotes :: BlockHeader -> ForkVotes
_blockForkVotes = view (blockForkState . forkVotes)

blockForkNumber :: Lens' BlockHeader ForkNumber
blockForkNumber = blockForkState . forkNumber

blockForkVotes :: Lens' BlockHeader ForkVotes
blockForkVotes = blockForkState . forkVotes

-- | Returns whether a block is the first block in a fork epoch.
--
isForkEpochStart :: BlockHeader -> Bool
isForkEpochStart hdr =
    rem (int $ view blockHeight hdr) forkEpochLength == 0

isLastForkEpochBlock :: BlockHeader -> Bool
isLastForkEpochBlock hdr =
    rem (1 + int (view blockHeight hdr)) forkEpochLength == 0

-- | Returns whether a block is at a height at which voting occurs.
--
isForkVoteBlock :: BlockHeader -> Bool
isForkVoteBlock hdr =
    rem (int $ view blockHeight hdr) forkEpochLength < (forkEpochLength - voteCountLength)

-- | Returns whether a block is at a height at which vote couting occurs.
--
isForkCountBlock :: BlockHeader -> Bool
isForkCountBlock hdr = not (isForkVoteBlock hdr)

-- | New Fork State computation
--
-- * isForkEpochStart -> forkNumber is deterministically increased
-- * isForkEpochStart -> forkVote is nondeterministically reset to 0 or forkStep
-- * forkVoteBlock && not isForkVoteStart -> forkVotes are non-deterministically monotonicly increasing
-- * forkCountBlock -> forkVotes are deterministically set
--
newForkState
    :: HM.HashMap ChainId ParentHeader
        -- ^ Adjacent parent headers
    -> ParentHeader
        -- Parent block header
    -> ForkNumber
        -- ^ Target fork number. Vote "yes" to increase fork number, if the fork
        -- number of the parent header is less than this value.
    -> ForkState
newForkState as p targetFork
    | isLastForkEpochBlock (view parentHeader p) = cur
        -- reset votes and vote
        & forkVotes .~ (if vote then addVote resetVotes else resetVotes)
        -- based on current vote count decide whether to increase fork number
        & forkNumber %~ (if decideVotes curVotes then succ else id)
    | isForkVoteBlock (view parentHeader p) = cur
        -- vote
        & forkVotes %~ (if vote then addVote else id)
    | otherwise = cur
        -- do one vote counting step
        & forkVotes .~ countVotes allParentVotes
  where
    vote = curNumber < targetFork
    cur = view (parentHeader . blockForkState) p
    curNumber = view (parentHeader . blockForkNumber) p
    curVotes = view (parentHeader . blockForkVotes) p
    allParentVotes = view (parentHeader . blockForkVotes) <$> (p : HM.elems as)

genesisForkState :: ForkState
genesisForkState = ForkState 0

-- -------------------------------------------------------------------------- --
-- IsBlockHeader

-- | Any type which can purely produce a `BlockHeader`, or purely construct one.
--
class IsBlockHeader t where
    isoBH :: Iso' t BlockHeader

instance IsBlockHeader BlockHeader where
    isoBH = id

-- -------------------------------------------------------------------------- --
-- Create new BlockHeader

-- | Creates a new block header. No validation of the input parameters is
-- performaned.
--
-- It's not guaranteed that the result is a valid block header. It is, however,
-- guaranteed by construction that
--
-- * the target,
-- * the weight,
-- * the block height,
-- * the version,
-- * the chain id,
-- * the epoch start time, and
-- * the fork state
--
-- are valid with respect to the given parent header and adjacent parent
-- headers.
--
-- TODO: also check adjacent chains. This would probably break a lot of tests,
-- but might be worth it!
--
newBlockHeader
    :: HM.HashMap ChainId ParentHeader
        -- ^ Adjacent parent hashes. The hash and the PoW target of these are
        -- needed for construction the new header.
    -> BlockPayloadHash
        -- ^ payload hash
    -> Nonce
        -- ^ Randomness to affect the block hash. It is not verified that the
        -- nonce is valid with respect to the target.
    -> BlockCreationTime
        -- ^ Creation time of the block.
    -> ParentHeader
        -- ^ parent block header
    -> BlockHeader
newBlockHeader adj pay nonce t p@(ParentHeader b) =
    fromLog @ChainwebMerkleHashAlgorithm $ newMerkleLog
        $ newForkState adj p (_versionForkNumber v)
        :+: t
        :+: _blockHash b
        :+: target
        :+: pay
        :+: cid
        :+: _blockWeight b + BlockWeight (targetToDifficulty target)
        :+: _blockHeight b + 1
        :+: _versionCode v
        :+: epochStart p adj t
        :+: nonce
        :+: MerkleLogBody (blockHashRecordToVector adjHashes)
  where
    cid = _chainId p
    v = _chainwebVersion p
    target = powTarget p adj t
    adjHashes = BlockHashRecord $ _blockHash . _parentHeader <$> adj

-- -------------------------------------------------------------------------- --
-- TreeDBEntry instance

instance TreeDbEntry BlockHeader where
    type Key BlockHeader = BlockHash
    key = _blockHash
    rank = int . _blockHeight
    parent e
        | isGenesisBlockHeader e = Nothing
        | otherwise = Just (_blockParent e)

-- -------------------------------------------------------------------------- --
-- Misc

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
headerSizes :: ChainwebVersion -> Rule BlockHeight Natural
headerSizes v = (\g -> _versionHeaderBaseSizeBytes v + 36 * degree g + 2)
    <$> _versionGraphs v

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
    $ ruleHead
    $ ruleDropWhile (> relevantHeight)
    $ headerSizes v
  where
    relevantHeight
        | genesisHeight v cid == h = h
        | otherwise = h - 1

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

_rankedBlockHash :: BlockHeader -> RankedBlockHash
_rankedBlockHash h = RankedBlockHash
    { _rankedBlockHashHeight = _blockHeight h
    , _rankedBlockHashHash = _blockHash h
    }
{-# INLINE _rankedBlockHash #-}

rankedBlockHash :: Getter BlockHeader RankedBlockHash
rankedBlockHash = to _rankedBlockHash
{-# INLINE rankedBlockHash #-}

_rankedBlockPayloadHash :: BlockHeader -> RankedBlockPayloadHash
_rankedBlockPayloadHash h = RankedBlockPayloadHash
    { _rankedBlockPayloadHashHeight = _blockHeight h
    , _rankedBlockPayloadHashHash = _blockPayloadHash h
    }
{-# INLINE _rankedBlockPayloadHash #-}

rankedBlockPayloadHash :: Getter BlockHeader RankedBlockPayloadHash
rankedBlockPayloadHash = to _rankedBlockPayloadHash
{-# INLINE rankedBlockPayloadHash #-}

