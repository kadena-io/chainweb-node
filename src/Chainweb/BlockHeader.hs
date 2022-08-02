{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Chainweb.BlockHeader
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.BlockHeader
(
-- * Newtype wrappers for function parameters
  ParentHeader(..)
, parentHeader
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

-- * FeatureFlags
, FeatureFlags
, mkFeatureFlags
, encodeFeatureFlags
, decodeFeatureFlags

-- * POW Target
, powTarget

-- * BlockHeader
, BlockHeader(..)
, blockNonce
, blockChainId
, blockHeight
, blockWeight
, blockChainwebVersion
, blockAdjacentHashes
, blockCreationTime
, blockHash
, blockParent
, blockPayloadHash
, blockTarget
, blockEpochStart
, blockFlags
, _blockPow
, blockPow
, _blockAdjacentChainIds
, blockAdjacentChainIds
, encodeBlockHeader
, encodeBlockHeaderWithoutHash
, decodeBlockHeader
, decodeBlockHeaderWithoutHash
, decodeBlockHeaderChecked
, decodeBlockHeaderCheckedChainId
, ObjectEncoded(..)

, timeBetween
, getAdjacentHash
, computeBlockHash
, adjacentChainIds
, absBlockHeightDiff

-- * IsBlockHeader
, IsBlockHeader(..)

-- * Genesis BlockHeader
, isGenesisBlockHeader

-- * Create a new BlockHeader
, newBlockHeader

-- * CAS Constraint
, BlockHeaderCas
) where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Kind
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import Data.Word

import GHC.Generics (Generic)

-- Internal imports

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeight
import Chainweb.BlockWeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse
import Chainweb.Payload
import Chainweb.PowHash
import Chainweb.Time
import Chainweb.TreeDB (TreeDbEntry(..))
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version

import Data.CAS

import Numeric.AffineSpace

import Text.Read (readEither)

-- -------------------------------------------------------------------------- --
-- Nonce

-- | FIXME: is 64 bit enough for the nonce. It seems that it may not be
-- sufficient for the current hashpower of the bitcoin network.
--
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

-- | During the first epoch after genesis there are 10 extra difficulty
-- adjustments. This is to account for rapidly changing total hash power in the
-- early stages of the network.
--
effectiveWindow :: BlockHeader -> Maybe WindowWidth
effectiveWindow h = WindowWidth <$> case window ver of
    Nothing -> Nothing
    Just (WindowWidth w)
        | int (_blockHeight h) <= w -> Just $ max 1 $ w `div` 10
        | otherwise -> Just w
  where
    ver = _blockChainwebVersion h
{-# INLINE effectiveWindow #-}

-- | Return whether the given 'BlockHeader' is the last header in its epoch.
--
isLastInEpoch :: BlockHeader -> Bool
isLastInEpoch h = case effectiveWindow h of
    Nothing -> False
    Just (WindowWidth w)
        | (int (_blockHeight h) + 1) `mod` w == 0 -> True
        | otherwise -> False
{-# INLINE isLastInEpoch #-}

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
    BlockRate s = blockRate (_blockChainwebVersion p)
    WindowWidth ww = fromJuste $ window (_blockChainwebVersion p)

    expected :: Seconds
    expected = s * int ww

    actual :: Seconds
    actual = timeSpanToSeconds $ ct .-. es

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
        | slowEpochGuard ver (_blockHeight ph) && slowEpoch p bct ->
            activeAdjust w
        | isLastInEpoch ph -> activeAdjust w
        | otherwise -> _blockTarget ph
  where
    t = EpochStartTime $ if oldTargetGuard ver (_blockHeight ph)
        then _bct bct
        else _bct (_blockCreationTime ph)
    ver = _chainwebVersion p

    activeAdjust w
        | oldDaGuard ver (_blockHeight ph + 1)
            = legacyAdjust ver w (t .-. _blockEpochStart ph) (_blockTarget ph)
        | otherwise
            = avgTarget $ adjustForParent w <$> (p : HM.elems as)

    adjustForParent w (ParentHeader a)
        = adjust ver w (toEpochStart a .-. _blockEpochStart a) (_blockTarget a)

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
    | ver == Development && _blockHeight p == 1 = EpochStartTime (_bct $ _blockCreationTime p)

    -- New Graph: the block time of the genesis block isn't accurate, we thus
    -- use the block time of the first block on the chain. Depending on where
    -- this is within an epoch, this can cause a shorter epoch, which could
    -- cause a larger difficulty and a reduced target. That is fine, since new
    -- chains are expected to start with a low difficulty.
    | parentIsFirstOnNewChain = EpochStartTime (_bct $ _blockCreationTime p)

    -- End of epoch, DA adjustment (legacy version)
    | isLastInEpoch p && oldTargetGuard ver (_blockHeight p) = EpochStartTime bt

    -- End of epoch, DA adjustment
    | isLastInEpoch p = EpochStartTime (_bct $ _blockCreationTime p)

    -- Within epoch with old legacy DA
    | oldDaGuard ver (_blockHeight p + 1) = _blockEpochStart p

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
    -- this is numberically sound because we compute the differences on integral
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
    adjCreationTimes = fmap (_blockCreationTime)
        $ HM.insert cid (_parentHeader ph)
        $ HM.filter (not . isGenesisBlockHeader)
        $ fmap _parentHeader adj

    parentIsFirstOnNewChain
        = _blockHeight p > 1 && _blockHeight p == genesisHeight ver cid + 1
{-# INLINE epochStart #-}

-- -----------------------------------------------------------------------------
-- Feature Flags

newtype FeatureFlags = FeatureFlags Word64
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype (ToJSON, FromJSON)

encodeFeatureFlags :: FeatureFlags -> Put
encodeFeatureFlags (FeatureFlags ff) = putWord64le ff

decodeFeatureFlags :: Get FeatureFlags
decodeFeatureFlags = FeatureFlags <$> getWord64le

instance MerkleHashAlgorithm a => IsMerkleLogEntry a ChainwebHashTag FeatureFlags where
    type Tag FeatureFlags = 'FeatureFlagsTag
    toMerkleNode = encodeMerkleInputNode encodeFeatureFlags
    fromMerkleNode = decodeMerkleInputNode decodeFeatureFlags
    {-# INLINE toMerkleNode #-}
    {-# INLINE fromMerkleNode #-}

mkFeatureFlags :: FeatureFlags
mkFeatureFlags = FeatureFlags 0x0

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

instance HasChainId ParentHeader where
    _chainId = _chainId . _parentHeader
    {-# INLINE _chainId #-}

instance HasChainwebVersion ParentHeader where
    _chainwebVersion = _chainwebVersion . _parentHeader
    {-# INLINE _chainwebVersion #-}

instance HasChainGraph ParentHeader where
    _chainGraph = _chainGraph . _parentHeader
    {-# INLINE _chainGraph #-}

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
        { _blockFlags :: {-# UNPACK #-} !FeatureFlags
            -- ^ An 8-byte bitmask reserved for the future addition of boolean
            -- "feature flags".

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

        , _blockChainwebVersion :: !ChainwebVersion
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

isGenesisBlockHeader :: BlockHeader -> Bool
isGenesisBlockHeader b =
    _blockHeight b == genesisHeight (_blockChainwebVersion b) (_blockChainId b)
{-# INLINE isGenesisBlockHeader #-}

instance Eq BlockHeader where
     (==) = (==) `on` _blockHash
     {-# INLINE (==) #-}

instance Ord BlockHeader where
     compare = compare `on` _blockHash
     {-# INLINE compare #-}

instance Hashable BlockHeader where
    hashWithSalt s = hashWithSalt s . _blockHash
    {-# INLINE hashWithSalt #-}

instance HasChainId BlockHeader where
    _chainId = _blockChainId
    {-# INLINE _chainId #-}

instance HasChainGraph BlockHeader where
    _chainGraph h = _chainGraph (_blockChainwebVersion h, _blockHeight h)
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion BlockHeader where
    _chainwebVersion = _blockChainwebVersion
    {-# INLINE _chainwebVersion #-}

instance IsCasValue BlockHeader where
    type CasKeyType BlockHeader = BlockHash
    casKey = _blockHash
    {-# INLINE casKey #-}

type BlockHeaderCas cas = (CasConstraint cas BlockHeader)

makeLenses ''BlockHeader

instance HasMerkleLog ChainwebMerkleHashAlgorithm ChainwebHashTag BlockHeader where

    -- /IMPORTANT/ a types must occur at most once in this list
    type MerkleLogHeader BlockHeader =
        '[ FeatureFlags
        , BlockCreationTime
        , BlockHash
        , HashTarget
        , BlockPayloadHash
        , ChainId
        , BlockWeight
        , BlockHeight
        , ChainwebVersion
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
            , _blockChainwebVersion = cwv
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
            :+: cwv
            :+: es
            :+: nonce
            :+: MerkleLogBody adjParents
            ) = _merkleLogEntries l

        adjGraph
            | height == genesisHeight cwv cid = chainGraphAt cwv height
            | otherwise = chainGraphAt cwv (height - 1)

encodeBlockHeaderWithoutHash :: BlockHeader -> Put
encodeBlockHeaderWithoutHash b = do
    encodeFeatureFlags (_blockFlags b)
    encodeBlockCreationTime (_blockCreationTime b)
    encodeBlockHash (_blockParent b)
    encodeBlockHashRecord (_blockAdjacentHashes b)
    encodeHashTarget (_blockTarget b)
    encodeBlockPayloadHash (_blockPayloadHash b)
    encodeChainId (_blockChainId b)
    encodeBlockWeight (_blockWeight b)
    encodeBlockHeight (_blockHeight b)
    encodeChainwebVersion (_blockChainwebVersion b)
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
    a0 <- decodeFeatureFlags
    a1 <- decodeBlockCreationTime
    a2 <- decodeBlockHash -- parent hash
    a3 <- decodeBlockHashRecord
    a4 <- decodeHashTarget
    a5 <- decodeBlockPayloadHash
    a6 <- decodeChainId
    a7 <- decodeBlockWeight
    a8 <- decodeBlockHeight
    a9 <- decodeChainwebVersion
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
    <$> decodeFeatureFlags
    <*> decodeBlockCreationTime
    <*> decodeBlockHash -- parent hash
    <*> decodeBlockHashRecord
    <*> decodeHashTarget
    <*> decodeBlockPayloadHash
    <*> decodeChainId
    <*> decodeBlockWeight
    <*> decodeBlockHeight
    <*> decodeChainwebVersion
    <*> decodeEpochStartTime
    <*> decodeNonce
    <*> decodeBlockHash

instance ToJSON BlockHeader where
    toJSON = toJSON .  encodeB64UrlNoPaddingText . runPutS . encodeBlockHeader
    toEncoding = toEncoding .  encodeB64UrlNoPaddingText . runPutS . encodeBlockHeader
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
_blockPow h = powHash (_blockChainwebVersion h)
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

blockHeaderProperties
    :: KeyValue kv
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
    , "chainwebVersion" .= _blockChainwebVersion b
    , "epochStart" .= _blockEpochStart b
    , "featureFlags" .= _blockFlags b
    , "hash" .= _blockHash b
    ]
{-# INLINE blockHeaderProperties #-}

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
    <*> o .: "chainwebVersion"
    <*> o .: "epochStart"
    <*> o .: "nonce"
    <*> o .: "hash"

instance FromJSON (ObjectEncoded BlockHeader) where
    parseJSON = withObject "BlockHeader"
        $ fmap ObjectEncoded . parseBlockHeaderObject
    {-# INLINE parseJSON #-}

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
-- * the chain id, and
-- * the epoch start time
--
-- are valid with respect to the given parent header and adjacent parent
-- headers.
--
-- TODO: also check adjacent chains. This would probably break a lot of tests,
-- but might be worth it!
--
newBlockHeader
    :: HM.HashMap ChainId ParentHeader
        -- ^ Adjacent parent hashes.
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
        $ mkFeatureFlags
        :+: t
        :+: _blockHash b
        :+: target
        :+: pay
        :+: cid
        :+: _blockWeight b + BlockWeight (targetToDifficulty target)
        :+: _blockHeight b + 1
        :+: v
        :+: epochStart p adj t
        :+: nonce
        :+: MerkleLogBody (blockHashRecordToVector adjHashes)
  where
    cid = _chainId p
    v = _chainwebVersion p
    target = powTarget p adj t
    adjHashes = BlockHashRecord $ (_blockHash . _parentHeader) <$> adj

-- -------------------------------------------------------------------------- --
-- TreeDBEntry instance

instance TreeDbEntry BlockHeader where
    type Key BlockHeader = BlockHash
    key = _blockHash
    rank = int . _blockHeight
    parent e
        | isGenesisBlockHeader e = Nothing
        | otherwise = Just (_blockParent e)
